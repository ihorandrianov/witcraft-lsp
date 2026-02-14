use crate::document::{DocumentStore, SharedDocumentStore};
use crate::workspace::{
    AmbiguousType, CrossFileResolver, ResolveResult, SharedWorkspaceManager, UndefinedType,
    WorkspaceManager,
};
use dashmap::DashMap;
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::SystemTime;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};
use tracing::{debug, info};
use witcraft_syntax::{
    DefinitionKind, GlobalDefinition, LineIndex, NodeRef, PackageId, SourceFile, SymbolIndex,
    SyntaxKind, TextRange, node_at,
};

const SEMANTIC_TOKEN_TYPES: &[SemanticTokenType] = &[
    SemanticTokenType::KEYWORD,
    SemanticTokenType::TYPE,
    SemanticTokenType::FUNCTION,
    SemanticTokenType::PROPERTY,
    SemanticTokenType::VARIABLE,
    SemanticTokenType::STRING,
    SemanticTokenType::NUMBER,
    SemanticTokenType::OPERATOR,
    SemanticTokenType::COMMENT,
    SemanticTokenType::NAMESPACE,
];

pub struct WitLanguageServer {
    client: Client,
    documents: SharedDocumentStore,
    workspace: SharedWorkspaceManager,
    unopened_cache: DashMap<PathBuf, UnopenedFileCacheEntry>,
}

enum LocalGoto {
    Found(Location),
    NeedCrossFile(String),
}

struct SymbolTarget {
    name: String,
    resolved_definition: Option<GlobalDefinition>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct FileStamp {
    modified: SystemTime,
    len: u64,
}

#[derive(Debug, Clone)]
struct UnopenedFileCacheEntry {
    stamp: FileStamp,
    root: Arc<SourceFile>,
    index: Arc<SymbolIndex>,
    line_index: Arc<LineIndex>,
    package_id: Option<PackageId>,
}

fn file_change_label(change: FileChangeType) -> &'static str {
    match change {
        FileChangeType::CREATED => "created",
        FileChangeType::CHANGED => "changed",
        FileChangeType::DELETED => "deleted",
        _ => "unknown",
    }
}

impl WitLanguageServer {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents: Arc::new(DocumentStore::new()),
            workspace: Arc::new(WorkspaceManager::new()),
            unopened_cache: DashMap::new(),
        }
    }

    fn uri_to_file_path(uri: &str) -> Option<std::path::PathBuf> {
        Url::parse(uri).ok()?.to_file_path().ok()
    }

    fn reindex_file_from_disk(&self, uri: &str) {
        let Some(entry) = self.cached_unopened_entry(uri) else {
            self.workspace.remove_file(uri);
            return;
        };

        debug!(uri = %uri, source = "disk", "reindexed file from disk");
        self.workspace
            .update_file_definitions(uri, entry.index.as_ref(), entry.package_id);
    }

    fn index_and_line_index_for_uri(
        &self,
        uri: &str,
    ) -> Option<(Arc<SymbolIndex>, Arc<LineIndex>)> {
        if let Some(from_open_doc) = self.documents.with_document_mut(uri, |doc| {
            let parse = doc.parse().clone();
            let index = SymbolIndex::build(&parse.root);
            (Arc::new(index), Arc::new(doc.line_index.clone()))
        }) {
            debug!(uri = %uri, source = "open_doc", "index source");
            return Some(from_open_doc);
        }

        let entry = self.cached_unopened_entry(uri)?;
        debug!(uri = %uri, source = "disk", "index source");
        Some((entry.index, entry.line_index))
    }

    fn range_to_lsp_in_uri(&self, uri: &str, range: witcraft_syntax::TextRange) -> Option<Range> {
        if let Some(doc_range) = self
            .documents
            .with_document(uri, |doc| doc.range_to_lsp(range))
        {
            return Some(doc_range);
        }

        let entry = self.cached_unopened_entry(uri)?;
        Some(range_to_lsp_with_index(entry.line_index.as_ref(), range))
    }

    fn interface_name_range_in_uri(&self, uri: &str, interface_name: &str) -> Option<Range> {
        if let Some(doc_range) = self.documents.with_document_mut(uri, |doc| {
            let parse = doc.parse();
            find_interface_name_range(&parse.root, interface_name)
                .map(|range| doc.range_to_lsp(range))
        }) {
            return doc_range;
        }

        let entry = self.cached_unopened_entry(uri)?;
        let range = find_interface_name_range(entry.root.as_ref(), interface_name)?;
        Some(range_to_lsp_with_index(entry.line_index.as_ref(), range))
    }

    fn try_url(uri: &str) -> Option<Url> {
        Url::parse(uri).ok()
    }

    fn file_stamp(path: &Path) -> Option<FileStamp> {
        let metadata = std::fs::metadata(path).ok()?;
        let modified = metadata.modified().ok()?;
        Some(FileStamp {
            modified,
            len: metadata.len(),
        })
    }

    fn load_unopened_entry(path: &Path, stamp: FileStamp) -> Option<UnopenedFileCacheEntry> {
        let content = std::fs::read_to_string(path).ok()?;
        let parse_result = witcraft_syntax::parse(&content);
        let index = SymbolIndex::build(&parse_result.root);
        let package_id = parse_result
            .root
            .package
            .as_ref()
            .map(PackageId::from_package_decl);
        let line_index = LineIndex::new(&content);

        Some(UnopenedFileCacheEntry {
            stamp,
            root: Arc::new(parse_result.root),
            index: Arc::new(index),
            line_index: Arc::new(line_index),
            package_id,
        })
    }

    fn cached_unopened_entry(&self, uri: &str) -> Option<UnopenedFileCacheEntry> {
        let path = Self::uri_to_file_path(uri)?;
        let Some(stamp) = Self::file_stamp(&path) else {
            self.unopened_cache.remove(&path);
            return None;
        };

        if let Some(entry) = self.unopened_cache.get(&path) {
            if entry.stamp == stamp {
                debug!(uri = %uri, source = "cache", "unopened cache hit");
                return Some(entry.clone());
            }
        }

        let Some(entry) = Self::load_unopened_entry(&path, stamp) else {
            self.unopened_cache.remove(&path);
            return None;
        };
        self.unopened_cache.insert(path, entry.clone());
        debug!(uri = %uri, source = "disk", "unopened cache refresh");
        Some(entry)
    }

    async fn on_change(&self, uri: &str) {
        info!("on_change called for {}", uri);

        let diagnostics = self.documents.with_document_mut(uri, |doc| {
            info!(
                "Inside with_document_mut closure, doc version: {}",
                doc.version
            );

            // First pass: collect all diagnostic data (ranges and messages)
            let (parse_errors, undefined_types, ambiguous_types, duplicates, unused_imports) = {
                info!(
                    "About to call parse(), content length: {}",
                    doc.content.len()
                );
                let parse = doc.parse();
                info!("Parse completed");
                let index = SymbolIndex::build(&parse.root);

                info!("Parse errors: {}", parse.errors.len());
                info!("References: {}", index.references().len());
                info!("Definitions: {}", index.definitions().len());

                let package_id = parse
                    .root
                    .package
                    .as_ref()
                    .map(PackageId::from_package_decl);
                self.workspace
                    .update_file_definitions(uri, &index, package_id.clone());

                let errors: Vec<_> = parse
                    .errors
                    .iter()
                    .map(|err| {
                        info!("Parse error: {} at {:?}", err.message, err.range);
                        (err.range, err.message.clone())
                    })
                    .collect();

                let resolver = CrossFileResolver::new(&self.workspace);
                let undefined: Vec<UndefinedType> = resolver.find_undefined_types(uri, &index);
                let ambiguous: Vec<AmbiguousType> = resolver.find_ambiguous_types(uri, &index);

                let duplicates: Vec<_> = index
                    .find_duplicate_definitions()
                    .into_iter()
                    .map(|d| (d.duplicate_range, d.first_range, d.name))
                    .collect();

                let unused: Vec<_> = index
                    .find_unused_imports()
                    .into_iter()
                    .map(|u| (u.range, u.name))
                    .collect();

                (errors, undefined, ambiguous, duplicates, unused)
            };

            // Second pass: convert to LSP diagnostics (now parse/index are dropped)
            let mut diagnostics = Vec::new();

            for (range, message) in parse_errors {
                diagnostics.push(Diagnostic {
                    range: doc.range_to_lsp(range),
                    severity: Some(DiagnosticSeverity::ERROR),
                    message,
                    ..Default::default()
                });
            }

            for undef in undefined_types {
                diagnostics.push(Diagnostic {
                    range: doc.range_to_lsp(undef.range),
                    severity: Some(DiagnosticSeverity::ERROR),
                    message: format_undefined_type_message(&undef),
                    ..Default::default()
                });
            }

            for amb in ambiguous_types {
                diagnostics.push(Diagnostic {
                    range: doc.range_to_lsp(amb.range),
                    severity: Some(DiagnosticSeverity::ERROR),
                    message: format_ambiguous_type_message(&amb),
                    ..Default::default()
                });
            }

            for (range, first_range, name) in duplicates {
                let first_pos = doc.line_index.position(first_range.start());
                diagnostics.push(Diagnostic {
                    range: doc.range_to_lsp(range),
                    severity: Some(DiagnosticSeverity::ERROR),
                    message: format!(
                        "duplicate definition `{}` (first defined at line {})",
                        name,
                        first_pos.line + 1
                    ),
                    related_information: Self::try_url(uri).map(|url| {
                        vec![DiagnosticRelatedInformation {
                            location: Location::new(url, doc.range_to_lsp(first_range)),
                            message: format!("`{}` first defined here", name),
                        }]
                    }),
                    ..Default::default()
                });
            }

            for (range, name) in unused_imports {
                diagnostics.push(Diagnostic {
                    range: doc.range_to_lsp(range),
                    severity: Some(DiagnosticSeverity::WARNING),
                    message: format!("unused import `{}`", name),
                    tags: Some(vec![DiagnosticTag::UNNECESSARY]),
                    ..Default::default()
                });
            }

            info!("Total diagnostics: {}", diagnostics.len());
            diagnostics
        });

        if let Some(diags) = diagnostics {
            info!("Publishing {} diagnostics for {}", diags.len(), uri);
            for d in &diags {
                info!("  - {:?}: {}", d.range, d.message);
            }
            if let Some(url) = Self::try_url(uri) {
                self.client.publish_diagnostics(url, diags, None).await;
                info!("Diagnostics published");
            } else {
                info!("Skipping diagnostics publish, invalid URI: {}", uri);
            }
        } else {
            info!("No document found for {}", uri);
        }
    }

    /// Index all .wit files in the same directory as the given file.
    fn index_sibling_files(&self, uri: &str) {
        let Some(path) = Self::uri_to_file_path(uri) else {
            return;
        };
        let Some(dir) = path.parent() else {
            return;
        };

        info!("Scanning directory {:?} for sibling .wit files", dir);

        let Ok(entries) = std::fs::read_dir(dir) else {
            return;
        };

        for entry in entries.flatten() {
            let entry_path = entry.path();
            if entry_path.extension().is_some_and(|ext| ext == "wit") {
                let Ok(sibling_url) = Url::from_file_path(&entry_path) else {
                    continue;
                };
                let sibling_uri = sibling_url.to_string();

                // Skip if already in documents (will be indexed via on_change)
                if self.documents.contains(&sibling_uri) {
                    continue;
                }

                info!("Indexing sibling file: {}", sibling_uri);
                self.reindex_file_from_disk(&sibling_uri);
            }
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for WitLanguageServer {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        info!("WIT LSP initializing");

        // Initialize workspace with root folders
        let folders: Vec<PathBuf> = params
            .workspace_folders
            .unwrap_or_default()
            .into_iter()
            .filter_map(|f| f.uri.to_file_path().ok())
            .collect();

        if !folders.is_empty() {
            info!("Initializing workspace with {} folders", folders.len());
            self.workspace.initialize(folders);
        } else if let Some(root_uri) = params.root_uri {
            if let Ok(path) = root_uri.to_file_path() {
                info!("Initializing workspace with root URI: {:?}", path);
                self.workspace.initialize(vec![path]);
            }
        }

        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions {
                        open_close: Some(true),
                        change: Some(TextDocumentSyncKind::FULL),
                        ..Default::default()
                    },
                )),
                definition_provider: Some(OneOf::Left(true)),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                references_provider: Some(OneOf::Left(true)),
                document_highlight_provider: Some(OneOf::Left(true)),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            legend: SemanticTokensLegend {
                                token_types: SEMANTIC_TOKEN_TYPES.to_vec(),
                                token_modifiers: vec![],
                            },
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                            ..Default::default()
                        },
                    ),
                ),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![":".to_string(), ".".to_string()]),
                    ..Default::default()
                }),
                signature_help_provider: Some(SignatureHelpOptions {
                    trigger_characters: Some(vec!["(".to_string(), ",".to_string()]),
                    retrigger_characters: Some(vec![",".to_string()]),
                    ..Default::default()
                }),
                document_symbol_provider: Some(OneOf::Left(true)),
                code_action_provider: Some(CodeActionProviderCapability::Options(
                    CodeActionOptions {
                        code_action_kinds: Some(vec![CodeActionKind::QUICKFIX]),
                        ..Default::default()
                    },
                )),
                rename_provider: Some(OneOf::Right(RenameOptions {
                    prepare_provider: Some(true),
                    work_done_progress_options: Default::default(),
                })),
                workspace_symbol_provider: Some(OneOf::Left(true)),
                document_formatting_provider: Some(OneOf::Left(true)),
                folding_range_provider: Some(FoldingRangeProviderCapability::Simple(true)),
                inlay_hint_provider: Some(OneOf::Left(true)),
                selection_range_provider: Some(SelectionRangeProviderCapability::Simple(true)),
                code_lens_provider: Some(CodeLensOptions {
                    resolve_provider: Some(false),
                }),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "wit-lsp".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        info!("WIT LSP initialized");
        self.client
            .log_message(MessageType::INFO, "WIT LSP server initialized")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        info!("WIT LSP shutting down");
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        if let Some(path) = Self::uri_to_file_path(&uri) {
            self.unopened_cache.remove(&path);
        }
        self.documents.open(
            uri.clone(),
            params.text_document.version,
            params.text_document.text,
        );

        // Index sibling .wit files for cross-file resolution
        self.index_sibling_files(&uri);

        self.on_change(&uri).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        info!(
            "did_change for {}, version {}",
            uri, params.text_document.version
        );
        if let Some(change) = params.content_changes.into_iter().next() {
            info!("Updating document, content length: {}", change.text.len());
            let updated = self
                .documents
                .update(&uri, params.text_document.version, change.text);
            info!("Document update result: {}", updated);
            self.on_change(&uri).await;
        } else {
            info!("No content changes received");
        }
    }

    async fn did_change_watched_files(&self, params: DidChangeWatchedFilesParams) {
        for change in params.changes {
            let uri = change.uri.to_string();
            debug!(uri = %uri, change = file_change_label(change.typ), "watched file change");
            match change.typ {
                FileChangeType::DELETED => {
                    if let Some(path) = Self::uri_to_file_path(&uri) {
                        self.unopened_cache.remove(&path);
                    }
                    self.workspace.remove_file(&uri)
                }
                FileChangeType::CREATED | FileChangeType::CHANGED => {
                    if !self.documents.contains(&uri) {
                        self.reindex_file_from_disk(&uri);
                    } else if let Some(path) = Self::uri_to_file_path(&uri) {
                        self.unopened_cache.remove(&path);
                    }
                }
                _ => {}
            }
        }
        self.workspace.rescan_roots();
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        self.documents.close(&uri);
        self.reindex_file_from_disk(&uri);
        self.client
            .publish_diagnostics(params.text_document.uri, vec![], None)
            .await;
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params
            .text_document_position_params
            .text_document
            .uri
            .to_string();
        let position = params.text_document_position_params.position;

        // First try local resolution
        let local_result = self.documents.with_document_mut(&uri, |doc| {
            let offset = doc.position_to_offset(witcraft_syntax::Position::new(
                position.line,
                position.character,
            ))?;

            let parse = doc.parse();
            let index = SymbolIndex::build(&parse.root);
            let node = node_at(&parse.root, offset)?;

            let name = match node {
                NodeRef::NamedType(named) => Some(named.name.name.to_string()),
                NodeRef::Ident(ident) => {
                    if let Some(reference) = index.reference_at(offset) {
                        Some(reference.name.to_string())
                    } else {
                        Some(ident.name.to_string())
                    }
                }
                NodeRef::UsePath(path) => Some(path.name.name.to_string()),
                _ => None,
            }?;

            // Try local definition first
            if let Some(def) = index.find_definition(&name) {
                return Some(LocalGoto::Found(Location::new(
                    params
                        .text_document_position_params
                        .text_document
                        .uri
                        .clone(),
                    doc.range_to_lsp(def.name_range),
                )));
            }

            Some(LocalGoto::NeedCrossFile(name))
        });

        // If found locally, return it
        if let Some(Some(LocalGoto::Found(location))) = local_result {
            return Ok(Some(GotoDefinitionResponse::Scalar(location)));
        }

        let search_name = if let Some(Some(LocalGoto::NeedCrossFile(name))) = local_result {
            name
        } else {
            return Ok(None);
        };

        // Try cross-file resolution
        let cross_file_result = self.documents.with_document_mut(&uri, |doc| {
            let parse = doc.parse();
            let index = SymbolIndex::build(&parse.root);

            // Use cross-file resolver
            let resolver = CrossFileResolver::new(&self.workspace);
            match resolver.resolve_type(&uri, &search_name, &index) {
                crate::workspace::ResolveResult::Found(global_def) => {
                    // We found it in another file - need to get that file's line index
                    // For now, just return the raw range (will need adjustment)
                    let target_uri = Url::parse(&global_def.uri).ok()?;

                    // Try to get the target document's line index for proper range conversion
                    if let Some(target_range) =
                        self.range_to_lsp_in_uri(&global_def.uri, global_def.name_range)
                    {
                        Some(Location::new(target_uri, target_range))
                    } else {
                        None
                    }
                }
                crate::workspace::ResolveResult::Ambiguous(_) => None,
                _ => None,
            }
        });

        match cross_file_result {
            Some(Some(location)) => Ok(Some(GotoDefinitionResponse::Scalar(location))),
            _ => Ok(None),
        }
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params
            .text_document_position_params
            .text_document
            .uri
            .to_string();
        let position = params.text_document_position_params.position;

        let result = self.documents.with_document_mut(&uri, |doc| {
            let offset = doc.position_to_offset(witcraft_syntax::Position::new(
                position.line,
                position.character,
            ))?;

            let parse = doc.parse();
            let index = SymbolIndex::build(&parse.root);
            let node = node_at(&parse.root, offset)?;

            // Helper to resolve a name, trying local first then cross-file
            let resolve_name = |name: &str| -> Option<String> {
                // Try local definition first
                if let Some(def) = index.find_definition(name) {
                    return Some(format_definition(def));
                }

                // Try cross-file resolution
                let resolver = CrossFileResolver::new(&self.workspace);
                match resolver.resolve_type(&uri, name, &index) {
                    ResolveResult::Found(global_def) => Some(format_global_definition(&global_def)),
                    ResolveResult::Imported(import) => Some(format!(
                        "```wit\n{}\n```\n\n*Imported from `{}`*",
                        import.original_name, import.from_interface
                    )),
                    ResolveResult::Builtin => {
                        Some(format!("```wit\n{}\n```\n\n*Builtin type*", name))
                    }
                    ResolveResult::Ambiguous(candidates) => {
                        Some(format_ambiguous_hover(name, candidates))
                    }
                    ResolveResult::NotFound => None,
                }
            };

            let (content, range) = match node {
                NodeRef::NamedType(named) => {
                    let content = resolve_name(&named.name.name)?;
                    (content, named.range)
                }
                NodeRef::Ident(ident) => {
                    let name = if let Some(reference) = index.reference_at(offset) {
                        reference.name.to_string()
                    } else {
                        ident.name.to_string()
                    };
                    let content = resolve_name(&name)?;
                    let range = index
                        .reference_at(offset)
                        .map(|r| r.range)
                        .unwrap_or(ident.range);
                    (content, range)
                }
                NodeRef::Interface(iface) => {
                    let def = index.find_definition(&iface.name.name)?;
                    (format_definition(def), iface.name.range)
                }
                NodeRef::World(world) => {
                    let def = index.find_definition(&world.name.name)?;
                    (format_definition(def), world.name.range)
                }
                NodeRef::Func(func) => {
                    let def = index.find_definition(&func.name.name)?;
                    (format_definition(def), func.name.range)
                }
                NodeRef::Record(rec) => {
                    let def = index.find_definition(&rec.name.name)?;
                    (format_definition(def), rec.name.range)
                }
                NodeRef::Variant(var) => {
                    let def = index.find_definition(&var.name.name)?;
                    (format_definition(def), var.name.range)
                }
                NodeRef::Enum(e) => {
                    let def = index.find_definition(&e.name.name)?;
                    (format_definition(def), e.name.range)
                }
                NodeRef::Flags(f) => {
                    let def = index.find_definition(&f.name.name)?;
                    (format_definition(def), f.name.range)
                }
                NodeRef::Resource(res) => {
                    let def = index.find_definition(&res.name.name)?;
                    (format_definition(def), res.name.range)
                }
                NodeRef::TypeAlias(alias) => {
                    let def = index.find_definition(&alias.name.name)?;
                    (format_definition(def), alias.name.range)
                }
                _ => return None,
            };

            Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: content,
                }),
                range: Some(doc.range_to_lsp(range)),
            })
        });

        Ok(result.flatten())
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let uri = params.text_document_position.text_document.uri.to_string();
        let position = params.text_document_position.position;
        let include_declaration = params.context.include_declaration;

        // First resolve the symbol target from the current position.
        let target = self
            .documents
            .with_document_mut(&uri, |doc| {
                let offset = doc.position_to_offset(witcraft_syntax::Position::new(
                    position.line,
                    position.character,
                ))?;

                let parse = doc.parse();
                let index = SymbolIndex::build(&parse.root);
                let node = node_at(&parse.root, offset)?;

                if let Some(def) = index
                    .definitions()
                    .iter()
                    .find(|def| def.name_range.contains(offset))
                {
                    return Some(SymbolTarget {
                        name: def.name.to_string(),
                        resolved_definition: Some(GlobalDefinition::from_definition(def, &uri)),
                    });
                }

                let name = match node {
                    NodeRef::NamedType(named) => named.name.name.to_string(),
                    NodeRef::Ident(ident) => ident.name.to_string(),
                    NodeRef::Interface(iface) => iface.name.name.to_string(),
                    NodeRef::World(world) => world.name.name.to_string(),
                    NodeRef::Func(func) => func.name.name.to_string(),
                    NodeRef::Record(rec) => rec.name.name.to_string(),
                    NodeRef::Variant(var) => var.name.name.to_string(),
                    NodeRef::Enum(e) => e.name.name.to_string(),
                    NodeRef::Flags(f) => f.name.name.to_string(),
                    NodeRef::Resource(res) => res.name.name.to_string(),
                    NodeRef::TypeAlias(alias) => alias.name.name.to_string(),
                    _ => return None,
                };

                let resolved_definition = match CrossFileResolver::new(&self.workspace)
                    .resolve_type(&uri, &name, &index)
                {
                    ResolveResult::Found(def) => Some(def),
                    _ => None,
                };

                Some(SymbolTarget {
                    name,
                    resolved_definition,
                })
            })
            .flatten();

        let Some(target) = target else {
            return Ok(None);
        };
        let name = &target.name;
        let resolver = CrossFileResolver::new(&self.workspace);

        // Get all files in the same package
        let package_files = self.workspace.files_in_same_package(&uri);
        let files_to_search: Vec<String> = if package_files.is_empty() {
            vec![uri.clone()]
        } else {
            package_files
        };

        let mut locations = Vec::new();

        // Search in each file
        for file_uri in &files_to_search {
            let Some((index, line_index)) = self.index_and_line_index_for_uri(file_uri) else {
                continue;
            };

            let index = index.as_ref();
            let line_index = line_index.as_ref();
            let Ok(url) = Url::parse(file_uri) else {
                continue;
            };

            let mut file_locs = Vec::new();

            // Include declaration if requested
            if include_declaration {
                if let Some(target_def) = &target.resolved_definition {
                    for def in index.definitions() {
                        let global = GlobalDefinition::from_definition(def, file_uri.as_str());
                        if same_global_definition(&global, target_def) {
                            file_locs.push(Location::new(
                                url.clone(),
                                range_to_lsp_with_index(line_index, def.name_range),
                            ));
                        }
                    }
                } else if let Some(def) = index.find_definition(name) {
                    file_locs.push(Location::new(
                        url.clone(),
                        range_to_lsp_with_index(line_index, def.name_range),
                    ));
                }
            }

            // Find all references
            let include_refs_in_file = if let Some(target_def) = &target.resolved_definition {
                match resolver.resolve_type(file_uri, name, index) {
                    ResolveResult::Found(found) => same_global_definition(&found, target_def),
                    _ => false,
                }
            } else {
                true
            };

            if include_refs_in_file {
                for reference in index.references() {
                    if &*reference.name == name {
                        file_locs.push(Location::new(
                            url.clone(),
                            range_to_lsp_with_index(line_index, reference.range),
                        ));
                    }
                }
            }

            if !file_locs.is_empty() {
                locations.extend(file_locs);
            }
        }

        if locations.is_empty() {
            Ok(None)
        } else {
            Ok(Some(locations))
        }
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri.to_string();

        let result = self.documents.with_document_mut(&uri, |doc| {
            let parse = doc.parse();

            let token_data: Vec<_> = parse
                .tokens
                .iter()
                .filter_map(|token| {
                    let token_type = if token.kind.is_keyword() {
                        Some(0u32) // KEYWORD
                    } else if token.kind.is_builtin_type() {
                        Some(1) // TYPE
                    } else {
                        match token.kind {
                            SyntaxKind::LineComment
                            | SyntaxKind::BlockComment
                            | SyntaxKind::DocComment => Some(8), // COMMENT
                            SyntaxKind::Colon | SyntaxKind::Arrow | SyntaxKind::At => Some(7), // OPERATOR
                            _ => None,
                        }
                    };

                    token_type.map(|t| (token.range.start(), token.range.len(), t))
                })
                .collect();

            let mut tokens = Vec::new();
            let mut prev_line = 0u32;
            let mut prev_start = 0u32;

            for (start, length, token_type) in token_data {
                let pos = doc.offset_to_position(start);

                let delta_line = pos.line - prev_line;
                let delta_start = if delta_line == 0 {
                    pos.column - prev_start
                } else {
                    pos.column
                };

                tokens.push(SemanticToken {
                    delta_line,
                    delta_start,
                    length,
                    token_type,
                    token_modifiers_bitset: 0,
                });

                prev_line = pos.line;
                prev_start = pos.column;
            }

            SemanticTokens {
                result_id: None,
                data: tokens,
            }
        });

        Ok(result.map(SemanticTokensResult::Tokens))
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri.to_string();
        let position = params.text_document_position.position;

        let result = self.documents.with_document_mut(&uri, |doc| {
            let offset = doc.position_to_offset(witcraft_syntax::Position::new(
                position.line,
                position.character,
            ))?;

            let parse = doc.parse();
            let index = SymbolIndex::build(&parse.root);

            let mut items = Vec::new();

            // Add primitive types
            for primitive in PRIMITIVE_TYPES {
                items.push(CompletionItem {
                    label: primitive.to_string(),
                    kind: Some(CompletionItemKind::KEYWORD),
                    detail: Some("primitive type".to_string()),
                    ..Default::default()
                });
            }

            // Add generic types
            for generic in GENERIC_TYPES {
                items.push(CompletionItem {
                    label: format!("{}<>", generic),
                    insert_text: Some(format!("{}<$1>$0", generic)),
                    insert_text_format: Some(InsertTextFormat::SNIPPET),
                    kind: Some(CompletionItemKind::KEYWORD),
                    detail: Some("generic type".to_string()),
                    ..Default::default()
                });
            }

            // Add user-defined types from the index
            for def in index.definitions() {
                let kind = match def.kind {
                    DefinitionKind::Interface => CompletionItemKind::INTERFACE,
                    DefinitionKind::World => CompletionItemKind::MODULE,
                    DefinitionKind::TypeAlias
                    | DefinitionKind::Record
                    | DefinitionKind::Variant
                    | DefinitionKind::Enum
                    | DefinitionKind::Flags => CompletionItemKind::STRUCT,
                    DefinitionKind::Resource => CompletionItemKind::CLASS,
                    DefinitionKind::Function => CompletionItemKind::FUNCTION,
                };

                items.push(CompletionItem {
                    label: def.name.to_string(),
                    kind: Some(kind),
                    detail: def.parent.as_ref().map(|p| p.to_string()),
                    ..Default::default()
                });
            }

            // Add imported types (via `use` statements)
            for import in index.imports() {
                items.push(CompletionItem {
                    label: import.local_name.to_string(),
                    kind: Some(CompletionItemKind::STRUCT),
                    detail: Some(format!("from {}", import.from_interface)),
                    ..Default::default()
                });
            }

            // Add keywords based on context
            let context_keywords = get_context_keywords(doc.content.as_str(), offset);
            for kw in context_keywords {
                items.push(CompletionItem {
                    label: kw.to_string(),
                    kind: Some(CompletionItemKind::KEYWORD),
                    ..Default::default()
                });
            }

            Some(items)
        });

        match result {
            Some(Some(items)) => Ok(Some(CompletionResponse::Array(items))),
            _ => Ok(None),
        }
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = params.text_document.uri.to_string();

        let result = self.documents.with_document_mut(&uri, |doc| {
            // Clone the line_index so we can use it after parsing
            let line_index = doc.line_index.clone();
            let parse = doc.parse();

            let range_to_lsp = |range: witcraft_syntax::TextRange| -> Range {
                let start = line_index.position(range.start());
                let end = line_index.position(range.end());
                Range {
                    start: tower_lsp::lsp_types::Position {
                        line: start.line,
                        character: start.column,
                    },
                    end: tower_lsp::lsp_types::Position {
                        line: end.line,
                        character: end.column,
                    },
                }
            };

            let mut symbols = Vec::new();

            // Add package declaration if present
            if let Some(pkg) = &parse.root.package {
                let ns_str = pkg
                    .namespace
                    .iter()
                    .map(|i| &*i.name)
                    .collect::<Vec<_>>()
                    .join(":");
                let name = format!("{}:{}", ns_str, pkg.name.name);
                let selection_range = pkg
                    .namespace
                    .first()
                    .map(|i| i.range)
                    .unwrap_or(pkg.name.range);
                #[allow(deprecated)]
                symbols.push(DocumentSymbol {
                    name,
                    detail: pkg
                        .version
                        .as_ref()
                        .map(|v| format!("@{}.{}.{}", v.major, v.minor, v.patch)),
                    kind: SymbolKind::PACKAGE,
                    tags: None,
                    deprecated: None,
                    range: range_to_lsp(pkg.range),
                    selection_range: range_to_lsp(selection_range),
                    children: None,
                });
            }

            // Add top-level items
            for item in &parse.root.items {
                match item {
                    witcraft_syntax::ast::Item::Interface(iface) => {
                        let children = build_interface_symbols(iface, &range_to_lsp);
                        #[allow(deprecated)]
                        symbols.push(DocumentSymbol {
                            name: iface.name.name.to_string(),
                            detail: Some("interface".to_string()),
                            kind: SymbolKind::INTERFACE,
                            tags: None,
                            deprecated: None,
                            range: range_to_lsp(iface.range),
                            selection_range: range_to_lsp(iface.name.range),
                            children: if children.is_empty() {
                                None
                            } else {
                                Some(children)
                            },
                        });
                    }
                    witcraft_syntax::ast::Item::World(world) => {
                        let children = build_world_symbols(world, &range_to_lsp);
                        #[allow(deprecated)]
                        symbols.push(DocumentSymbol {
                            name: world.name.name.to_string(),
                            detail: Some("world".to_string()),
                            kind: SymbolKind::MODULE,
                            tags: None,
                            deprecated: None,
                            range: range_to_lsp(world.range),
                            selection_range: range_to_lsp(world.name.range),
                            children: if children.is_empty() {
                                None
                            } else {
                                Some(children)
                            },
                        });
                    }
                    witcraft_syntax::ast::Item::TypeDef(typedef) => {
                        if let Some(sym) = build_typedef_symbol(typedef, &range_to_lsp) {
                            symbols.push(sym);
                        }
                    }
                }
            }

            Some(symbols)
        });

        match result {
            Some(Some(symbols)) => Ok(Some(DocumentSymbolResponse::Nested(symbols))),
            _ => Ok(None),
        }
    }

    async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        let uri = params.text_document.uri.to_string();
        let _range = params.range;

        // Collect code actions based on diagnostics
        let mut actions = Vec::new();

        // Get the document and build index
        let context = self.documents.with_document_mut(&uri, |doc| {
            let line_index = doc.line_index.clone();
            let content = doc.content.clone();
            let parse = doc.parse();
            let root = parse.root.clone();
            let index = SymbolIndex::build(&parse.root);
            let use_path_ranges = collect_use_path_ranges(&parse.root);
            let resolver = CrossFileResolver::new(&self.workspace);
            let ambiguous_types = resolver.find_ambiguous_types(&uri, &index);

            // Helper to convert LSP Range to byte range
            let _lsp_to_offset = |pos: tower_lsp::lsp_types::Position| -> Option<u32> {
                line_index.offset(witcraft_syntax::Position::new(pos.line, pos.character))
            };

            (
                index,
                root,
                line_index.clone(),
                content,
                use_path_ranges,
                ambiguous_types,
            )
        });

        let Some((index, root, line_index, content, use_path_ranges, ambiguous_types)) = context
        else {
            return Ok(None);
        };

        // Process diagnostics to find actionable ones
        for diag in &params.context.diagnostics {
            // Handle "unused import" diagnostics
            if diag.message.starts_with("unused import") {
                if let Some(action) =
                    self.make_remove_import_action(&uri, diag, &index, &line_index, &content)
                {
                    actions.push(action);
                }
            }

            // Handle "undefined type" diagnostics
            if diag.message.starts_with("undefined type") {
                if let Some(action) =
                    self.make_add_import_action(&uri, diag, &index, &root, &line_index)
                {
                    actions.push(action);
                }
            }

            // Handle "ambiguous type" diagnostics
            if diag.message.starts_with("ambiguous type") {
                let ambiguity_actions = self.make_ambiguity_actions(
                    &uri,
                    diag,
                    &index,
                    &line_index,
                    &use_path_ranges,
                    &ambiguous_types,
                );
                actions.extend(ambiguity_actions);
            }
        }

        if actions.is_empty() {
            Ok(None)
        } else {
            Ok(Some(actions))
        }
    }

    async fn prepare_rename(
        &self,
        params: TextDocumentPositionParams,
    ) -> Result<Option<PrepareRenameResponse>> {
        let uri = params.text_document.uri.to_string();
        let position = params.position;

        let result = self.documents.with_document_mut(&uri, |doc| {
            let offset = doc.position_to_offset(witcraft_syntax::Position::new(
                position.line,
                position.character,
            ))?;

            let parse = doc.parse();
            let index = SymbolIndex::build(&parse.root);
            let node = node_at(&parse.root, offset)?;

            // Check if we're on a renameable symbol
            let (name, name_range) = match node {
                NodeRef::Ident(ident) => {
                    // Check if this is a definition or a reference
                    if index.find_definition(&ident.name).is_some() {
                        (ident.name.to_string(), ident.range)
                    } else if index.reference_at(offset).is_some() {
                        // It's a reference - find the definition
                        let reference = index.reference_at(offset)?;
                        (reference.name.to_string(), reference.range)
                    } else {
                        return None;
                    }
                }
                NodeRef::Interface(iface) => (iface.name.name.to_string(), iface.name.range),
                NodeRef::World(world) => (world.name.name.to_string(), world.name.range),
                NodeRef::Record(rec) => (rec.name.name.to_string(), rec.name.range),
                NodeRef::Variant(var) => (var.name.name.to_string(), var.name.range),
                NodeRef::Enum(e) => (e.name.name.to_string(), e.name.range),
                NodeRef::Flags(f) => (f.name.name.to_string(), f.name.range),
                NodeRef::Resource(res) => (res.name.name.to_string(), res.name.range),
                NodeRef::TypeAlias(alias) => (alias.name.name.to_string(), alias.name.range),
                NodeRef::Func(func) => (func.name.name.to_string(), func.name.range),
                NodeRef::NamedType(named) => (named.name.name.to_string(), named.name.range),
                _ => return None,
            };

            Some((name, doc.range_to_lsp(name_range)))
        });

        match result {
            Some(Some((name, range))) => Ok(Some(PrepareRenameResponse::RangeWithPlaceholder {
                range,
                placeholder: name,
            })),
            _ => Ok(None),
        }
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let uri = params.text_document_position.text_document.uri.to_string();
        let position = params.text_document_position.position;
        let new_name = params.new_name;

        // First, resolve the symbol target at the position.
        let target = self
            .documents
            .with_document_mut(&uri, |doc| {
                let offset = doc.position_to_offset(witcraft_syntax::Position::new(
                    position.line,
                    position.character,
                ))?;

                let parse = doc.parse();
                let index = SymbolIndex::build(&parse.root);
                let node = node_at(&parse.root, offset)?;

                if let Some(def) = index
                    .definitions()
                    .iter()
                    .find(|def| def.name_range.contains(offset))
                {
                    return Some(SymbolTarget {
                        name: def.name.to_string(),
                        resolved_definition: Some(GlobalDefinition::from_definition(def, &uri)),
                    });
                }

                let name = match node {
                    NodeRef::Ident(ident) => ident.name.to_string(),
                    NodeRef::Interface(iface) => iface.name.name.to_string(),
                    NodeRef::World(world) => world.name.name.to_string(),
                    NodeRef::Record(rec) => rec.name.name.to_string(),
                    NodeRef::Variant(var) => var.name.name.to_string(),
                    NodeRef::Enum(e) => e.name.name.to_string(),
                    NodeRef::Flags(f) => f.name.name.to_string(),
                    NodeRef::Resource(res) => res.name.name.to_string(),
                    NodeRef::TypeAlias(alias) => alias.name.name.to_string(),
                    NodeRef::Func(func) => func.name.name.to_string(),
                    NodeRef::NamedType(named) => named.name.name.to_string(),
                    _ => return None,
                };

                let resolved_definition = match CrossFileResolver::new(&self.workspace)
                    .resolve_type(&uri, &name, &index)
                {
                    ResolveResult::Found(def) => Some(def),
                    _ => None,
                };

                Some(SymbolTarget {
                    name,
                    resolved_definition,
                })
            })
            .flatten();

        let Some(target) = target else {
            return Ok(None);
        };
        let old_name = &target.name;
        let resolver = CrossFileResolver::new(&self.workspace);

        // Get all files in the same package
        let package_files = self.workspace.files_in_same_package(&uri);
        let files_to_search: Vec<String> = if package_files.is_empty() {
            vec![uri.clone()]
        } else {
            package_files
        };

        let mut changes: std::collections::HashMap<Url, Vec<TextEdit>> =
            std::collections::HashMap::new();

        // Search in each file for occurrences
        for file_uri in &files_to_search {
            let Some((index, line_index)) = self.index_and_line_index_for_uri(file_uri) else {
                continue;
            };

            let index = index.as_ref();
            let line_index = line_index.as_ref();

            let mut edits = Vec::new();

            // Find definition in this file
            if let Some(target_def) = &target.resolved_definition {
                for def in index.definitions() {
                    let global = GlobalDefinition::from_definition(def, file_uri.as_str());
                    if same_global_definition(&global, target_def) {
                        edits.push(TextEdit {
                            range: range_to_lsp_with_index(line_index, def.name_range),
                            new_text: new_name.clone(),
                        });
                    }
                }
            } else if let Some(def) = index.find_definition(old_name) {
                edits.push(TextEdit {
                    range: range_to_lsp_with_index(line_index, def.name_range),
                    new_text: new_name.clone(),
                });
            }

            // Find all references in this file
            let include_refs_in_file = if let Some(target_def) = &target.resolved_definition {
                match resolver.resolve_type(file_uri, old_name, index) {
                    ResolveResult::Found(found) => same_global_definition(&found, target_def),
                    _ => false,
                }
            } else {
                true
            };

            if include_refs_in_file {
                for reference in index.references() {
                    if &*reference.name == old_name {
                        edits.push(TextEdit {
                            range: range_to_lsp_with_index(line_index, reference.range),
                            new_text: new_name.clone(),
                        });
                    }
                }
            }

            // Find imports that reference this name
            for import in index.imports() {
                let import_matches_target = if let Some(target_def) = &target.resolved_definition {
                    match resolver.resolve_type(file_uri, &import.local_name, index) {
                        ResolveResult::Found(found) => same_global_definition(&found, target_def),
                        _ => false,
                    }
                } else {
                    true
                };
                if !import_matches_target {
                    continue;
                }

                if &*import.original_name == old_name {
                    // Rename the original name in the import statement
                    // For `use iface.{old-name as alias}`, we rename `old-name`
                    edits.push(TextEdit {
                        range: range_to_lsp_with_index(line_index, import.original_name_range),
                        new_text: new_name.clone(),
                    });

                    // If there's no alias, also update usages (they use the original name)
                    // If there IS an alias, usages use the alias so we don't touch them
                    if &*import.local_name == old_name {
                        // No alias - local_name equals original_name
                        // References using this name will be caught by the references loop above
                    }
                } else if &*import.local_name == old_name && &*import.original_name != old_name {
                    // The import has an alias that matches old_name
                    // e.g., `use iface.{something as old-name}`
                    // Rename the alias
                    edits.push(TextEdit {
                        range: range_to_lsp_with_index(line_index, import.range),
                        new_text: new_name.clone(),
                    });
                }
            }

            if !edits.is_empty() {
                if let Ok(url) = Url::parse(file_uri) {
                    changes.insert(url, edits);
                }
            }
        }

        if changes.is_empty() {
            Ok(None)
        } else {
            Ok(Some(WorkspaceEdit {
                changes: Some(changes),
                ..Default::default()
            }))
        }
    }

    async fn symbol(
        &self,
        params: WorkspaceSymbolParams,
    ) -> Result<Option<Vec<SymbolInformation>>> {
        let query = params.query.to_lowercase();

        let mut symbols = Vec::new();

        // Iterate all indexed definitions in the workspace, including unopened files.
        for def in self.workspace.all_workspace_definitions() {
            if !query.is_empty() && !def.name.to_lowercase().contains(&query) {
                continue;
            }

            let kind = match def.kind {
                DefinitionKind::Interface => SymbolKind::INTERFACE,
                DefinitionKind::World => SymbolKind::MODULE,
                DefinitionKind::TypeAlias => SymbolKind::TYPE_PARAMETER,
                DefinitionKind::Record => SymbolKind::STRUCT,
                DefinitionKind::Variant => SymbolKind::ENUM,
                DefinitionKind::Enum => SymbolKind::ENUM,
                DefinitionKind::Flags => SymbolKind::ENUM,
                DefinitionKind::Resource => SymbolKind::CLASS,
                DefinitionKind::Function => SymbolKind::FUNCTION,
            };

            let Some(url) = Self::try_url(&def.uri) else {
                continue;
            };
            let Some(range) = self.range_to_lsp_in_uri(&def.uri, def.name_range) else {
                continue;
            };

            #[allow(deprecated)]
            symbols.push(SymbolInformation {
                name: def.name.to_string(),
                kind,
                tags: None,
                deprecated: None,
                location: Location::new(url, range),
                container_name: def.parent.as_ref().map(|p| p.to_string()),
            });
        }

        // Deterministic ordering helps editor stability and test repeatability.
        symbols.sort_by(|a, b| {
            a.name
                .cmp(&b.name)
                .then_with(|| a.location.uri.as_str().cmp(b.location.uri.as_str()))
                .then_with(|| {
                    a.location
                        .range
                        .start
                        .line
                        .cmp(&b.location.range.start.line)
                })
                .then_with(|| {
                    a.location
                        .range
                        .start
                        .character
                        .cmp(&b.location.range.start.character)
                })
        });

        if symbols.is_empty() {
            Ok(None)
        } else {
            Ok(Some(symbols))
        }
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let uri = params.text_document.uri.to_string();

        let result = self.documents.with_document_mut(&uri, |doc| {
            let parse = doc.parse();

            // Format the parsed AST
            let formatted = witcraft_syntax::Formatter::format(&parse.root);

            // If the formatted output is the same as the original, no edits needed
            if formatted == doc.content {
                return None;
            }

            // Create a single edit that replaces the entire document
            let end_pos = doc.offset_to_position(doc.content.len() as u32);
            Some(vec![TextEdit {
                range: Range {
                    start: tower_lsp::lsp_types::Position {
                        line: 0,
                        character: 0,
                    },
                    end: tower_lsp::lsp_types::Position {
                        line: end_pos.line,
                        character: end_pos.column,
                    },
                },
                new_text: formatted,
            }])
        });

        Ok(result.flatten())
    }

    async fn signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        let uri = params
            .text_document_position_params
            .text_document
            .uri
            .to_string();
        let position = params.text_document_position_params.position;

        let result = self.documents.with_document_mut(&uri, |doc| {
            let offset = doc.position_to_offset(witcraft_syntax::Position::new(
                position.line,
                position.character,
            ))?;

            let content = &doc.content;

            // Find signature context by scanning backwards
            find_signature_context(content, offset as usize)
        });

        Ok(result.flatten())
    }

    async fn folding_range(&self, params: FoldingRangeParams) -> Result<Option<Vec<FoldingRange>>> {
        let uri = params.text_document.uri.to_string();

        let ranges = self.documents.with_document_mut(&uri, |doc| {
            let parse = doc.parse().clone();
            let mut folding_ranges = Vec::new();
            let line_index = &doc.line_index;

            // Helper to add a folding range
            let mut add_range = |range: witcraft_syntax::TextRange| {
                let start = line_index.position(range.start());
                let end = line_index.position(range.end());
                // Only fold if it spans multiple lines
                if end.line > start.line {
                    folding_ranges.push(FoldingRange {
                        start_line: start.line,
                        start_character: Some(start.column),
                        end_line: end.line,
                        end_character: Some(end.column),
                        kind: Some(FoldingRangeKind::Region),
                        collapsed_text: None,
                    });
                }
            };

            // Collect folding ranges from the AST
            for item in &parse.root.items {
                match item {
                    witcraft_syntax::Item::Interface(iface) => {
                        add_range(iface.range);
                        // Also fold nested types
                        for item in &iface.items {
                            if let witcraft_syntax::InterfaceItem::TypeDef(td) = item {
                                add_range(td.range());
                            }
                        }
                    }
                    witcraft_syntax::Item::World(world) => {
                        add_range(world.range);
                    }
                    witcraft_syntax::Item::TypeDef(td) => {
                        add_range(td.range());
                    }
                }
            }

            // Nested packages
            for nested in &parse.root.nested_packages {
                add_range(nested.range);
            }

            folding_ranges
        });

        match ranges {
            Some(r) if !r.is_empty() => Ok(Some(r)),
            _ => Ok(None),
        }
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        let uri = params.text_document.uri.to_string();

        let hints = self.documents.with_document_mut(&uri, |doc| {
            let parse = doc.parse().clone();
            collect_inlay_hints(&parse.root, &self.workspace, &uri, &doc.line_index)
        });

        match hints {
            Some(h) if !h.is_empty() => Ok(Some(h)),
            _ => Ok(None),
        }
    }

    async fn document_highlight(
        &self,
        params: DocumentHighlightParams,
    ) -> Result<Option<Vec<DocumentHighlight>>> {
        let uri = params
            .text_document_position_params
            .text_document
            .uri
            .to_string();
        let position = params.text_document_position_params.position;

        let highlights = self.documents.with_document_mut(&uri, |doc| {
            let offset = doc.position_to_offset(witcraft_syntax::Position::new(
                position.line,
                position.character,
            ))?;

            let parse = doc.parse();
            let index = SymbolIndex::build(&parse.root);
            let node = node_at(&parse.root, offset)?;

            let name = match node {
                NodeRef::NamedType(named) => named.name.name.to_string(),
                NodeRef::Ident(ident) => ident.name.to_string(),
                NodeRef::Interface(iface) => iface.name.name.to_string(),
                NodeRef::World(world) => world.name.name.to_string(),
                NodeRef::Func(func) => func.name.name.to_string(),
                NodeRef::Record(rec) => rec.name.name.to_string(),
                NodeRef::Variant(var) => var.name.name.to_string(),
                NodeRef::Enum(e) => e.name.name.to_string(),
                NodeRef::Flags(f) => f.name.name.to_string(),
                NodeRef::Resource(res) => res.name.name.to_string(),
                NodeRef::TypeAlias(alias) => alias.name.name.to_string(),
                _ => return None,
            };

            let mut highlights = Vec::new();

            if let Some(def) = index.find_definition(&name) {
                highlights.push(DocumentHighlight {
                    range: doc.range_to_lsp(def.name_range),
                    kind: Some(DocumentHighlightKind::TEXT),
                });
            }

            for reference in index.references() {
                if &*reference.name == name {
                    highlights.push(DocumentHighlight {
                        range: doc.range_to_lsp(reference.range),
                        kind: Some(DocumentHighlightKind::READ),
                    });
                }
            }

            if highlights.is_empty() {
                None
            } else {
                Some(highlights)
            }
        });

        Ok(highlights.flatten())
    }

    async fn selection_range(
        &self,
        params: SelectionRangeParams,
    ) -> Result<Option<Vec<SelectionRange>>> {
        let uri = params.text_document.uri.to_string();

        let ranges = self.documents.with_document_mut(&uri, |doc| {
            let parse = doc.parse().clone();
            let mut results = Vec::new();

            for position in &params.positions {
                let offset = doc.position_to_offset(witcraft_syntax::Position::new(
                    position.line,
                    position.character,
                ));

                if let Some(offset) = offset {
                    let selection = build_selection_range(&parse.root, offset, &doc.line_index);
                    results.push(selection);
                }
            }

            results
        });

        match ranges {
            Some(r) if !r.is_empty() => Ok(Some(r)),
            _ => Ok(None),
        }
    }

    async fn code_lens(&self, params: CodeLensParams) -> Result<Option<Vec<CodeLens>>> {
        let uri = params.text_document.uri.as_str();

        let lenses = self.documents.with_document_mut(uri, |doc| {
            let parse = doc.parse().clone();
            let index = SymbolIndex::build(&parse.root);
            collect_code_lenses(&index, &doc.line_index)
        });

        match lenses {
            Some(l) if !l.is_empty() => Ok(Some(l)),
            _ => Ok(None),
        }
    }
}

impl WitLanguageServer {
    /// Create a code action to remove an unused import.
    fn make_remove_import_action(
        &self,
        uri: &str,
        diag: &Diagnostic,
        index: &SymbolIndex,
        line_index: &witcraft_syntax::LineIndex,
        content: &str,
    ) -> Option<CodeActionOrCommand> {
        // Extract the import name from the diagnostic message
        let name = diag
            .message
            .strip_prefix("unused import `")?
            .strip_suffix('`')?;

        // Find the import in the index
        let import = index.find_import(name)?;

        // Count how many imports share the same use statement
        let siblings_count = index
            .imports()
            .iter()
            .filter(|i| i.use_statement_range == import.use_statement_range)
            .count();

        // Determine what range to delete
        let delete_range = if siblings_count == 1 {
            // Only import in this use statement - delete the whole statement
            import.use_statement_range
        } else {
            delete_import_item_range(import, content)
        };

        // Convert to LSP range
        let start = line_index.position(delete_range.start());
        let end = line_index.position(delete_range.end());
        let lsp_range = Range {
            start: tower_lsp::lsp_types::Position {
                line: start.line,
                character: start.column,
            },
            end: tower_lsp::lsp_types::Position {
                line: end.line,
                character: end.column,
            },
        };

        // Create the text edit
        let edit = TextEdit {
            range: lsp_range,
            new_text: String::new(),
        };

        let mut changes = std::collections::HashMap::new();
        changes.insert(Url::parse(uri).ok()?, vec![edit]);

        Some(CodeActionOrCommand::CodeAction(CodeAction {
            title: format!("Remove unused import `{}`", name),
            kind: Some(CodeActionKind::QUICKFIX),
            diagnostics: Some(vec![diag.clone()]),
            edit: Some(WorkspaceEdit {
                changes: Some(changes),
                ..Default::default()
            }),
            ..Default::default()
        }))
    }

    /// Create a code action to add a missing import.
    fn make_add_import_action(
        &self,
        uri: &str,
        diag: &Diagnostic,
        index: &SymbolIndex,
        root: &SourceFile,
        line_index: &witcraft_syntax::LineIndex,
    ) -> Option<CodeActionOrCommand> {
        // Extract the type name from the diagnostic message
        let type_name = extract_backticked_name(&diag.message, "undefined type ")?;

        // Find where this type is defined in the workspace
        let global_def = self.workspace.find_definition(uri, type_name)?;

        // Get the interface that contains this type
        let interface_name = global_def.parent.as_ref()?;

        let scope_offset = line_index.offset(witcraft_syntax::Position::new(
            diag.range.start.line,
            diag.range.start.character,
        ))?;
        let insertion = find_import_insertion(root, scope_offset);
        let mut insert_offset = insertion.offset;
        if !insertion.after_existing_use {
            let insert_line = line_index.position(insert_offset).line;
            if let Some(line_range) = line_index.line_range(insert_line) {
                insert_offset = line_range.start();
            }
        }
        let pos = line_index.position(insert_offset);
        let insert_position = tower_lsp::lsp_types::Position {
            line: pos.line,
            character: pos.column,
        };

        // Check if we already have a matching import in this file.
        let already_imported = index
            .imports()
            .iter()
            .any(|i| i.from_interface == *interface_name && i.original_name.as_ref() == type_name);
        if already_imported {
            return None;
        }

        let new_text = if insertion.after_existing_use {
            format!(
                "\n{}use {}.{{{}}};",
                insertion.indent, interface_name, type_name
            )
        } else {
            format!(
                "{}use {}.{{{}}};\n",
                insertion.indent, interface_name, type_name
            )
        };

        // Create the text edit
        let edit = TextEdit {
            range: Range {
                start: insert_position,
                end: insert_position,
            },
            new_text,
        };

        let mut changes = std::collections::HashMap::new();
        changes.insert(Url::parse(uri).ok()?, vec![edit]);

        Some(CodeActionOrCommand::CodeAction(CodeAction {
            title: format!("Import `{}` from `{}`", type_name, interface_name),
            kind: Some(CodeActionKind::QUICKFIX),
            diagnostics: Some(vec![diag.clone()]),
            edit: Some(WorkspaceEdit {
                changes: Some(changes),
                ..Default::default()
            }),
            ..Default::default()
        }))
    }

    fn make_ambiguity_actions(
        &self,
        uri: &str,
        diag: &Diagnostic,
        index: &SymbolIndex,
        line_index: &witcraft_syntax::LineIndex,
        use_path_ranges: &HashMap<String, TextRange>,
        ambiguous_types: &[AmbiguousType],
    ) -> Vec<CodeActionOrCommand> {
        let mut actions = Vec::new();
        let Some(current_uri) = Url::parse(uri).ok() else {
            return actions;
        };

        let Some(ambiguous) = ambiguous_types
            .iter()
            .find(|amb| range_to_lsp_with_index(line_index, amb.range) == diag.range)
        else {
            return actions;
        };

        let Some(import) = index.find_import(&ambiguous.name) else {
            return actions;
        };

        let Some(use_path_range) = use_path_ranges.get(ambiguous.name.as_str()) else {
            return actions;
        };

        let resolver = CrossFileResolver::new(&self.workspace);
        let candidates = match resolver.resolve_type(uri, &ambiguous.name, index) {
            ResolveResult::Ambiguous(defs) => collect_ambiguous_candidates(defs),
            _ => return actions,
        };

        let mut used_names: HashSet<String> = self
            .workspace
            .package_index_for_file(uri)
            .map(|pkg| pkg.interfaces().into_iter().collect())
            .unwrap_or_default();

        let use_path_range = *use_path_range;
        for candidate in candidates {
            let new_interface_name = make_unique_interface_name(
                &import.from_interface,
                &candidate.file,
                &mut used_names,
            );

            let Some(candidate_range) =
                self.interface_name_range_in_uri(&candidate.uri, &candidate.interface)
            else {
                continue;
            };

            let mut changes = HashMap::new();
            changes
                .entry(current_uri.clone())
                .or_insert_with(Vec::new)
                .push(TextEdit {
                    range: range_to_lsp_with_index(line_index, use_path_range),
                    new_text: new_interface_name.clone(),
                });

            let Some(candidate_uri) = Url::parse(&candidate.uri).ok() else {
                continue;
            };
            changes
                .entry(candidate_uri)
                .or_insert_with(Vec::new)
                .push(TextEdit {
                    range: candidate_range,
                    new_text: new_interface_name.clone(),
                });

            actions.push(CodeActionOrCommand::CodeAction(CodeAction {
                title: format!("Use `{}` from `{}`", new_interface_name, candidate.file),
                kind: Some(CodeActionKind::QUICKFIX),
                diagnostics: Some(vec![diag.clone()]),
                edit: Some(WorkspaceEdit {
                    changes: Some(changes),
                    ..Default::default()
                }),
                ..Default::default()
            }));
        }

        actions
    }
}

#[derive(Debug, Clone)]
struct AmbiguousCandidateEdit {
    interface: String,
    file: String,
    uri: String,
}

struct ImportInsertion {
    offset: u32,
    indent: &'static str,
    after_existing_use: bool,
}

fn collect_use_path_ranges(root: &SourceFile) -> HashMap<String, TextRange> {
    let mut ranges = HashMap::new();
    collect_use_path_ranges_in_items(&root.items, &mut ranges);
    for nested in &root.nested_packages {
        collect_use_path_ranges_in_items(&nested.items, &mut ranges);
    }
    ranges
}

fn collect_use_path_ranges_in_items(
    items: &[witcraft_syntax::ast::Item],
    ranges: &mut HashMap<String, TextRange>,
) {
    let mut add_use = |use_stmt: &witcraft_syntax::ast::InterfaceUse| {
        for name in &use_stmt.names {
            let local = name.alias.as_ref().unwrap_or(&name.name);
            ranges
                .entry(local.name.to_string())
                .or_insert(use_stmt.path.name.range);
        }
    };

    for item in items {
        match item {
            witcraft_syntax::ast::Item::Interface(iface) => {
                for iface_item in &iface.items {
                    if let witcraft_syntax::ast::InterfaceItem::Use(use_stmt) = iface_item {
                        add_use(use_stmt);
                    }
                }
            }
            witcraft_syntax::ast::Item::World(world) => {
                for world_item in &world.items {
                    if let witcraft_syntax::ast::WorldItem::Use(use_stmt) = world_item {
                        add_use(use_stmt);
                    }
                }
            }
            witcraft_syntax::ast::Item::TypeDef(_) => {}
        }
    }
}

fn collect_ambiguous_candidates(defs: Vec<GlobalDefinition>) -> Vec<AmbiguousCandidateEdit> {
    let mut candidates = Vec::new();
    for def in defs {
        let Some(parent) = def.parent.as_ref() else {
            continue;
        };
        let file = def.uri.rsplit('/').next().unwrap_or(&def.uri).to_string();
        candidates.push(AmbiguousCandidateEdit {
            interface: parent.to_string(),
            file,
            uri: def.uri,
        });
    }

    candidates.sort_by(|a, b| {
        a.interface
            .cmp(&b.interface)
            .then(a.file.cmp(&b.file))
            .then(a.uri.cmp(&b.uri))
    });
    candidates.dedup_by(|a, b| a.interface == b.interface && a.uri == b.uri);
    candidates
}

fn same_global_definition(a: &GlobalDefinition, b: &GlobalDefinition) -> bool {
    a.uri == b.uri
        && a.name == b.name
        && a.name_range == b.name_range
        && a.parent.as_deref() == b.parent.as_deref()
}

fn delete_import_item_range(import: &witcraft_syntax::Import, content: &str) -> TextRange {
    let bytes = content.as_bytes();
    let stmt_start = import.use_statement_range.start() as usize;
    let stmt_end = import.use_statement_range.end() as usize;
    let item_start = import.item_range.start() as usize;
    let item_end = import.item_range.end() as usize;

    let mut end = item_end;
    while end < stmt_end && is_ascii_space(bytes[end]) {
        end += 1;
    }
    if end < stmt_end && bytes[end] == b',' {
        end += 1;
        while end < stmt_end && is_ascii_space(bytes[end]) {
            end += 1;
        }
        return TextRange::new(item_start as u32, end as u32);
    }

    let mut start = item_start;
    while start > stmt_start && is_ascii_space(bytes[start - 1]) {
        start -= 1;
    }
    if start > stmt_start && bytes[start - 1] == b',' {
        start -= 1;
        while start > stmt_start && is_ascii_space(bytes[start - 1]) {
            start -= 1;
        }
    }

    TextRange::new(start as u32, item_end as u32)
}

fn is_ascii_space(byte: u8) -> bool {
    matches!(byte, b' ' | b'\t' | b'\r' | b'\n')
}

fn find_import_insertion(root: &SourceFile, scope_offset: u32) -> ImportInsertion {
    for item in &root.items {
        match item {
            witcraft_syntax::Item::Interface(iface) if iface.range.contains(scope_offset) => {
                if let Some(last_use_end) = iface.items.iter().rev().find_map(|i| match i {
                    witcraft_syntax::InterfaceItem::Use(use_stmt) => Some(use_stmt.range.end()),
                    _ => None,
                }) {
                    return ImportInsertion {
                        offset: last_use_end,
                        indent: "    ",
                        after_existing_use: true,
                    };
                }
                if let Some(first_item) = iface.items.first() {
                    return ImportInsertion {
                        offset: first_item.range().start(),
                        indent: "    ",
                        after_existing_use: false,
                    };
                }
                return ImportInsertion {
                    offset: iface.range.end().saturating_sub(1),
                    indent: "    ",
                    after_existing_use: false,
                };
            }
            witcraft_syntax::Item::World(world) if world.range.contains(scope_offset) => {
                if let Some(last_use_end) = world.items.iter().rev().find_map(|i| match i {
                    witcraft_syntax::WorldItem::Use(use_stmt) => Some(use_stmt.range.end()),
                    _ => None,
                }) {
                    return ImportInsertion {
                        offset: last_use_end,
                        indent: "    ",
                        after_existing_use: true,
                    };
                }
                if let Some(first_item) = world.items.first() {
                    return ImportInsertion {
                        offset: first_item.range().start(),
                        indent: "    ",
                        after_existing_use: false,
                    };
                }
                return ImportInsertion {
                    offset: world.range.end().saturating_sub(1),
                    indent: "    ",
                    after_existing_use: false,
                };
            }
            _ => {}
        }
    }

    if let Some(last_use) = root.uses.last() {
        return ImportInsertion {
            offset: last_use.range.end(),
            indent: "",
            after_existing_use: true,
        };
    }
    if let Some(first_item) = root.items.first() {
        return ImportInsertion {
            offset: first_item.range().start(),
            indent: "",
            after_existing_use: false,
        };
    }

    ImportInsertion {
        offset: root.range.end(),
        indent: "",
        after_existing_use: false,
    }
}

fn extract_backticked_name<'a>(message: &'a str, prefix: &str) -> Option<&'a str> {
    let rest = message.strip_prefix(prefix)?;
    let start = rest.find('`')?;
    let rest = &rest[start + 1..];
    let end = rest.find('`')?;
    Some(&rest[..end])
}

fn make_unique_interface_name(
    base_interface: &str,
    candidate_file: &str,
    used: &mut HashSet<String>,
) -> String {
    let stem = Path::new(candidate_file)
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or(candidate_file);
    let mut sanitized: String = stem
        .chars()
        .map(|c| {
            if c.is_ascii_alphanumeric() || c == '-' || c == '_' {
                c
            } else {
                '-'
            }
        })
        .collect();
    sanitized = sanitized.trim_matches('-').to_string();
    if sanitized.is_empty() {
        sanitized = "alt".to_string();
    }

    let mut base = format!("{}-{}", base_interface, sanitized);
    if base == base_interface {
        base.push_str("-alt");
    }

    let mut candidate = base.clone();
    let mut counter = 1;
    while used.contains(&candidate) {
        candidate = format!("{}-{}", base, counter);
        counter += 1;
    }
    used.insert(candidate.clone());
    candidate
}

fn find_interface_name_range(root: &SourceFile, interface_name: &str) -> Option<TextRange> {
    if let Some(range) = find_interface_name_range_in_items(&root.items, interface_name) {
        return Some(range);
    }
    for nested in &root.nested_packages {
        if let Some(range) = find_interface_name_range_in_items(&nested.items, interface_name) {
            return Some(range);
        }
    }
    None
}

fn find_interface_name_range_in_items(
    items: &[witcraft_syntax::ast::Item],
    interface_name: &str,
) -> Option<TextRange> {
    for item in items {
        if let witcraft_syntax::ast::Item::Interface(iface) = item {
            if iface.name.name.as_ref() == interface_name {
                return Some(iface.name.range);
            }
        }
    }
    None
}

fn range_to_lsp_with_index(
    line_index: &witcraft_syntax::LineIndex,
    range: witcraft_syntax::TextRange,
) -> Range {
    let start = line_index.position(range.start());
    let end = line_index.position(range.end());
    Range {
        start: tower_lsp::lsp_types::Position {
            line: start.line,
            character: start.column,
        },
        end: tower_lsp::lsp_types::Position {
            line: end.line,
            character: end.column,
        },
    }
}

fn build_interface_symbols<F>(
    iface: &witcraft_syntax::ast::InterfaceDecl,
    range_to_lsp: &F,
) -> Vec<DocumentSymbol>
where
    F: Fn(witcraft_syntax::TextRange) -> Range,
{
    let mut children = Vec::new();

    for item in &iface.items {
        match item {
            witcraft_syntax::ast::InterfaceItem::TypeDef(typedef) => {
                if let Some(sym) = build_typedef_symbol(typedef, range_to_lsp) {
                    children.push(sym);
                }
            }
            witcraft_syntax::ast::InterfaceItem::Func(func) => {
                #[allow(deprecated)]
                children.push(DocumentSymbol {
                    name: func.name.name.to_string(),
                    detail: Some("func".to_string()),
                    kind: SymbolKind::FUNCTION,
                    tags: None,
                    deprecated: None,
                    range: range_to_lsp(func.range),
                    selection_range: range_to_lsp(func.name.range),
                    children: None,
                });
            }
            witcraft_syntax::ast::InterfaceItem::Use(use_stmt) => {
                let names: Vec<_> = use_stmt.names.iter().map(|n| &*n.name.name).collect();
                #[allow(deprecated)]
                children.push(DocumentSymbol {
                    name: format!("use {}", use_stmt.path.name.name),
                    detail: Some(format!("{{{}}}", names.join(", "))),
                    kind: SymbolKind::NAMESPACE,
                    tags: None,
                    deprecated: None,
                    range: range_to_lsp(use_stmt.range),
                    selection_range: range_to_lsp(use_stmt.path.name.range),
                    children: None,
                });
            }
        }
    }

    children
}

fn build_world_symbols<F>(
    world: &witcraft_syntax::ast::WorldDecl,
    range_to_lsp: &F,
) -> Vec<DocumentSymbol>
where
    F: Fn(witcraft_syntax::TextRange) -> Range,
{
    let mut children = Vec::new();

    for item in &world.items {
        match item {
            witcraft_syntax::ast::WorldItem::Import(import) => {
                #[allow(deprecated)]
                children.push(DocumentSymbol {
                    name: format!("import {}", import.name.name),
                    detail: None,
                    kind: SymbolKind::PROPERTY,
                    tags: None,
                    deprecated: None,
                    range: range_to_lsp(import.range),
                    selection_range: range_to_lsp(import.name.range),
                    children: None,
                });
            }
            witcraft_syntax::ast::WorldItem::Export(export) => {
                #[allow(deprecated)]
                children.push(DocumentSymbol {
                    name: format!("export {}", export.name.name),
                    detail: None,
                    kind: SymbolKind::PROPERTY,
                    tags: None,
                    deprecated: None,
                    range: range_to_lsp(export.range),
                    selection_range: range_to_lsp(export.name.range),
                    children: None,
                });
            }
            witcraft_syntax::ast::WorldItem::Include(include) => {
                #[allow(deprecated)]
                children.push(DocumentSymbol {
                    name: format!("include {}", include.path.name.name),
                    detail: None,
                    kind: SymbolKind::NAMESPACE,
                    tags: None,
                    deprecated: None,
                    range: range_to_lsp(include.range),
                    selection_range: range_to_lsp(include.path.name.range),
                    children: None,
                });
            }
            witcraft_syntax::ast::WorldItem::TypeDef(typedef) => {
                if let Some(sym) = build_typedef_symbol(typedef, range_to_lsp) {
                    children.push(sym);
                }
            }
            witcraft_syntax::ast::WorldItem::Use(use_stmt) => {
                let names: Vec<_> = use_stmt.names.iter().map(|n| &*n.name.name).collect();
                #[allow(deprecated)]
                children.push(DocumentSymbol {
                    name: format!("use {}", use_stmt.path.name.name),
                    detail: Some(format!("{{{}}}", names.join(", "))),
                    kind: SymbolKind::NAMESPACE,
                    tags: None,
                    deprecated: None,
                    range: range_to_lsp(use_stmt.range),
                    selection_range: range_to_lsp(use_stmt.path.name.range),
                    children: None,
                });
            }
        }
    }

    children
}

fn build_typedef_symbol<F>(
    typedef: &witcraft_syntax::ast::TypeDef,
    range_to_lsp: &F,
) -> Option<DocumentSymbol>
where
    F: Fn(witcraft_syntax::TextRange) -> Range,
{
    let (name, kind, detail, range, name_range) = match typedef {
        witcraft_syntax::ast::TypeDef::Alias(alias) => (
            alias.name.name.to_string(),
            SymbolKind::TYPE_PARAMETER,
            "type",
            alias.range,
            alias.name.range,
        ),
        witcraft_syntax::ast::TypeDef::Record(rec) => (
            rec.name.name.to_string(),
            SymbolKind::STRUCT,
            "record",
            rec.range,
            rec.name.range,
        ),
        witcraft_syntax::ast::TypeDef::Variant(var) => (
            var.name.name.to_string(),
            SymbolKind::ENUM,
            "variant",
            var.range,
            var.name.range,
        ),
        witcraft_syntax::ast::TypeDef::Enum(e) => (
            e.name.name.to_string(),
            SymbolKind::ENUM,
            "enum",
            e.range,
            e.name.range,
        ),
        witcraft_syntax::ast::TypeDef::Flags(f) => (
            f.name.name.to_string(),
            SymbolKind::ENUM,
            "flags",
            f.range,
            f.name.range,
        ),
        witcraft_syntax::ast::TypeDef::Resource(res) => (
            res.name.name.to_string(),
            SymbolKind::CLASS,
            "resource",
            res.range,
            res.name.range,
        ),
    };

    #[allow(deprecated)]
    Some(DocumentSymbol {
        name,
        detail: Some(detail.to_string()),
        kind,
        tags: None,
        deprecated: None,
        range: range_to_lsp(range),
        selection_range: range_to_lsp(name_range),
        children: None,
    })
}

const PRIMITIVE_TYPES: &[&str] = &[
    "bool", "u8", "u16", "u32", "u64", "s8", "s16", "s32", "s64", "f32", "f64", "char", "string",
];

const GENERIC_TYPES: &[&str] = &["list", "option", "result", "tuple", "borrow", "own"];

fn get_context_keywords(source: &str, offset: u32) -> Vec<&'static str> {
    let before = &source[..offset as usize];
    let trimmed = before.trim_end();

    if trimmed.ends_with('{') || trimmed.is_empty() || trimmed.ends_with(';') {
        vec![
            "type", "record", "variant", "enum", "flags", "resource", "use",
        ]
    } else if trimmed.ends_with("interface") || trimmed.ends_with("world") {
        vec![]
    } else {
        vec![]
    }
}

fn format_definition(def: &witcraft_syntax::Definition) -> String {
    let kind_str = match def.kind {
        DefinitionKind::Interface => "interface",
        DefinitionKind::World => "world",
        DefinitionKind::TypeAlias => "type",
        DefinitionKind::Record => "record",
        DefinitionKind::Variant => "variant",
        DefinitionKind::Enum => "enum",
        DefinitionKind::Flags => "flags",
        DefinitionKind::Resource => "resource",
        DefinitionKind::Function => "func",
    };

    let mut result = format!("```wit\n{} {}\n```", kind_str, def.name);

    if let Some(parent) = &def.parent {
        result.push_str(&format!("\n\n*Defined in `{}`*", parent));
    }

    result
}

fn format_global_definition(def: &GlobalDefinition) -> String {
    let kind_str = match def.kind {
        DefinitionKind::Interface => "interface",
        DefinitionKind::World => "world",
        DefinitionKind::TypeAlias => "type",
        DefinitionKind::Record => "record",
        DefinitionKind::Variant => "variant",
        DefinitionKind::Enum => "enum",
        DefinitionKind::Flags => "flags",
        DefinitionKind::Resource => "resource",
        DefinitionKind::Function => "func",
    };

    let mut result = format!("```wit\n{} {}\n```", kind_str, def.name);

    if let Some(parent) = &def.parent {
        result.push_str(&format!("\n\n*Defined in `{}`*", parent));
    }

    // Show the file location for cross-file definitions
    if let Some(file_name) = def.uri.rsplit('/').next() {
        result.push_str(&format!("\n\n*From `{}`*", file_name));
    }

    result
}

/// Find signature help context by scanning backwards from cursor position.
/// Returns SignatureHelp if cursor is inside a generic type or function parameter list.
fn find_signature_context(content: &str, offset: usize) -> Option<SignatureHelp> {
    let before = &content[..offset.min(content.len())];

    // Track nesting depth to handle nested generics/parens
    let mut angle_depth = 0i32;
    let mut paren_depth = 0i32;
    let mut comma_count = 0u32;
    let mut context_start = None;

    // Scan backwards to find the opening `<` or `(`
    for (i, ch) in before.char_indices().rev() {
        match ch {
            '>' => angle_depth += 1,
            '<' => {
                if angle_depth > 0 {
                    angle_depth -= 1;
                } else {
                    context_start = Some((i, '<'));
                    break;
                }
            }
            ')' => paren_depth += 1,
            '(' => {
                if paren_depth > 0 {
                    paren_depth -= 1;
                } else {
                    context_start = Some((i, '('));
                    break;
                }
            }
            ',' if angle_depth == 0 && paren_depth == 0 => {
                comma_count += 1;
            }
            // Stop at statement boundaries
            ';' | '{' | '}' => break,
            _ => {}
        }
    }

    let (open_pos, open_char) = context_start?;

    // Extract the keyword before the opening bracket
    let keyword_end = open_pos;
    let keyword_start = before[..keyword_end]
        .rfind(|c: char| !c.is_alphanumeric() && c != '-' && c != '_')
        .map(|i| i + 1)
        .unwrap_or(0);
    let keyword = before[keyword_start..keyword_end].trim();

    // Build signature help based on context
    match (keyword, open_char) {
        ("result", '<') => Some(make_signature_help(
            "result<ok-type, err-type>",
            "Result type with success and error variants.\n\n- Use `_` for ok-type if only error matters\n- Omit err-type for infallible results: `result<T>`",
            &[
                ("ok-type", "The success type (use `_` to omit)"),
                ("err-type", "The error type"),
            ],
            comma_count,
        )),
        ("option", '<') => Some(make_signature_help(
            "option<inner-type>",
            "Optional type that may contain a value or be none.",
            &[("inner-type", "The wrapped type")],
            comma_count,
        )),
        ("list", '<') => Some(make_signature_help(
            "list<element-type>",
            "A variable-length sequence of elements.",
            &[("element-type", "The type of list elements")],
            comma_count,
        )),
        ("tuple", '<') => Some(make_signature_help(
            "tuple<T, U, ...>",
            "A fixed-size sequence of heterogeneous types.",
            &[
                ("T", "First element type"),
                ("U", "Second element type"),
                ("...", "Additional element types"),
            ],
            comma_count,
        )),
        ("borrow", '<') => Some(make_signature_help(
            "borrow<resource-name>",
            "A borrowed handle to a resource. The resource is not consumed.",
            &[("resource-name", "The resource type to borrow")],
            comma_count,
        )),
        ("own", '<') => Some(make_signature_help(
            "own<resource-name>",
            "An owned handle to a resource. Ownership is transferred.",
            &[("resource-name", "The resource type to own")],
            comma_count,
        )),
        ("future", '<') => Some(make_signature_help(
            "future<inner-type>",
            "An async value that will be available later.\n\nCan also be used without type parameter: `future`",
            &[("inner-type", "The type of the future value")],
            comma_count,
        )),
        ("stream", '<') => Some(make_signature_help(
            "stream<element-type>",
            "An async sequence of values.\n\nCan also be used without type parameter: `stream`",
            &[("element-type", "The type of stream elements")],
            comma_count,
        )),
        ("func", '(') => Some(make_signature_help(
            "func(name: type, ...) -> return-type",
            "Function signature with named parameters.\n\n- Parameters: `name: type`\n- Return: `-> type` or `-> (name: type, ...)`",
            &[
                ("name: type", "Parameter with name and type"),
                ("...", "Additional parameters"),
            ],
            comma_count,
        )),
        ("constructor", '(') => Some(make_signature_help(
            "constructor(name: type, ...)",
            "Resource constructor with parameters.\n\nOptionally returns a result: `constructor(...) -> result<_, error>`",
            &[
                ("name: type", "Parameter with name and type"),
                ("...", "Additional parameters"),
            ],
            comma_count,
        )),
        _ => None,
    }
}

/// Build a SignatureHelp response.
fn make_signature_help(
    label: &str,
    doc: &str,
    params: &[(&str, &str)],
    active_param: u32,
) -> SignatureHelp {
    let parameters: Vec<ParameterInformation> = params
        .iter()
        .map(|(name, description)| ParameterInformation {
            label: ParameterLabel::Simple(name.to_string()),
            documentation: Some(Documentation::String(description.to_string())),
        })
        .collect();

    SignatureHelp {
        signatures: vec![SignatureInformation {
            label: label.to_string(),
            documentation: Some(Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: doc.to_string(),
            })),
            parameters: Some(parameters),
            active_parameter: Some(active_param.min(params.len().saturating_sub(1) as u32)),
        }],
        active_signature: Some(0),
        active_parameter: Some(active_param.min(params.len().saturating_sub(1) as u32)),
    }
}

fn collect_inlay_hints(
    root: &witcraft_syntax::SourceFile,
    workspace: &SharedWorkspaceManager,
    uri: &str,
    line_index: &witcraft_syntax::LineIndex,
) -> Vec<InlayHint> {
    let mut hints = Vec::new();

    for item in &root.items {
        collect_hints_from_item(item, workspace, uri, line_index, &mut hints);
    }

    hints
}

fn collect_hints_from_item(
    item: &witcraft_syntax::Item,
    workspace: &SharedWorkspaceManager,
    uri: &str,
    line_index: &witcraft_syntax::LineIndex,
    hints: &mut Vec<InlayHint>,
) {
    match item {
        witcraft_syntax::Item::Interface(iface) => {
            for iface_item in &iface.items {
                match iface_item {
                    witcraft_syntax::InterfaceItem::TypeDef(typedef) => {
                        if let witcraft_syntax::TypeDef::Alias(alias) = typedef {
                            add_type_alias_hint(alias, line_index, hints);
                        }
                    }
                    witcraft_syntax::InterfaceItem::Use(use_stmt) => {
                        add_use_source_hints(use_stmt, workspace, uri, line_index, hints);
                    }
                    _ => {}
                }
            }
        }
        witcraft_syntax::Item::World(world) => {
            for world_item in &world.items {
                match world_item {
                    witcraft_syntax::WorldItem::TypeDef(typedef) => {
                        if let witcraft_syntax::TypeDef::Alias(alias) = typedef {
                            add_type_alias_hint(alias, line_index, hints);
                        }
                    }
                    witcraft_syntax::WorldItem::Use(use_stmt) => {
                        add_use_source_hints(use_stmt, workspace, uri, line_index, hints);
                    }
                    _ => {}
                }
            }
        }
        witcraft_syntax::Item::TypeDef(typedef) => {
            if let witcraft_syntax::TypeDef::Alias(alias) = typedef {
                add_type_alias_hint(alias, line_index, hints);
            }
        }
    }
}

fn add_type_alias_hint(
    alias: &witcraft_syntax::TypeAlias,
    line_index: &witcraft_syntax::LineIndex,
    hints: &mut Vec<InlayHint>,
) {
    let type_text = format_type(&alias.ty);
    if type_text.len() <= 30 {
        let pos = line_index.position(alias.name.range.end());
        hints.push(InlayHint {
            position: Position::new(pos.line, pos.column),
            label: InlayHintLabel::String(format!(": {}", type_text)),
            kind: Some(InlayHintKind::TYPE),
            text_edits: None,
            tooltip: None,
            padding_left: Some(true),
            padding_right: None,
            data: None,
        });
    }
}

fn add_use_source_hints(
    use_stmt: &witcraft_syntax::InterfaceUse,
    workspace: &SharedWorkspaceManager,
    uri: &str,
    line_index: &witcraft_syntax::LineIndex,
    hints: &mut Vec<InlayHint>,
) {
    let source = &use_stmt.path.name.name;
    if let Some(pkg) = workspace.package_index_for_file(uri) {
        for name_item in &use_stmt.names {
            if let Some(def) = pkg.find_definition(name_item.name.name.as_ref()) {
                if def.uri.as_str() != uri {
                    let file_name = def.uri.rsplit('/').next().unwrap_or(&def.uri);
                    let pos = line_index.position(name_item.range.end());
                    hints.push(InlayHint {
                        position: Position::new(pos.line, pos.column),
                        label: InlayHintLabel::String(format!(" ({})", file_name)),
                        kind: None,
                        text_edits: None,
                        tooltip: Some(InlayHintTooltip::String(format!(
                            "Imported from {} in {}",
                            source, def.uri
                        ))),
                        padding_left: None,
                        padding_right: None,
                        data: None,
                    });
                }
            }
        }
    }
}

fn format_type(ty: &witcraft_syntax::Type) -> String {
    match ty {
        witcraft_syntax::Type::Named(named) => named.name.name.to_string(),
        witcraft_syntax::Type::Primitive(p) => format!("{:?}", p.kind).to_lowercase(),
        witcraft_syntax::Type::List(list) => format!("list<{}>", format_type(&list.element)),
        witcraft_syntax::Type::Option(opt) => format!("option<{}>", format_type(&opt.inner)),
        witcraft_syntax::Type::Result(res) => match (&res.ok, &res.err) {
            (Some(ok), Some(err)) => format!("result<{}, {}>", format_type(ok), format_type(err)),
            (Some(ok), None) => format!("result<{}>", format_type(ok)),
            (None, Some(err)) => format!("result<_, {}>", format_type(err)),
            (None, None) => "result".to_string(),
        },
        witcraft_syntax::Type::Tuple(tuple) => {
            let elems: Vec<_> = tuple.elements.iter().map(format_type).collect();
            format!("tuple<{}>", elems.join(", "))
        }
        witcraft_syntax::Type::Borrow(b) => format!("borrow<{}>", b.resource.name),
        witcraft_syntax::Type::Own(o) => format!("own<{}>", o.resource.name),
        witcraft_syntax::Type::Future(f) => match &f.inner {
            Some(inner) => format!("future<{}>", format_type(inner)),
            None => "future".to_string(),
        },
        witcraft_syntax::Type::Stream(s) => match &s.inner {
            Some(inner) => format!("stream<{}>", format_type(inner)),
            None => "stream".to_string(),
        },
    }
}

fn build_selection_range(
    root: &witcraft_syntax::SourceFile,
    offset: u32,
    line_index: &witcraft_syntax::LineIndex,
) -> SelectionRange {
    let mut ranges: Vec<witcraft_syntax::TextRange> = Vec::new();

    ranges.push(root.range);

    for item in &root.items {
        if item.range().contains(offset) {
            collect_containing_ranges(item, offset, &mut ranges);
        }
    }

    ranges.sort_by_key(|r| std::cmp::Reverse(r.len()));

    let to_lsp_range = |r: witcraft_syntax::TextRange| {
        let start = line_index.position(r.start());
        let end = line_index.position(r.end());
        Range::new(
            Position::new(start.line, start.column),
            Position::new(end.line, end.column),
        )
    };

    let mut result: Option<SelectionRange> = None;

    for range in ranges {
        result = Some(SelectionRange {
            range: to_lsp_range(range),
            parent: result.map(Box::new),
        });
    }

    result.unwrap_or_else(|| SelectionRange {
        range: to_lsp_range(root.range),
        parent: None,
    })
}

fn collect_containing_ranges(
    item: &witcraft_syntax::Item,
    offset: u32,
    ranges: &mut Vec<witcraft_syntax::TextRange>,
) {
    ranges.push(item.range());

    match item {
        witcraft_syntax::Item::Interface(iface) => {
            if iface.name.range.contains(offset) {
                ranges.push(iface.name.range);
                return;
            }
            for iface_item in &iface.items {
                if iface_item.range().contains(offset) {
                    collect_interface_item_ranges(iface_item, offset, ranges);
                }
            }
        }
        witcraft_syntax::Item::World(world) => {
            if world.name.range.contains(offset) {
                ranges.push(world.name.range);
                return;
            }
            for world_item in &world.items {
                if world_item.range().contains(offset) {
                    collect_world_item_ranges(world_item, offset, ranges);
                }
            }
        }
        witcraft_syntax::Item::TypeDef(td) => {
            collect_typedef_ranges(td, offset, ranges);
        }
    }
}

fn collect_interface_item_ranges(
    item: &witcraft_syntax::InterfaceItem,
    offset: u32,
    ranges: &mut Vec<witcraft_syntax::TextRange>,
) {
    ranges.push(item.range());

    match item {
        witcraft_syntax::InterfaceItem::TypeDef(td) => {
            collect_typedef_ranges(td, offset, ranges);
        }
        witcraft_syntax::InterfaceItem::Func(func) => {
            if func.name.range.contains(offset) {
                ranges.push(func.name.range);
            } else {
                for param in &func.sig.params {
                    if param.range.contains(offset) {
                        ranges.push(param.range);
                        if param.name.range.contains(offset) {
                            ranges.push(param.name.range);
                        } else {
                            collect_type_ranges(&param.ty, offset, ranges);
                        }
                        return;
                    }
                }
            }
        }
        witcraft_syntax::InterfaceItem::Use(use_stmt) => {
            if use_stmt.path.range.contains(offset) {
                ranges.push(use_stmt.path.range);
            }
        }
    }
}

fn collect_world_item_ranges(
    item: &witcraft_syntax::WorldItem,
    offset: u32,
    ranges: &mut Vec<witcraft_syntax::TextRange>,
) {
    ranges.push(item.range());

    match item {
        witcraft_syntax::WorldItem::TypeDef(td) => {
            collect_typedef_ranges(td, offset, ranges);
        }
        witcraft_syntax::WorldItem::Import(imp) => {
            if imp.name.range.contains(offset) {
                ranges.push(imp.name.range);
            }
        }
        witcraft_syntax::WorldItem::Export(exp) => {
            if exp.name.range.contains(offset) {
                ranges.push(exp.name.range);
            }
        }
        _ => {}
    }
}

fn collect_typedef_ranges(
    td: &witcraft_syntax::TypeDef,
    offset: u32,
    ranges: &mut Vec<witcraft_syntax::TextRange>,
) {
    ranges.push(td.range());

    match td {
        witcraft_syntax::TypeDef::Alias(alias) => {
            if alias.name.range.contains(offset) {
                ranges.push(alias.name.range);
            } else {
                collect_type_ranges(&alias.ty, offset, ranges);
            }
        }
        witcraft_syntax::TypeDef::Record(rec) => {
            if rec.name.range.contains(offset) {
                ranges.push(rec.name.range);
            } else {
                for field in &rec.fields {
                    if field.range.contains(offset) {
                        ranges.push(field.range);
                        if field.name.range.contains(offset) {
                            ranges.push(field.name.range);
                        } else {
                            collect_type_ranges(&field.ty, offset, ranges);
                        }
                        return;
                    }
                }
            }
        }
        witcraft_syntax::TypeDef::Variant(var) => {
            if var.name.range.contains(offset) {
                ranges.push(var.name.range);
            } else {
                for case in &var.cases {
                    if case.range.contains(offset) {
                        ranges.push(case.range);
                        if case.name.range.contains(offset) {
                            ranges.push(case.name.range);
                        } else if let Some(ty) = &case.ty {
                            collect_type_ranges(ty, offset, ranges);
                        }
                        return;
                    }
                }
            }
        }
        witcraft_syntax::TypeDef::Enum(e) => {
            if e.name.range.contains(offset) {
                ranges.push(e.name.range);
            } else {
                for case in &e.cases {
                    if case.range.contains(offset) {
                        ranges.push(case.range);
                        return;
                    }
                }
            }
        }
        witcraft_syntax::TypeDef::Flags(f) => {
            if f.name.range.contains(offset) {
                ranges.push(f.name.range);
            } else {
                for flag in &f.flags {
                    if flag.range.contains(offset) {
                        ranges.push(flag.range);
                        return;
                    }
                }
            }
        }
        witcraft_syntax::TypeDef::Resource(res) => {
            if res.name.range.contains(offset) {
                ranges.push(res.name.range);
            }
        }
    }
}

fn collect_type_ranges(
    ty: &witcraft_syntax::Type,
    offset: u32,
    ranges: &mut Vec<witcraft_syntax::TextRange>,
) {
    if !ty.range().contains(offset) {
        return;
    }

    ranges.push(ty.range());

    match ty {
        witcraft_syntax::Type::Named(named) => {
            if named.name.range.contains(offset) {
                ranges.push(named.name.range);
            }
        }
        witcraft_syntax::Type::List(list) => {
            collect_type_ranges(&list.element, offset, ranges);
        }
        witcraft_syntax::Type::Option(opt) => {
            collect_type_ranges(&opt.inner, offset, ranges);
        }
        witcraft_syntax::Type::Result(res) => {
            if let Some(ok) = &res.ok {
                collect_type_ranges(ok, offset, ranges);
            }
            if let Some(err) = &res.err {
                collect_type_ranges(err, offset, ranges);
            }
        }
        witcraft_syntax::Type::Tuple(tuple) => {
            for elem in &tuple.elements {
                collect_type_ranges(elem, offset, ranges);
            }
        }
        witcraft_syntax::Type::Borrow(handle) | witcraft_syntax::Type::Own(handle) => {
            if handle.resource.range.contains(offset) {
                ranges.push(handle.resource.range);
            }
        }
        witcraft_syntax::Type::Future(future) => {
            if let Some(inner) = &future.inner {
                collect_type_ranges(inner, offset, ranges);
            }
        }
        witcraft_syntax::Type::Stream(stream) => {
            if let Some(inner) = &stream.inner {
                collect_type_ranges(inner, offset, ranges);
            }
        }
        witcraft_syntax::Type::Primitive(_) => {}
    }
}

fn collect_code_lenses(
    index: &SymbolIndex,
    line_index: &witcraft_syntax::LineIndex,
) -> Vec<CodeLens> {
    let mut lenses = Vec::new();
    let references = index.references();

    for def in index.definitions() {
        let ref_count = references.iter().filter(|r| r.name == def.name).count();
        if ref_count > 0 {
            let pos = line_index.position(def.name_range.start());
            let range = Range::new(
                Position::new(pos.line, pos.column),
                Position::new(pos.line, pos.column),
            );

            let title = if ref_count == 1 {
                "1 reference".to_string()
            } else {
                format!("{} references", ref_count)
            };

            lenses.push(CodeLens {
                range,
                command: Some(Command {
                    title,
                    command: "editor.action.findReferences".to_string(),
                    arguments: None,
                }),
                data: None,
            });
        }
    }

    lenses
}

fn format_undefined_type_message(undef: &UndefinedType) -> String {
    let mut msg = format!("undefined type `{}`", undef.name);

    if !undef.available_in.is_empty() {
        let first = &undef.available_in[0];
        msg.push_str(&format!(
            ". add import: `use {}.{{{}}}`",
            first.interface, undef.name
        ));
    } else if !undef.similar_names.is_empty() {
        let suggestion = &undef.similar_names[0];
        msg.push_str(&format!(". did you mean `{}`", suggestion.name));
        if let Some(iface) = &suggestion.interface {
            msg.push_str(&format!(" from `{}`", iface));
        }
        msg.push('?');
    }

    msg
}

fn format_ambiguous_type_message(amb: &AmbiguousType) -> String {
    let mut msg = format!("ambiguous type `{}`", amb.name);
    if !amb.candidates.is_empty() {
        let choices = amb
            .candidates
            .iter()
            .take(3)
            .map(|c| format!("`{}` in `{}`", c.interface, c.file))
            .collect::<Vec<_>>()
            .join(", ");
        msg.push_str(&format!(". candidates: {}", choices));
    }
    msg
}

fn format_ambiguous_hover(name: &str, candidates: Vec<GlobalDefinition>) -> String {
    let mut lines = vec![format!("`{}` is ambiguous:", name)];
    for candidate in collect_ambiguous_candidates(candidates) {
        lines.push(format!(
            "- `{}` in `{}`",
            candidate.interface, candidate.file
        ));
    }
    lines.join("\n")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document::DocumentStore;
    use serde_json::json;
    use std::fs;
    use std::future::Future;
    use std::path::{Path, PathBuf};
    use std::task::Poll;
    use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};
    use tower::{Service, ServiceExt};
    use tower_lsp::ClientSocket;
    use tower_lsp::LspService;
    use tower_lsp::jsonrpc::{Request, Response};

    const TEST_TIMEOUT: Duration = Duration::from_secs(15);

    async fn with_test_timeout<F, T>(future: F, context: String) -> T
    where
        F: Future<Output = T>,
    {
        let started = Instant::now();
        tokio::pin!(future);

        loop {
            if started.elapsed() > TEST_TIMEOUT {
                panic!("timed out after {:?}: {context}", TEST_TIMEOUT);
            }

            let maybe_output = std::future::poll_fn(|cx| match future.as_mut().poll(cx) {
                Poll::Ready(output) => Poll::Ready(Some(output)),
                Poll::Pending => Poll::Ready(None),
            })
            .await;

            if let Some(output) = maybe_output {
                return output;
            }

            tokio::task::yield_now().await;
        }
    }

    fn make_store_with_doc(content: &str) -> DocumentStore {
        let store = DocumentStore::new();
        store.open("file:///test.wit".into(), 1, content.into());
        store
    }

    /// Helper to get folding ranges from a document
    fn get_folding_ranges(content: &str) -> Vec<FoldingRange> {
        let store = make_store_with_doc(content);
        store
            .with_document_mut("file:///test.wit", |doc| {
                let parse = doc.parse().clone();
                let line_index = doc.line_index.clone();
                let mut ranges = Vec::new();

                let mut add_range = |range: witcraft_syntax::TextRange| {
                    let start = line_index.position(range.start());
                    let end = line_index.position(range.end());
                    if end.line > start.line {
                        ranges.push(FoldingRange {
                            start_line: start.line,
                            start_character: Some(start.column),
                            end_line: end.line,
                            end_character: Some(end.column),
                            kind: Some(FoldingRangeKind::Region),
                            collapsed_text: None,
                        });
                    }
                };

                for item in &parse.root.items {
                    match item {
                        witcraft_syntax::Item::Interface(iface) => {
                            add_range(iface.range);
                            for item in &iface.items {
                                if let witcraft_syntax::InterfaceItem::TypeDef(td) = item {
                                    add_range(td.range());
                                }
                            }
                        }
                        witcraft_syntax::Item::World(world) => {
                            add_range(world.range);
                        }
                        witcraft_syntax::Item::TypeDef(td) => {
                            add_range(td.range());
                        }
                    }
                }

                for nested in &parse.root.nested_packages {
                    add_range(nested.range);
                }

                ranges
            })
            .unwrap_or_default()
    }

    #[test]
    fn folding_range_interface() {
        let content = r#"interface api {
    greet: func(name: string);
    goodbye: func();
}"#;
        let ranges = get_folding_ranges(content);
        assert_eq!(ranges.len(), 1);
        assert_eq!(ranges[0].start_line, 0);
        assert_eq!(ranges[0].end_line, 3);
    }

    #[test]
    fn folding_range_world() {
        let content = r#"world my-world {
    import log: func(msg: string);
    export run: func();
}"#;
        let ranges = get_folding_ranges(content);
        assert_eq!(ranges.len(), 1);
        assert_eq!(ranges[0].start_line, 0);
        assert_eq!(ranges[0].end_line, 3);
    }

    #[test]
    fn folding_range_nested_types() {
        let content = r#"interface api {
    record user {
        id: u64,
        name: string,
    }

    enum status {
        pending,
        active,
    }
}"#;
        let ranges = get_folding_ranges(content);
        // Interface + record + enum = 3 folding ranges
        assert_eq!(ranges.len(), 3);
    }

    #[test]
    fn folding_range_single_line_no_fold() {
        let content = "interface api {}";
        let ranges = get_folding_ranges(content);
        // Single line should not create a folding range
        assert_eq!(ranges.len(), 0);
    }

    #[test]
    fn folding_range_multiple_interfaces() {
        let content = r#"interface api {
    greet: func();
}

interface internal {
    log: func(msg: string);
}"#;
        let ranges = get_folding_ranges(content);
        assert_eq!(ranges.len(), 2);
        // First interface
        assert_eq!(ranges[0].start_line, 0);
        assert_eq!(ranges[0].end_line, 2);
        // Second interface
        assert_eq!(ranges[1].start_line, 4);
        assert_eq!(ranges[1].end_line, 6);
    }

    fn get_document_symbols(content: &str) -> Vec<DocumentSymbol> {
        let store = make_store_with_doc(content);
        store
            .with_document_mut("file:///test.wit", |doc| {
                let parse = doc.parse().clone();
                let line_index = doc.line_index.clone();
                let range_to_lsp = |r: witcraft_syntax::TextRange| -> Range {
                    let start = line_index.position(r.start());
                    let end = line_index.position(r.end());
                    Range {
                        start: Position {
                            line: start.line,
                            character: start.column,
                        },
                        end: Position {
                            line: end.line,
                            character: end.column,
                        },
                    }
                };

                let mut symbols = Vec::new();
                for item in &parse.root.items {
                    match item {
                        witcraft_syntax::Item::Interface(iface) => {
                            #[allow(deprecated)]
                            symbols.push(DocumentSymbol {
                                name: iface.name.name.to_string(),
                                detail: Some("interface".to_string()),
                                kind: SymbolKind::INTERFACE,
                                tags: None,
                                deprecated: None,
                                range: range_to_lsp(iface.range),
                                selection_range: range_to_lsp(iface.name.range),
                                children: None,
                            });
                        }
                        witcraft_syntax::Item::World(world) => {
                            #[allow(deprecated)]
                            symbols.push(DocumentSymbol {
                                name: world.name.name.to_string(),
                                detail: Some("world".to_string()),
                                kind: SymbolKind::MODULE,
                                tags: None,
                                deprecated: None,
                                range: range_to_lsp(world.range),
                                selection_range: range_to_lsp(world.name.range),
                                children: None,
                            });
                        }
                        _ => {}
                    }
                }
                symbols
            })
            .unwrap_or_default()
    }

    #[test]
    fn document_symbol_interface() {
        let content = "interface api {}";
        let symbols = get_document_symbols(content);
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "api");
        assert_eq!(symbols[0].kind, SymbolKind::INTERFACE);
    }

    #[test]
    fn document_symbol_world() {
        let content = "world my-world {}";
        let symbols = get_document_symbols(content);
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "my-world");
        assert_eq!(symbols[0].kind, SymbolKind::MODULE);
    }

    #[test]
    fn document_symbol_multiple() {
        let content = r#"interface api {}
world my-world {}
interface internal {}"#;
        let symbols = get_document_symbols(content);
        assert_eq!(symbols.len(), 3);
        assert_eq!(symbols[0].name, "api");
        assert_eq!(symbols[1].name, "my-world");
        assert_eq!(symbols[2].name, "internal");
    }

    fn get_hover_at(content: &str, line: u32, character: u32) -> Option<String> {
        let store = make_store_with_doc(content);
        store
            .with_document_mut("file:///test.wit", |doc| {
                let offset =
                    doc.position_to_offset(witcraft_syntax::Position::new(line, character))?;
                let parse = doc.parse().clone();
                let index = witcraft_syntax::SymbolIndex::build(&parse.root);

                let node = witcraft_syntax::node_at(&parse.root, offset)?;

                match node {
                    witcraft_syntax::NodeRef::Interface(iface) => {
                        Some(format!("interface {}", iface.name.name))
                    }
                    witcraft_syntax::NodeRef::World(world) => {
                        Some(format!("world {}", world.name.name))
                    }
                    witcraft_syntax::NodeRef::Record(rec) => {
                        Some(format!("record {}", rec.name.name))
                    }
                    witcraft_syntax::NodeRef::Func(func) => {
                        Some(format!("func {}", func.name.name))
                    }
                    witcraft_syntax::NodeRef::NamedType(named) => {
                        // Look up the definition
                        if let Some(def) = index.find_definition(&named.name.name) {
                            Some(format!("{:?} {}", def.kind, def.name))
                        } else {
                            Some(format!("type {}", named.name.name))
                        }
                    }
                    witcraft_syntax::NodeRef::Ident(ident) => {
                        // Look up what this identifier refers to
                        if let Some(def) = index.find_definition(&ident.name) {
                            Some(format!("{:?} {}", def.kind, def.name))
                        } else {
                            Some(format!("identifier {}", ident.name))
                        }
                    }
                    _ => None,
                }
            })
            .flatten()
    }

    #[test]
    fn hover_interface_name() {
        let content = "interface api {}";
        let hover = get_hover_at(content, 0, 11); // on "api"
        assert!(hover.is_some());
        assert!(hover.unwrap().contains("api"));
    }

    #[test]
    fn hover_world_name() {
        let content = "world my-world {}";
        let hover = get_hover_at(content, 0, 8); // on "my-world"
        assert!(hover.is_some());
        assert!(hover.unwrap().contains("my-world"));
    }

    #[test]
    fn hover_record_name() {
        let content = r#"interface api {
    record user {
        id: u64,
    }
}"#;
        let hover = get_hover_at(content, 1, 12); // on "user"
        assert!(hover.is_some());
        assert!(hover.unwrap().contains("user"));
    }

    #[test]
    fn hover_type_reference() {
        let content = r#"interface api {
    record user {
        id: u64,
    }
    get-user: func() -> user;
}"#;
        let hover = get_hover_at(content, 4, 25); // on "user" reference
        assert!(hover.is_some());
    }

    fn get_completions_at(content: &str, _line: u32, _character: u32) -> Vec<String> {
        let store = make_store_with_doc(content);
        store
            .with_document_mut("file:///test.wit", |doc| {
                let parse = doc.parse().clone();
                let index = witcraft_syntax::SymbolIndex::build(&parse.root);

                let mut items = Vec::new();

                // Add defined types
                for def in index.definitions() {
                    items.push(def.name.to_string());
                }

                // Add primitive types
                for prim in PRIMITIVE_TYPES {
                    items.push(prim.to_string());
                }

                items
            })
            .unwrap_or_default()
    }

    #[test]
    fn completion_includes_primitives() {
        let content = "interface api {}";
        let completions = get_completions_at(content, 0, 0);
        assert!(completions.contains(&"string".to_string()));
        assert!(completions.contains(&"u32".to_string()));
        assert!(completions.contains(&"bool".to_string()));
    }

    #[test]
    fn completion_includes_defined_types() {
        let content = r#"interface api {
    record user {
        id: u64,
    }
}"#;
        let completions = get_completions_at(content, 0, 0);
        assert!(completions.contains(&"user".to_string()));
    }

    fn format_document(content: &str) -> String {
        let store = make_store_with_doc(content);
        store
            .with_document_mut("file:///test.wit", |doc| {
                let parse = doc.parse().clone();
                witcraft_syntax::Formatter::format(&parse.root)
            })
            .unwrap_or_default()
    }

    #[test]
    fn formatting_normalizes_whitespace() {
        let content = "interface   api   {  }";
        let formatted = format_document(content);
        // Formatter normalizes whitespace and adds newlines in braces
        assert!(formatted.contains("interface api {"));
        assert!(formatted.contains("}"));
    }

    #[test]
    fn formatting_adds_newlines() {
        let content = "interface api { greet: func(); }";
        let formatted = format_document(content);
        assert!(formatted.contains('\n'));
        assert!(formatted.contains("greet: func();"));
    }

    #[test]
    fn formatting_indents_correctly() {
        let content = "interface api {\ngreet: func();\n}";
        let formatted = format_document(content);
        // Should have proper indentation (2 spaces)
        assert!(formatted.contains("  greet: func();"));
    }

    #[test]
    fn formatting_is_idempotent() {
        let content = r#"interface api {
    record user {
        id: u64,
        name: string,
    }
    greet: func(name: string) -> string;
}"#;
        let first = format_document(content);
        let second = format_document(&first);
        assert_eq!(first, second);
    }

    fn get_diagnostics(content: &str) -> Vec<String> {
        let store = make_store_with_doc(content);
        store
            .with_document_mut("file:///test.wit", |doc| {
                let parse = doc.parse().clone();
                let index = witcraft_syntax::SymbolIndex::build(&parse.root);

                let mut diagnostics = Vec::new();

                // Parse errors
                for err in &parse.errors {
                    diagnostics.push(err.message.clone());
                }

                // Undefined types
                for reference in index.references() {
                    if index.find_definition(&reference.name).is_none() {
                        // Check if it's a builtin
                        let name = &*reference.name;
                        if !PRIMITIVE_TYPES.contains(&name) && !GENERIC_TYPES.contains(&name) {
                            diagnostics.push(format!("undefined type: {}", name));
                        }
                    }
                }

                diagnostics
            })
            .unwrap_or_default()
    }

    #[test]
    fn diagnostics_parse_error() {
        let content = "interface api {"; // missing closing brace
        let diagnostics = get_diagnostics(content);
        assert!(!diagnostics.is_empty());
    }

    #[test]
    fn diagnostics_undefined_type() {
        let content = r#"interface api {
    get-user: func() -> unknown-type;
}"#;
        let diagnostics = get_diagnostics(content);
        assert!(
            diagnostics
                .iter()
                .any(|d| d.contains("undefined") || d.contains("unknown-type"))
        );
    }

    #[test]
    fn diagnostics_valid_file() {
        let content = r#"interface api {
    greet: func(name: string) -> string;
}"#;
        let diagnostics = get_diagnostics(content);
        // Should have no diagnostics for valid file
        assert!(
            diagnostics.is_empty(),
            "Expected no diagnostics, got: {:?}",
            diagnostics
        );
    }

    /// Returns the definition location (line, column) for a symbol at the given position
    fn goto_definition_at(content: &str, line: u32, character: u32) -> Option<(u32, u32)> {
        let store = make_store_with_doc(content);
        store
            .with_document_mut("file:///test.wit", |doc| {
                let offset =
                    doc.position_to_offset(witcraft_syntax::Position::new(line, character))?;
                let parse = doc.parse().clone();
                let index = witcraft_syntax::SymbolIndex::build(&parse.root);

                let node = witcraft_syntax::node_at(&parse.root, offset)?;

                let name = match node {
                    witcraft_syntax::NodeRef::NamedType(named) => named.name.name.to_string(),
                    witcraft_syntax::NodeRef::Ident(ident) => ident.name.to_string(),
                    _ => return None,
                };

                let def = index.find_definition(&name)?;
                let pos = doc.line_index.position(def.name_range.start());
                Some((pos.line, pos.column))
            })
            .flatten()
    }

    #[test]
    fn goto_definition_type_reference() {
        let content = r#"interface api {
    record user {
        id: u64,
    }
    get-user: func() -> user;
}"#;
        // Click on "user" in the return type (line 4, around column 24)
        let def_pos = goto_definition_at(content, 4, 24);
        assert!(def_pos.is_some(), "Should find definition");
        let (line, _col) = def_pos.unwrap();
        assert_eq!(line, 1, "Definition should be on line 1 (record user)");
    }

    #[test]
    fn goto_definition_record_field_type() {
        let content = r#"interface api {
    record point {
        x: u32,
        y: u32,
    }
    record line {
        start: point,
        end: point,
    }
}"#;
        // Click on "point" in "start: point" (line 6, around column 15)
        let def_pos = goto_definition_at(content, 6, 15);
        assert!(def_pos.is_some(), "Should find definition");
        let (line, _col) = def_pos.unwrap();
        assert_eq!(line, 1, "Definition should be on line 1 (record point)");
    }

    #[test]
    fn goto_definition_variant_type() {
        let content = r#"interface api {
    record error-info {
        code: u32,
    }
    variant my-result {
        ok(string),
        err(error-info),
    }
}"#;
        // Click on "error-info" in variant case (line 6, around column 12)
        let def_pos = goto_definition_at(content, 6, 12);
        assert!(def_pos.is_some(), "Should find definition");
        let (line, _col) = def_pos.unwrap();
        assert_eq!(
            line, 1,
            "Definition should be on line 1 (record error-info)"
        );
    }

    #[test]
    fn goto_definition_not_found() {
        let content = r#"interface api {
    get-user: func() -> unknown-type;
}"#;
        // Click on "unknown-type" - should not find definition
        let def_pos = goto_definition_at(content, 1, 25);
        assert!(
            def_pos.is_none(),
            "Should not find definition for undefined type"
        );
    }

    /// Returns all reference locations (line, column) for a symbol at the given position
    fn find_references_at(content: &str, line: u32, character: u32) -> Vec<(u32, u32)> {
        let store = make_store_with_doc(content);
        store
            .with_document_mut("file:///test.wit", |doc| {
                let offset =
                    doc.position_to_offset(witcraft_syntax::Position::new(line, character))?;
                let parse = doc.parse().clone();
                let index = witcraft_syntax::SymbolIndex::build(&parse.root);

                let node = witcraft_syntax::node_at(&parse.root, offset)?;

                let name = match node {
                    witcraft_syntax::NodeRef::NamedType(named) => named.name.name.to_string(),
                    witcraft_syntax::NodeRef::Ident(ident) => ident.name.to_string(),
                    witcraft_syntax::NodeRef::Record(rec) => rec.name.name.to_string(),
                    witcraft_syntax::NodeRef::Variant(var) => var.name.name.to_string(),
                    witcraft_syntax::NodeRef::Enum(e) => e.name.name.to_string(),
                    witcraft_syntax::NodeRef::Flags(f) => f.name.name.to_string(),
                    witcraft_syntax::NodeRef::TypeAlias(alias) => alias.name.name.to_string(),
                    _ => return None,
                };

                let mut locations = Vec::new();

                // Include the definition itself
                if let Some(def) = index.find_definition(&name) {
                    let pos = doc.line_index.position(def.name_range.start());
                    locations.push((pos.line, pos.column));
                }

                // Add all references
                for reference in index.references() {
                    if &*reference.name == name {
                        let pos = doc.line_index.position(reference.range.start());
                        locations.push((pos.line, pos.column));
                    }
                }

                Some(locations)
            })
            .flatten()
            .unwrap_or_default()
    }

    #[test]
    fn references_single_usage() {
        let content = r#"interface api {
    record user {
        id: u64,
    }
    get-user: func() -> user;
}"#;
        // Find references to "user" from the definition
        let refs = find_references_at(content, 1, 11);
        assert_eq!(refs.len(), 2, "Should find definition + 1 reference");
    }

    #[test]
    fn references_multiple_usages() {
        let content = r#"interface api {
    record user {
        id: u64,
    }
    get-user: func() -> user;
    create-user: func(u: user) -> user;
    delete-user: func(u: user);
}"#;
        // Find references to "user"
        let refs = find_references_at(content, 1, 11);
        assert_eq!(refs.len(), 5, "Should find definition + 4 references");
    }

    #[test]
    fn references_no_usages() {
        let content = r#"interface api {
    record unused-type {
        id: u64,
    }
    greet: func() -> string;
}"#;
        // Find references to "unused-type"
        let refs = find_references_at(content, 1, 11);
        assert_eq!(refs.len(), 1, "Should find only the definition");
    }

    #[test]
    fn references_from_usage_site() {
        let content = r#"interface api {
    record user {
        id: u64,
    }
    get-user: func() -> user;
}"#;
        // Find references from a usage site (not the definition)
        let refs = find_references_at(content, 4, 24);
        assert_eq!(refs.len(), 2, "Should find definition + 1 reference");
    }

    /// Simulates rename and returns all edit locations (line, column)
    fn get_rename_locations(content: &str, line: u32, character: u32) -> Vec<(u32, u32)> {
        let store = make_store_with_doc(content);
        store
            .with_document_mut("file:///test.wit", |doc| {
                let offset =
                    doc.position_to_offset(witcraft_syntax::Position::new(line, character))?;
                let parse = doc.parse().clone();
                let index = witcraft_syntax::SymbolIndex::build(&parse.root);

                let node = witcraft_syntax::node_at(&parse.root, offset)?;

                let name = match node {
                    witcraft_syntax::NodeRef::NamedType(named) => named.name.name.to_string(),
                    witcraft_syntax::NodeRef::Ident(ident) => ident.name.to_string(),
                    witcraft_syntax::NodeRef::Record(rec) => rec.name.name.to_string(),
                    witcraft_syntax::NodeRef::Variant(var) => var.name.name.to_string(),
                    witcraft_syntax::NodeRef::Enum(e) => e.name.name.to_string(),
                    witcraft_syntax::NodeRef::Flags(f) => f.name.name.to_string(),
                    witcraft_syntax::NodeRef::TypeAlias(alias) => alias.name.name.to_string(),
                    witcraft_syntax::NodeRef::Func(func) => func.name.name.to_string(),
                    _ => return None,
                };

                let mut locations = Vec::new();

                // Definition location
                if let Some(def) = index.find_definition(&name) {
                    let pos = doc.line_index.position(def.name_range.start());
                    locations.push((pos.line, pos.column));
                }

                // Reference locations
                for reference in index.references() {
                    if &*reference.name == name {
                        let pos = doc.line_index.position(reference.range.start());
                        locations.push((pos.line, pos.column));
                    }
                }

                Some(locations)
            })
            .flatten()
            .unwrap_or_default()
    }

    #[test]
    fn rename_type_with_references() {
        let content = r#"interface api {
    record user {
        id: u64,
    }
    get-user: func() -> user;
    create-user: func(u: user) -> user;
}"#;
        // Rename "user" - should find all locations to rename
        let locations = get_rename_locations(content, 1, 11);
        assert_eq!(
            locations.len(),
            4,
            "Should rename definition + 3 references"
        );
    }

    #[test]
    fn rename_function() {
        let content = r#"interface api {
    greet: func(name: string) -> string;
}"#;
        // Rename "greet"
        let locations = get_rename_locations(content, 1, 6);
        assert_eq!(locations.len(), 1, "Should find function definition");
    }

    #[test]
    fn rename_enum() {
        let content = r#"interface api {
    enum status {
        pending,
        active,
    }
    get-status: func() -> status;
    set-status: func(s: status);
}"#;
        // Rename "status"
        let locations = get_rename_locations(content, 1, 9);
        assert_eq!(
            locations.len(),
            3,
            "Should rename definition + 2 references"
        );
    }

    #[test]
    fn rename_unused_type() {
        let content = r#"interface api {
    record orphan {
        id: u64,
    }
}"#;
        // Rename "orphan" - only the definition
        let locations = get_rename_locations(content, 1, 11);
        assert_eq!(locations.len(), 1, "Should only rename the definition");
    }

    #[test]
    fn signature_help_result_first_param() {
        let content = "type foo = result<";
        let help = find_signature_context(content, content.len());
        assert!(help.is_some());
        let help = help.unwrap();
        assert_eq!(help.signatures[0].label, "result<ok-type, err-type>");
        assert_eq!(help.active_parameter, Some(0));
    }

    #[test]
    fn signature_help_result_second_param() {
        let content = "type foo = result<string, ";
        let help = find_signature_context(content, content.len());
        assert!(help.is_some());
        let help = help.unwrap();
        assert_eq!(help.signatures[0].label, "result<ok-type, err-type>");
        assert_eq!(help.active_parameter, Some(1));
    }

    #[test]
    fn signature_help_option() {
        let content = "type foo = option<";
        let help = find_signature_context(content, content.len());
        assert!(help.is_some());
        let help = help.unwrap();
        assert_eq!(help.signatures[0].label, "option<inner-type>");
        assert_eq!(help.active_parameter, Some(0));
    }

    #[test]
    fn signature_help_list() {
        let content = "type foo = list<";
        let help = find_signature_context(content, content.len());
        assert!(help.is_some());
        let help = help.unwrap();
        assert_eq!(help.signatures[0].label, "list<element-type>");
    }

    #[test]
    fn signature_help_tuple() {
        let content = "type foo = tuple<string, u32, ";
        let help = find_signature_context(content, content.len());
        assert!(help.is_some());
        let help = help.unwrap();
        assert_eq!(help.signatures[0].label, "tuple<T, U, ...>");
        assert_eq!(help.active_parameter, Some(2)); // third param
    }

    #[test]
    fn signature_help_func() {
        let content = "interface api { greet: func(";
        let help = find_signature_context(content, content.len());
        assert!(help.is_some());
        let help = help.unwrap();
        assert_eq!(
            help.signatures[0].label,
            "func(name: type, ...) -> return-type"
        );
    }

    #[test]
    fn signature_help_func_second_param() {
        let content = "interface api { greet: func(name: string, ";
        let help = find_signature_context(content, content.len());
        assert!(help.is_some());
        let help = help.unwrap();
        assert_eq!(help.active_parameter, Some(1));
    }

    #[test]
    fn signature_help_constructor() {
        let content = "resource file { constructor(";
        let help = find_signature_context(content, content.len());
        assert!(help.is_some());
        let help = help.unwrap();
        assert_eq!(help.signatures[0].label, "constructor(name: type, ...)");
    }

    #[test]
    fn signature_help_borrow() {
        let content = "type handle = borrow<";
        let help = find_signature_context(content, content.len());
        assert!(help.is_some());
        let help = help.unwrap();
        assert_eq!(help.signatures[0].label, "borrow<resource-name>");
    }

    #[test]
    fn signature_help_own() {
        let content = "type handle = own<";
        let help = find_signature_context(content, content.len());
        assert!(help.is_some());
        let help = help.unwrap();
        assert_eq!(help.signatures[0].label, "own<resource-name>");
    }

    #[test]
    fn signature_help_nested_generic() {
        // Inside nested result, should detect the outer result
        let content = "type foo = result<option<string>, ";
        let help = find_signature_context(content, content.len());
        assert!(help.is_some());
        let help = help.unwrap();
        assert_eq!(help.signatures[0].label, "result<ok-type, err-type>");
        assert_eq!(help.active_parameter, Some(1)); // second param of result
    }

    #[test]
    fn signature_help_no_context() {
        let content = "interface api {";
        let help = find_signature_context(content, content.len());
        assert!(help.is_none());
    }

    #[test]
    fn signature_help_after_close() {
        let content = "type foo = result<string, error>";
        let help = find_signature_context(content, content.len());
        assert!(help.is_none()); // cursor is after closing >
    }

    fn get_inlay_hints(content: &str) -> Vec<InlayHint> {
        let store = make_store_with_doc(content);
        let workspace = Arc::new(WorkspaceManager::new());
        store
            .with_document_mut("file:///test.wit", |doc| {
                let parse = doc.parse().clone();
                collect_inlay_hints(&parse.root, &workspace, "file:///test.wit", &doc.line_index)
            })
            .unwrap_or_default()
    }

    #[test]
    fn inlay_hint_type_alias_primitive() {
        let hints = get_inlay_hints("interface api { type my-string = string; }");
        assert_eq!(hints.len(), 1);
        let label = match &hints[0].label {
            InlayHintLabel::String(s) => s.as_str(),
            _ => panic!("expected string label"),
        };
        assert_eq!(label, ": string");
    }

    #[test]
    fn inlay_hint_type_alias_list() {
        let hints = get_inlay_hints("interface api { type names = list<string>; }");
        assert_eq!(hints.len(), 1);
        let label = match &hints[0].label {
            InlayHintLabel::String(s) => s.as_str(),
            _ => panic!("expected string label"),
        };
        assert_eq!(label, ": list<string>");
    }

    #[test]
    fn inlay_hint_type_alias_result() {
        let hints = get_inlay_hints("interface api { type response = result<string, u32>; }");
        assert_eq!(hints.len(), 1);
        let label = match &hints[0].label {
            InlayHintLabel::String(s) => s.as_str(),
            _ => panic!("expected string label"),
        };
        assert_eq!(label, ": result<string, u32>");
    }

    #[test]
    fn inlay_hint_type_alias_too_long() {
        let hints = get_inlay_hints(
            "interface api { type resp = result<list<tuple<string, string, string>>, list<u32>>; }",
        );
        assert_eq!(hints.len(), 0);
    }

    #[test]
    fn inlay_hint_no_hints_for_record() {
        let hints = get_inlay_hints("interface api { record foo { x: u32, } }");
        assert_eq!(hints.len(), 0);
    }

    #[test]
    fn inlay_hint_multiple_aliases() {
        let hints =
            get_inlay_hints("interface api { type a = u32; type b = string; type c = list<u8>; }");
        assert_eq!(hints.len(), 3);
    }

    fn get_document_highlights(content: &str, line: u32, col: u32) -> Vec<DocumentHighlight> {
        let store = make_store_with_doc(content);
        store
            .with_document_mut("file:///test.wit", |doc| {
                let offset = doc.position_to_offset(witcraft_syntax::Position::new(line, col))?;
                let parse = doc.parse();
                let index = SymbolIndex::build(&parse.root);
                let node = node_at(&parse.root, offset)?;

                let name = match node {
                    NodeRef::NamedType(named) => named.name.name.to_string(),
                    NodeRef::Ident(ident) => ident.name.to_string(),
                    NodeRef::Interface(iface) => iface.name.name.to_string(),
                    NodeRef::Record(rec) => rec.name.name.to_string(),
                    NodeRef::TypeAlias(alias) => alias.name.name.to_string(),
                    _ => return None,
                };

                let mut highlights = Vec::new();

                if let Some(def) = index.find_definition(&name) {
                    highlights.push(DocumentHighlight {
                        range: doc.range_to_lsp(def.name_range),
                        kind: Some(DocumentHighlightKind::TEXT),
                    });
                }

                for reference in index.references() {
                    if &*reference.name == name {
                        highlights.push(DocumentHighlight {
                            range: doc.range_to_lsp(reference.range),
                            kind: Some(DocumentHighlightKind::READ),
                        });
                    }
                }

                Some(highlights)
            })
            .flatten()
            .unwrap_or_default()
    }

    #[test]
    fn document_highlight_type_definition() {
        let content = "interface api { record foo { x: u32, } type bar = foo; }";
        let highlights = get_document_highlights(content, 0, 23);
        assert_eq!(highlights.len(), 2);
        assert_eq!(highlights[0].kind, Some(DocumentHighlightKind::TEXT));
        assert_eq!(highlights[1].kind, Some(DocumentHighlightKind::READ));
    }

    #[test]
    fn document_highlight_from_reference() {
        let content = "interface api { record foo { x: u32, } type bar = foo; }";
        let highlights = get_document_highlights(content, 0, 50);
        assert_eq!(highlights.len(), 2);
    }

    #[test]
    fn document_highlight_no_references() {
        let content = "interface api { record foo { x: u32, } }";
        let highlights = get_document_highlights(content, 0, 23);
        assert_eq!(highlights.len(), 1);
        assert_eq!(highlights[0].kind, Some(DocumentHighlightKind::TEXT));
    }

    #[test]
    fn document_highlight_multiple_references() {
        let content = "interface api { record foo { x: u32, } type a = foo; type b = foo; }";
        let highlights = get_document_highlights(content, 0, 23);
        assert_eq!(highlights.len(), 3);
    }

    #[test]
    fn document_highlight_interface_name() {
        let content = "interface api { record foo { x: u32, } }";
        let highlights = get_document_highlights(content, 0, 10);
        assert_eq!(highlights.len(), 1);
        assert_eq!(highlights[0].kind, Some(DocumentHighlightKind::TEXT));
    }

    fn get_selection_range(content: &str, line: u32, col: u32) -> SelectionRange {
        let store = make_store_with_doc(content);
        store
            .with_document_mut("file:///test.wit", |doc| {
                let parse = doc.parse().clone();
                let offset = doc.position_to_offset(witcraft_syntax::Position::new(line, col))?;
                Some(build_selection_range(&parse.root, offset, &doc.line_index))
            })
            .flatten()
            .unwrap()
    }

    fn count_selection_levels(sel: &SelectionRange) -> usize {
        let mut count = 1;
        let mut current = sel.parent.as_ref();
        while let Some(p) = current {
            count += 1;
            current = p.parent.as_ref();
        }
        count
    }

    #[test]
    fn selection_range_record_field_name() {
        let content = "interface api { record foo { x: u32, } }";
        let sel = get_selection_range(content, 0, 28);
        assert!(count_selection_levels(&sel) >= 4);
    }

    #[test]
    fn selection_range_type_in_field() {
        let content = "interface api { record foo { x: u32, } }";
        let sel = get_selection_range(content, 0, 32);
        assert!(count_selection_levels(&sel) >= 4);
    }

    #[test]
    fn selection_range_nested_type() {
        let content = "interface api { type foo = list<string>; }";
        let sel = get_selection_range(content, 0, 32);
        assert!(count_selection_levels(&sel) >= 5);
    }

    #[test]
    fn selection_range_interface_name() {
        let content = "interface api { record foo { x: u32, } }";
        let sel = get_selection_range(content, 0, 10);
        assert!(count_selection_levels(&sel) >= 3);
    }

    #[test]
    fn selection_range_expands_outward() {
        let content = "interface api { record foo { x: u32, } }";
        let sel = get_selection_range(content, 0, 28);
        let mut current = &sel;
        let mut prev_size = 0;
        loop {
            let size = (current.range.end.character - current.range.start.character) as usize
                + (current.range.end.line - current.range.start.line) as usize * 100;
            assert!(size >= prev_size);
            prev_size = size;
            match &current.parent {
                Some(p) => current = p,
                None => break,
            }
        }
    }

    fn get_code_lenses(content: &str) -> Vec<CodeLens> {
        let store = make_store_with_doc(content);
        store
            .with_document_mut("file:///test.wit", |doc| {
                let parse = doc.parse().clone();
                let index = SymbolIndex::build(&parse.root);
                Some(collect_code_lenses(&index, &doc.line_index))
            })
            .flatten()
            .unwrap_or_default()
    }

    #[test]
    fn code_lens_shows_references() {
        let content = "interface api { record foo { x: u32, } type bar = foo; }";
        let lenses = get_code_lenses(content);
        assert_eq!(lenses.len(), 1);
        assert!(
            lenses[0]
                .command
                .as_ref()
                .unwrap()
                .title
                .contains("1 reference")
        );
    }

    #[test]
    fn code_lens_multiple_references() {
        let content = "interface api { record foo { x: u32, } type a = foo; type b = foo; }";
        let lenses = get_code_lenses(content);
        assert_eq!(lenses.len(), 1);
        assert!(
            lenses[0]
                .command
                .as_ref()
                .unwrap()
                .title
                .contains("2 references")
        );
    }

    #[test]
    fn code_lens_no_references() {
        let content = "interface api { record foo { x: u32, } }";
        let lenses = get_code_lenses(content);
        assert_eq!(lenses.len(), 0);
    }

    #[test]
    fn code_lens_multiple_definitions_with_refs() {
        let content = "interface api { record a { x: u32, } record b { y: a, } type c = a; }";
        let lenses = get_code_lenses(content);
        assert_eq!(lenses.len(), 1);
        assert!(
            lenses[0]
                .command
                .as_ref()
                .unwrap()
                .title
                .contains("2 references")
        );
    }

    fn unique_test_dir(name: &str) -> PathBuf {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        let dir = std::env::temp_dir().join(format!("wit-lsp-{name}-{nanos}"));
        fs::create_dir_all(&dir).unwrap();
        dir
    }

    fn file_uri(path: &Path) -> String {
        Url::from_file_path(path).unwrap().to_string()
    }

    fn folder_uri(path: &Path) -> String {
        Url::from_directory_path(path).unwrap().to_string()
    }

    fn write_file(path: &Path, content: &str) {
        fs::write(path, content).unwrap();
    }

    async fn init_service() -> (LspService<WitLanguageServer>, ClientSocket) {
        let (mut service, socket) = LspService::new(WitLanguageServer::new);
        let initialize = Request::build("initialize")
            .params(json!({ "capabilities": {} }))
            .id(1)
            .finish();
        let response = service
            .ready()
            .await
            .unwrap()
            .call(initialize)
            .await
            .unwrap();
        assert!(response.is_some());
        (service, socket)
    }

    async fn init_service_with_folders(
        workspace_folders: Vec<WorkspaceFolder>,
    ) -> (LspService<WitLanguageServer>, ClientSocket) {
        let (mut service, socket) = LspService::new(WitLanguageServer::new);
        let initialize = Request::build("initialize")
            .params(json!({
                "capabilities": {},
                "workspaceFolders": workspace_folders
            }))
            .id(1)
            .finish();
        let response = service
            .ready()
            .await
            .unwrap()
            .call(initialize)
            .await
            .unwrap();
        assert!(response.is_some());
        (service, socket)
    }

    async fn send_notification(
        service: &mut LspService<WitLanguageServer>,
        method: &'static str,
        params: serde_json::Value,
    ) {
        let req = Request::build(method).params(params).finish();
        let response = service.ready().await.unwrap().call(req).await.unwrap();
        assert!(response.is_none());
    }

    async fn send_request(
        service: &mut LspService<WitLanguageServer>,
        id: i64,
        method: &'static str,
        params: serde_json::Value,
    ) -> serde_json::Value {
        let req = Request::build(method).params(params).id(id).finish();
        let response = with_test_timeout(
            async { service.ready().await.unwrap().call(req).await.unwrap() },
            format!("waiting for response to `{method}`"),
        )
        .await
        .expect("request should return a response");
        response.result().cloned().expect("request should succeed")
    }

    async fn send_raw_request(
        service: &mut LspService<WitLanguageServer>,
        id: i64,
        method: &'static str,
        params: serde_json::Value,
    ) -> Option<Response> {
        let req = Request::build(method).params(params).id(id).finish();
        service.ready().await.unwrap().call(req).await.unwrap()
    }

    async fn wait_for_diagnostics(
        socket: &mut ClientSocket,
        uri: &str,
    ) -> Option<PublishDiagnosticsParams> {
        with_test_timeout(
            async {
                loop {
                    let req = std::future::poll_fn(|cx| {
                        futures_core::stream::Stream::poll_next(
                            std::pin::Pin::new(&mut *socket),
                            cx,
                        )
                    })
                    .await?;
                    if req.method() != "textDocument/publishDiagnostics" {
                        continue;
                    }
                    let params: PublishDiagnosticsParams =
                        serde_json::from_value(req.params()?.clone()).ok()?;
                    if params.uri.to_string() == uri {
                        return Some(params);
                    }
                }
            },
            format!("waiting for diagnostics for `{uri}`"),
        )
        .await
    }

    fn apply_text_edits(content: &str, edits: &[TextEdit]) -> String {
        let line_index = witcraft_syntax::LineIndex::new(content);
        let mut edits_with_offsets: Vec<(u32, u32, String)> = edits
            .iter()
            .map(|edit| {
                let start = line_index
                    .offset(witcraft_syntax::Position::new(
                        edit.range.start.line,
                        edit.range.start.character,
                    ))
                    .expect("invalid edit start position");
                let end = line_index
                    .offset(witcraft_syntax::Position::new(
                        edit.range.end.line,
                        edit.range.end.character,
                    ))
                    .expect("invalid edit end position");
                (start, end, edit.new_text.clone())
            })
            .collect();
        edits_with_offsets.sort_by(|a, b| b.0.cmp(&a.0));

        let mut result = content.to_string();
        for (start, end, new_text) in edits_with_offsets {
            result.replace_range(start as usize..end as usize, &new_text);
        }
        result
    }

    #[tokio::test(flavor = "current_thread")]
    async fn regression_goto_definition_after_close_uses_disk_index() {
        let root = unique_test_dir("goto-after-close");
        let types_path = root.join("types.wit");
        let api_path = root.join("api.wit");

        let types_content = r#"interface types {
    record user {
        id: u64,
    }
}"#;
        let api_content = r#"interface api {
    use types.{user};
    get-user: func() -> user;
}"#;

        write_file(&types_path, types_content);
        write_file(&api_path, api_content);

        let types_uri = file_uri(&types_path);
        let api_uri = file_uri(&api_path);

        let (mut service, _) = init_service().await;

        send_notification(
            &mut service,
            "textDocument/didOpen",
            json!({
                "textDocument": {
                    "uri": types_uri,
                    "languageId": "wit",
                    "version": 1,
                    "text": types_content
                }
            }),
        )
        .await;

        send_notification(
            &mut service,
            "textDocument/didOpen",
            json!({
                "textDocument": {
                    "uri": api_uri,
                    "languageId": "wit",
                    "version": 1,
                    "text": api_content
                }
            }),
        )
        .await;

        send_notification(
            &mut service,
            "textDocument/didClose",
            json!({ "textDocument": { "uri": types_uri } }),
        )
        .await;

        let result = send_request(
            &mut service,
            2,
            "textDocument/definition",
            json!({
                "textDocument": { "uri": api_uri },
                "position": { "line": 2, "character": 24 }
            }),
        )
        .await;

        let location: Location = serde_json::from_value(result).unwrap();
        assert_eq!(location.uri.to_string(), types_uri);
        assert_eq!(location.range.start.line, 1);

        fs::remove_dir_all(root).ok();
    }

    #[tokio::test(flavor = "current_thread")]
    async fn regression_references_include_unopened_sibling_files() {
        let root = unique_test_dir("references-unopened");
        let types_path = root.join("types.wit");
        let api_path = root.join("api.wit");

        let types_content = r#"interface types {
    record user {
        id: u64,
    }
}"#;
        let api_content = r#"interface api {
    use types.{user};
    get-user: func() -> user;
}"#;

        write_file(&types_path, types_content);
        write_file(&api_path, api_content);

        let types_uri = file_uri(&types_path);
        let api_uri = file_uri(&api_path);

        let (mut service, _) = init_service().await;

        // Open only api; sibling indexing should discover types from disk.
        send_notification(
            &mut service,
            "textDocument/didOpen",
            json!({
                "textDocument": {
                    "uri": api_uri,
                    "languageId": "wit",
                    "version": 1,
                    "text": api_content
                }
            }),
        )
        .await;

        let result = send_request(
            &mut service,
            3,
            "textDocument/references",
            json!({
                "textDocument": { "uri": api_uri },
                "position": { "line": 2, "character": 24 },
                "context": { "includeDeclaration": true }
            }),
        )
        .await;

        let locations: Vec<Location> = serde_json::from_value(result).unwrap();
        assert!(
            locations.iter().any(|loc| loc.uri.to_string() == types_uri),
            "references should include definition from unopened sibling file"
        );

        fs::remove_dir_all(root).ok();
    }

    #[tokio::test(flavor = "current_thread")]
    async fn regression_rename_updates_unopened_sibling_files() {
        let root = unique_test_dir("rename-unopened");
        let types_path = root.join("types.wit");
        let api_path = root.join("api.wit");

        let types_content = r#"interface types {
    record user {
        id: u64,
    }
}"#;
        let api_content = r#"interface api {
    use types.{user};
    get-user: func() -> user;
}"#;

        write_file(&types_path, types_content);
        write_file(&api_path, api_content);

        let types_uri = file_uri(&types_path);
        let api_uri = file_uri(&api_path);

        let (mut service, _) = init_service().await;

        // Open only api; sibling indexing should discover types from disk.
        send_notification(
            &mut service,
            "textDocument/didOpen",
            json!({
                "textDocument": {
                    "uri": api_uri,
                    "languageId": "wit",
                    "version": 1,
                    "text": api_content
                }
            }),
        )
        .await;

        let result = send_request(
            &mut service,
            4,
            "textDocument/rename",
            json!({
                "textDocument": { "uri": api_uri },
                "position": { "line": 2, "character": 24 },
                "newName": "account"
            }),
        )
        .await;

        let edit: WorkspaceEdit = serde_json::from_value(result).unwrap();
        let changes = edit.changes.unwrap_or_default();

        assert!(changes.contains_key(&Url::parse(&api_uri).unwrap()));
        assert!(
            changes.contains_key(&Url::parse(&types_uri).unwrap()),
            "rename should include edits for unopened sibling file"
        );

        fs::remove_dir_all(root).ok();
    }

    #[tokio::test(flavor = "current_thread")]
    async fn regression_ambiguous_import_publishes_diagnostic() {
        let root = unique_test_dir("ambiguous-diagnostic");
        let types_a_path = root.join("types-a.wit");
        let types_b_path = root.join("types-b.wit");
        let api_path = root.join("api.wit");

        let types_a_content = r#"interface types {
    record user {
        id: u64,
    }
}"#;
        let types_b_content = r#"interface types {
    record user {
        id: u64,
    }
}"#;
        let api_content = r#"interface api {
    use types.{user};
    get-user: func() -> user;
}"#;

        write_file(&types_a_path, types_a_content);
        write_file(&types_b_path, types_b_content);
        write_file(&api_path, api_content);

        let api_uri = file_uri(&api_path);
        let (mut service, mut socket) = init_service().await;

        send_notification(
            &mut service,
            "textDocument/didOpen",
            json!({
                "textDocument": {
                    "uri": api_uri,
                    "languageId": "wit",
                    "version": 1,
                    "text": api_content
                }
            }),
        )
        .await;

        let diagnostics = wait_for_diagnostics(&mut socket, &api_uri)
            .await
            .expect("expected diagnostics for opened file");

        assert!(
            diagnostics
                .diagnostics
                .iter()
                .any(|d| d.message.contains("ambiguous type `user`")),
            "expected ambiguous import diagnostic"
        );

        fs::remove_dir_all(root).ok();
    }

    #[tokio::test(flavor = "current_thread")]
    async fn regression_ambiguous_import_returns_code_actions() {
        let root = unique_test_dir("ambiguous-code-actions");
        let types_a_path = root.join("types-a.wit");
        let types_b_path = root.join("types-b.wit");
        let api_path = root.join("api.wit");

        let types_a_content = r#"interface types {
    record user {
        id: u64,
    }
}"#;
        let types_b_content = r#"interface types {
    record user {
        id: u64,
    }
}"#;
        let api_content = r#"interface api {
    use types.{user};
    get-user: func() -> user;
}"#;

        write_file(&types_a_path, types_a_content);
        write_file(&types_b_path, types_b_content);
        write_file(&api_path, api_content);

        let api_uri = file_uri(&api_path);
        let (mut service, mut socket) = init_service().await;

        send_notification(
            &mut service,
            "textDocument/didOpen",
            json!({
                "textDocument": {
                    "uri": api_uri,
                    "languageId": "wit",
                    "version": 1,
                    "text": api_content
                }
            }),
        )
        .await;

        let diagnostics = wait_for_diagnostics(&mut socket, &api_uri)
            .await
            .expect("expected diagnostics for opened file");
        let diag = diagnostics
            .diagnostics
            .iter()
            .find(|d| d.message.contains("ambiguous type `user`"))
            .expect("expected ambiguous import diagnostic");

        let result = send_request(
            &mut service,
            5,
            "textDocument/codeAction",
            json!({
                "textDocument": { "uri": api_uri },
                "range": diag.range,
                "context": {
                    "diagnostics": [diag],
                    "only": ["quickfix"]
                }
            }),
        )
        .await;

        let actions: CodeActionResponse = serde_json::from_value(result).unwrap_or_default();
        assert!(
            actions
                .iter()
                .any(|action| matches!(action, CodeActionOrCommand::CodeAction(_))),
            "expected ambiguity code actions"
        );

        fs::remove_dir_all(root).ok();
    }

    #[tokio::test(flavor = "current_thread")]
    async fn regression_ambiguous_import_code_action_clears_diagnostic() {
        let root = unique_test_dir("ambiguous-code-action-apply");
        let types_a_path = root.join("types-a.wit");
        let types_b_path = root.join("types-b.wit");
        let api_path = root.join("api.wit");

        let types_a_content = r#"interface types {
    record user {
        id: u64,
    }
}"#;
        let types_b_content = r#"interface types {
    record user {
        id: u64,
    }
}"#;
        let api_content = r#"interface api {
    use types.{user};
    get-user: func() -> user;
}"#;

        write_file(&types_a_path, types_a_content);
        write_file(&types_b_path, types_b_content);
        write_file(&api_path, api_content);

        let api_uri = file_uri(&api_path);
        let (mut service, mut socket) = init_service().await;

        send_notification(
            &mut service,
            "textDocument/didOpen",
            json!({
                "textDocument": {
                    "uri": api_uri,
                    "languageId": "wit",
                    "version": 1,
                    "text": api_content
                }
            }),
        )
        .await;

        let diagnostics = wait_for_diagnostics(&mut socket, &api_uri)
            .await
            .expect("expected diagnostics for opened file");
        let diag = diagnostics
            .diagnostics
            .iter()
            .find(|d| d.message.contains("ambiguous type `user`"))
            .expect("expected ambiguous import diagnostic");

        let result = send_request(
            &mut service,
            6,
            "textDocument/codeAction",
            json!({
                "textDocument": { "uri": api_uri },
                "range": diag.range,
                "context": {
                    "diagnostics": [diag],
                    "only": ["quickfix"]
                }
            }),
        )
        .await;

        let actions: CodeActionResponse = serde_json::from_value(result).unwrap_or_default();
        let action = actions
            .iter()
            .find_map(|action| match action {
                CodeActionOrCommand::CodeAction(action) => {
                    action.edit.as_ref().map(|edit| (action, edit))
                }
                _ => None,
            })
            .expect("expected code action with edit");

        let mut contents: HashMap<String, String> = HashMap::new();
        contents.insert(api_uri.clone(), api_content.to_string());
        contents.insert(file_uri(&types_a_path), types_a_content.to_string());
        contents.insert(file_uri(&types_b_path), types_b_content.to_string());

        if let Some(changes) = &action.1.changes {
            for (uri, edits) in changes {
                if let Some(content) = contents.get(&uri.to_string()) {
                    let updated = apply_text_edits(content, edits);
                    contents.insert(uri.to_string(), updated);
                }
            }
        }

        let updated_api = contents
            .get(&api_uri)
            .expect("missing updated api content")
            .to_string();
        let updated_types_a = contents
            .get(&file_uri(&types_a_path))
            .expect("missing updated types-a content")
            .to_string();
        let updated_types_b = contents
            .get(&file_uri(&types_b_path))
            .expect("missing updated types-b content")
            .to_string();

        write_file(&api_path, &updated_api);
        write_file(&types_a_path, &updated_types_a);
        write_file(&types_b_path, &updated_types_b);

        let mut changed_files = Vec::new();
        if updated_types_a != types_a_content {
            changed_files.push(file_uri(&types_a_path));
        }
        if updated_types_b != types_b_content {
            changed_files.push(file_uri(&types_b_path));
        }

        if !changed_files.is_empty() {
            send_notification(
                &mut service,
                "workspace/didChangeWatchedFiles",
                json!({
                    "changes": changed_files
                        .iter()
                        .map(|uri| json!({ "uri": uri, "type": 2 }))
                        .collect::<Vec<_>>()
                }),
            )
            .await;
        }

        send_notification(
            &mut service,
            "textDocument/didChange",
            json!({
                "textDocument": { "uri": api_uri, "version": 2 },
                "contentChanges": [{ "text": updated_api }]
            }),
        )
        .await;

        let updated_diagnostics = wait_for_diagnostics(&mut socket, &api_uri)
            .await
            .expect("expected diagnostics for updated file");
        assert!(
            updated_diagnostics
                .diagnostics
                .iter()
                .all(|d| !d.message.contains("ambiguous type `user`")),
            "expected ambiguity to be resolved by code action"
        );

        fs::remove_dir_all(root).ok();
    }

    #[tokio::test(flavor = "current_thread")]
    async fn regression_remove_unused_import_keeps_use_syntax_valid() {
        let root = unique_test_dir("unused-import-cleanup");
        let api_path = root.join("api.wit");

        let api_content = r#"interface api {
    use types.{foo, bar};
    type x = foo;
}"#;
        write_file(&api_path, api_content);
        let api_uri = file_uri(&api_path);

        let (mut service, mut socket) = init_service().await;
        send_notification(
            &mut service,
            "textDocument/didOpen",
            json!({
                "textDocument": {
                    "uri": api_uri,
                    "languageId": "wit",
                    "version": 1,
                    "text": api_content
                }
            }),
        )
        .await;

        let diagnostics = wait_for_diagnostics(&mut socket, &api_uri)
            .await
            .expect("expected diagnostics for opened file");
        let diag = diagnostics
            .diagnostics
            .iter()
            .find(|d| d.message.contains("unused import `bar`"))
            .expect("expected unused import diagnostic");

        let result = send_request(
            &mut service,
            21,
            "textDocument/codeAction",
            json!({
                "textDocument": { "uri": api_uri },
                "range": diag.range,
                "context": {
                    "diagnostics": [diag],
                    "only": ["quickfix"]
                }
            }),
        )
        .await;
        let actions: CodeActionResponse = serde_json::from_value(result).unwrap_or_default();
        let edit = actions
            .iter()
            .find_map(|action| match action {
                CodeActionOrCommand::CodeAction(action)
                    if action.title.contains("Remove unused import `bar`") =>
                {
                    action.edit.as_ref()
                }
                _ => None,
            })
            .expect("expected remove import code action");
        let updated = apply_text_edits(
            api_content,
            edit.changes
                .as_ref()
                .and_then(|changes| changes.get(&Url::parse(&api_uri).unwrap()))
                .expect("expected edits for api file"),
        );

        assert!(
            updated.contains("use types.{foo};"),
            "expected import list to stay syntactically valid"
        );
        assert!(
            !updated.contains("foo, }"),
            "should not leave dangling comma in use list"
        );

        fs::remove_dir_all(root).ok();
    }

    #[tokio::test(flavor = "current_thread")]
    async fn regression_add_missing_import_targets_current_scope() {
        let root = unique_test_dir("add-import-scope");
        let api_path = root.join("api.wit");
        let types_path = root.join("types.wit");
        let api_content = r#"interface first {
    type t = u32;
}

interface api {
    get-user: func() -> user;
}"#;
        let types_content = r#"interface types {
    record user {
        id: u64,
    }
}"#;
        write_file(&api_path, api_content);
        write_file(&types_path, types_content);
        let api_uri = file_uri(&api_path);

        let (mut service, mut socket) = init_service().await;
        send_notification(
            &mut service,
            "textDocument/didOpen",
            json!({
                "textDocument": {
                    "uri": api_uri,
                    "languageId": "wit",
                    "version": 1,
                    "text": api_content
                }
            }),
        )
        .await;

        let diagnostics = wait_for_diagnostics(&mut socket, &api_uri)
            .await
            .expect("expected diagnostics for opened file");
        let diag = diagnostics
            .diagnostics
            .iter()
            .find(|d| d.message.contains("undefined type `user`"))
            .expect("expected undefined type diagnostic");

        let result = send_request(
            &mut service,
            22,
            "textDocument/codeAction",
            json!({
                "textDocument": { "uri": api_uri },
                "range": diag.range,
                "context": {
                    "diagnostics": [diag],
                    "only": ["quickfix"]
                }
            }),
        )
        .await;
        let actions: CodeActionResponse = serde_json::from_value(result).unwrap_or_default();
        let edit = actions
            .iter()
            .find_map(|action| match action {
                CodeActionOrCommand::CodeAction(action)
                    if action.title.contains("Import `user` from `types`") =>
                {
                    action.edit.as_ref()
                }
                _ => None,
            })
            .expect("expected add import code action");
        let updated = apply_text_edits(
            api_content,
            edit.changes
                .as_ref()
                .and_then(|changes| changes.get(&Url::parse(&api_uri).unwrap()))
                .expect("expected edits for api file"),
        );

        assert!(
            updated
                .contains("interface api {\n    use types.{user};\n    get-user: func() -> user;"),
            "import should be inserted inside the interface that has the undefined type; updated:\n{}",
            updated
        );
        assert!(
            !updated.contains("interface first {\n    use types.{user};"),
            "import must not be inserted into unrelated first scope; updated:\n{}",
            updated
        );

        fs::remove_dir_all(root).ok();
    }

    #[tokio::test(flavor = "current_thread")]
    async fn regression_rename_homonyms_only_updates_selected_definition() {
        let root = unique_test_dir("rename-homonyms");
        let types_a_path = root.join("types-a.wit");
        let types_b_path = root.join("types-b.wit");
        let api_path = root.join("api.wit");

        let types_a_content = r#"interface a {
    record user {
        id: u64,
    }
}"#;
        let types_b_content = r#"interface b {
    record user {
        id: u64,
    }
}"#;
        let api_content = r#"interface api {
    use a.{user as a-user};
    use b.{user as b-user};
    take-a: func(x: a-user);
    take-b: func(x: b-user);
}"#;
        write_file(&types_a_path, types_a_content);
        write_file(&types_b_path, types_b_content);
        write_file(&api_path, api_content);

        let types_a_uri = file_uri(&types_a_path);
        let api_uri = file_uri(&api_path);

        let (mut service, _) = init_service().await;
        send_notification(
            &mut service,
            "textDocument/didOpen",
            json!({
                "textDocument": {
                    "uri": types_a_uri,
                    "languageId": "wit",
                    "version": 1,
                    "text": types_a_content
                }
            }),
        )
        .await;
        send_notification(
            &mut service,
            "textDocument/didOpen",
            json!({
                "textDocument": {
                    "uri": api_uri,
                    "languageId": "wit",
                    "version": 1,
                    "text": api_content
                }
            }),
        )
        .await;

        let result = send_request(
            &mut service,
            23,
            "textDocument/rename",
            json!({
                "textDocument": { "uri": types_a_uri },
                "position": { "line": 1, "character": 12 },
                "newName": "account"
            }),
        )
        .await;
        let edit: WorkspaceEdit = serde_json::from_value(result).unwrap();
        let changes = edit.changes.unwrap_or_default();

        let updated_api = apply_text_edits(
            api_content,
            changes
                .get(&Url::parse(&api_uri).unwrap())
                .expect("expected api edits"),
        );
        assert!(
            updated_api.contains("use a.{account as a-user};"),
            "rename should update import that resolves to selected definition"
        );
        assert!(
            updated_api.contains("use b.{user as b-user};"),
            "rename should not touch homonym import from different definition"
        );

        fs::remove_dir_all(root).ok();
    }

    #[tokio::test(flavor = "current_thread")]
    async fn regression_workspace_symbol_includes_unopened_sibling_files() {
        let root = unique_test_dir("workspace-symbol-unopened");
        let types_path = root.join("types.wit");
        let api_path = root.join("api.wit");

        let types_content = r#"interface types {
    record user {
        id: u64,
    }
}"#;
        let api_content = r#"interface api {
    use types.{user};
    get-user: func() -> user;
}"#;

        write_file(&types_path, types_content);
        write_file(&api_path, api_content);

        let types_uri = file_uri(&types_path);
        let api_uri = file_uri(&api_path);

        let (mut service, _) = init_service().await;

        // Open only api; sibling indexing should discover types from disk.
        send_notification(
            &mut service,
            "textDocument/didOpen",
            json!({
                "textDocument": {
                    "uri": api_uri,
                    "languageId": "wit",
                    "version": 1,
                    "text": api_content
                }
            }),
        )
        .await;

        let result = send_request(
            &mut service,
            6,
            "workspace/symbol",
            json!({ "query": "user" }),
        )
        .await;

        let symbols: Vec<SymbolInformation> = serde_json::from_value(result).unwrap();
        assert!(
            symbols
                .iter()
                .any(|symbol| symbol.name == "user" && symbol.location.uri.to_string() == types_uri),
            "workspace/symbol should include symbols from unopened sibling files"
        );

        fs::remove_dir_all(root).ok();
    }

    #[tokio::test(flavor = "current_thread")]
    async fn regression_workspace_symbol_removes_deleted_file_symbols() {
        let root = unique_test_dir("workspace-symbol-delete");
        let types_path = root.join("types.wit");
        let api_path = root.join("api.wit");

        let types_content = r#"interface types {
    record user {
        id: u64,
    }
}"#;
        let api_content = r#"interface api {
    use types.{user};
    get-user: func() -> user;
}"#;

        write_file(&types_path, types_content);
        write_file(&api_path, api_content);

        let types_uri = file_uri(&types_path);
        let api_uri = file_uri(&api_path);

        let (mut service, _) = init_service().await;

        send_notification(
            &mut service,
            "textDocument/didOpen",
            json!({
                "textDocument": {
                    "uri": api_uri,
                    "languageId": "wit",
                    "version": 1,
                    "text": api_content
                }
            }),
        )
        .await;

        let before = send_request(
            &mut service,
            7,
            "workspace/symbol",
            json!({ "query": "user" }),
        )
        .await;
        let before_symbols: Vec<SymbolInformation> = serde_json::from_value(before).unwrap();
        assert!(
            before_symbols
                .iter()
                .any(|symbol| symbol.location.uri.to_string() == types_uri),
            "precondition failed: expected symbol from types.wit before deletion"
        );

        fs::remove_file(&types_path).unwrap();
        send_notification(
            &mut service,
            "workspace/didChangeWatchedFiles",
            json!({
                "changes": [
                    { "uri": types_uri, "type": 3 }
                ]
            }),
        )
        .await;

        let after = send_request(
            &mut service,
            8,
            "workspace/symbol",
            json!({ "query": "user" }),
        )
        .await;
        let after_symbols: Vec<SymbolInformation> = serde_json::from_value(after).unwrap();
        assert!(
            after_symbols
                .iter()
                .all(|symbol| symbol.location.uri.to_string() != types_uri),
            "workspace/symbol should not keep stale symbols after delete event"
        );

        fs::remove_dir_all(root).ok();
    }

    #[tokio::test(flavor = "current_thread")]
    async fn regression_non_file_uri_requests_return_none_without_crash() {
        let (mut service, _) = init_service().await;
        let uri = "untitled:in-memory.wit";

        let def = send_request(
            &mut service,
            9,
            "textDocument/definition",
            json!({
                "textDocument": { "uri": uri },
                "position": { "line": 0, "character": 0 }
            }),
        )
        .await;
        assert_eq!(def, serde_json::Value::Null);

        let refs = send_request(
            &mut service,
            10,
            "textDocument/references",
            json!({
                "textDocument": { "uri": uri },
                "position": { "line": 0, "character": 0 },
                "context": { "includeDeclaration": true }
            }),
        )
        .await;
        assert_eq!(refs, serde_json::Value::Null);

        let rename = send_request(
            &mut service,
            11,
            "textDocument/rename",
            json!({
                "textDocument": { "uri": uri },
                "position": { "line": 0, "character": 0 },
                "newName": "renamed"
            }),
        )
        .await;
        assert_eq!(rename, serde_json::Value::Null);
    }

    #[tokio::test(flavor = "current_thread")]
    async fn regression_malformed_uri_payload_returns_protocol_error() {
        let (mut service, _) = init_service().await;

        let response = send_raw_request(
            &mut service,
            12,
            "textDocument/definition",
            json!({
                "textDocument": { "uri": "not a uri" },
                "position": { "line": 0, "character": 0 }
            }),
        )
        .await
        .expect("request should return a response");

        assert!(
            response.error().is_some(),
            "malformed URI should produce a JSON-RPC error response"
        );
    }

    #[tokio::test(flavor = "current_thread")]
    async fn regression_invalid_positions_return_none() {
        let root = unique_test_dir("invalid-positions");
        let api_path = root.join("api.wit");
        let api_content = r#"interface api {
    record user {
        id: u64,
    }
    get-user: func() -> user;
}"#;

        write_file(&api_path, api_content);

        let api_uri = file_uri(&api_path);
        let (mut service, _) = init_service().await;

        send_notification(
            &mut service,
            "textDocument/didOpen",
            json!({
                "textDocument": {
                    "uri": api_uri,
                    "languageId": "wit",
                    "version": 1,
                    "text": api_content
                }
            }),
        )
        .await;

        let invalid_position = json!({ "line": 999, "character": 999 });

        let def = send_request(
            &mut service,
            13,
            "textDocument/definition",
            json!({
                "textDocument": { "uri": api_uri },
                "position": invalid_position
            }),
        )
        .await;
        assert_eq!(def, serde_json::Value::Null);

        let refs = send_request(
            &mut service,
            14,
            "textDocument/references",
            json!({
                "textDocument": { "uri": api_uri },
                "position": invalid_position,
                "context": { "includeDeclaration": true }
            }),
        )
        .await;
        assert_eq!(refs, serde_json::Value::Null);

        let hover = send_request(
            &mut service,
            15,
            "textDocument/hover",
            json!({
                "textDocument": { "uri": api_uri },
                "position": invalid_position
            }),
        )
        .await;
        assert_eq!(hover, serde_json::Value::Null);

        let rename = send_request(
            &mut service,
            16,
            "textDocument/rename",
            json!({
                "textDocument": { "uri": api_uri },
                "position": invalid_position,
                "newName": "renamed"
            }),
        )
        .await;
        assert_eq!(rename, serde_json::Value::Null);

        fs::remove_dir_all(root).ok();
    }

    #[tokio::test(flavor = "current_thread")]
    async fn regression_file_churn_create_change_delete() {
        let root = unique_test_dir("file-churn-create-change-delete");
        let api_path = root.join("api.wit");
        let types_path = root.join("types.wit");

        let api_content = r#"interface api {
    use types.{user};
    get-user: func() -> user;
}"#;
        let types_content = r#"interface types {
    record user {
        id: u64,
    }
}"#;

        write_file(&api_path, api_content);

        let api_uri = file_uri(&api_path);
        let types_uri = file_uri(&types_path);
        let (mut service, _) = init_service().await;

        send_notification(
            &mut service,
            "textDocument/didOpen",
            json!({
                "textDocument": {
                    "uri": api_uri,
                    "languageId": "wit",
                    "version": 1,
                    "text": api_content
                }
            }),
        )
        .await;

        let initial_def = send_request(
            &mut service,
            17,
            "textDocument/definition",
            json!({
                "textDocument": { "uri": api_uri },
                "position": { "line": 2, "character": 24 }
            }),
        )
        .await;
        assert_eq!(initial_def, serde_json::Value::Null);

        write_file(&types_path, types_content);
        send_notification(
            &mut service,
            "workspace/didChangeWatchedFiles",
            json!({
                "changes": [
                    { "uri": types_uri, "type": 1 }
                ]
            }),
        )
        .await;

        send_notification(
            &mut service,
            "textDocument/didChange",
            json!({
                "textDocument": { "uri": api_uri, "version": 2 },
                "contentChanges": [{ "text": api_content }]
            }),
        )
        .await;

        let definition = send_request(
            &mut service,
            18,
            "textDocument/definition",
            json!({
                "textDocument": { "uri": api_uri },
                "position": { "line": 2, "character": 24 }
            }),
        )
        .await;
        let location: Location = serde_json::from_value(definition).unwrap();
        assert_eq!(location.uri.to_string(), types_uri);

        let types_changed = r#"interface types {
    record account {
        id: u64,
    }
}"#;
        write_file(&types_path, types_changed);
        send_notification(
            &mut service,
            "workspace/didChangeWatchedFiles",
            json!({
                "changes": [
                    { "uri": types_uri, "type": 2 }
                ]
            }),
        )
        .await;

        send_notification(
            &mut service,
            "textDocument/didChange",
            json!({
                "textDocument": { "uri": api_uri, "version": 3 },
                "contentChanges": [{ "text": api_content }]
            }),
        )
        .await;

        let def_after_change = send_request(
            &mut service,
            19,
            "textDocument/definition",
            json!({
                "textDocument": { "uri": api_uri },
                "position": { "line": 2, "character": 24 }
            }),
        )
        .await;
        assert_eq!(def_after_change, serde_json::Value::Null);

        fs::remove_file(&types_path).ok();
        send_notification(
            &mut service,
            "workspace/didChangeWatchedFiles",
            json!({
                "changes": [
                    { "uri": types_uri, "type": 3 }
                ]
            }),
        )
        .await;

        send_notification(
            &mut service,
            "textDocument/didChange",
            json!({
                "textDocument": { "uri": api_uri, "version": 4 },
                "contentChanges": [{ "text": api_content }]
            }),
        )
        .await;

        let def_after_delete = send_request(
            &mut service,
            20,
            "textDocument/definition",
            json!({
                "textDocument": { "uri": api_uri },
                "position": { "line": 2, "character": 24 }
            }),
        )
        .await;
        assert_eq!(def_after_delete, serde_json::Value::Null);

        fs::remove_dir_all(root).ok();
    }

    #[tokio::test(flavor = "current_thread")]
    async fn regression_file_churn_rename_updates_targets() {
        let root = unique_test_dir("file-churn-rename");
        let api_path = root.join("api.wit");
        let types_path = root.join("types.wit");
        let models_path = root.join("models.wit");

        let api_content = r#"interface api {
    use types.{user};
    get-user: func() -> user;
}"#;
        let types_content = r#"interface types {
    record user {
        id: u64,
    }
}"#;

        write_file(&api_path, api_content);
        write_file(&types_path, types_content);

        let api_uri = file_uri(&api_path);
        let types_uri = file_uri(&types_path);
        let models_uri = file_uri(&models_path);
        let (mut service, _) = init_service().await;

        send_notification(
            &mut service,
            "textDocument/didOpen",
            json!({
                "textDocument": {
                    "uri": api_uri,
                    "languageId": "wit",
                    "version": 1,
                    "text": api_content
                }
            }),
        )
        .await;

        send_notification(
            &mut service,
            "workspace/didChangeWatchedFiles",
            json!({
                "changes": [
                    { "uri": types_uri, "type": 1 }
                ]
            }),
        )
        .await;

        let definition = send_request(
            &mut service,
            19,
            "textDocument/definition",
            json!({
                "textDocument": { "uri": api_uri },
                "position": { "line": 2, "character": 24 }
            }),
        )
        .await;
        let location: Location = serde_json::from_value(definition).unwrap();
        assert_eq!(location.uri.to_string(), types_uri);

        fs::rename(&types_path, &models_path).unwrap();
        send_notification(
            &mut service,
            "workspace/didChangeWatchedFiles",
            json!({
                "changes": [
                    { "uri": types_uri, "type": 3 },
                    { "uri": models_uri, "type": 1 }
                ]
            }),
        )
        .await;

        let definition_after = send_request(
            &mut service,
            20,
            "textDocument/definition",
            json!({
                "textDocument": { "uri": api_uri },
                "position": { "line": 2, "character": 24 }
            }),
        )
        .await;
        let location_after: Location = serde_json::from_value(definition_after).unwrap();
        assert_eq!(location_after.uri.to_string(), models_uri);

        fs::remove_dir_all(root).ok();
    }

    #[tokio::test(flavor = "current_thread")]
    async fn regression_multi_root_workspace_symbol_and_boundaries() {
        let root_a = unique_test_dir("multi-root-a");
        let root_b = unique_test_dir("multi-root-b");

        let api_path = root_a.join("api.wit");
        let types_path = root_b.join("types.wit");

        let api_content = r#"interface api {
    record local {
        id: u64,
    }
    use types.{user};
    get-user: func() -> user;
}"#;
        let types_content = r#"interface types {
    record user {
        id: u64,
    }
}"#;

        write_file(&api_path, api_content);
        write_file(&types_path, types_content);

        let api_uri = file_uri(&api_path);
        let types_uri = file_uri(&types_path);

        let folders = vec![
            WorkspaceFolder {
                uri: Url::parse(&folder_uri(&root_a)).unwrap(),
                name: "root-a".to_string(),
            },
            WorkspaceFolder {
                uri: Url::parse(&folder_uri(&root_b)).unwrap(),
                name: "root-b".to_string(),
            },
        ];

        let (mut service, _) = init_service_with_folders(folders).await;

        send_notification(
            &mut service,
            "textDocument/didOpen",
            json!({
                "textDocument": {
                    "uri": api_uri,
                    "languageId": "wit",
                    "version": 1,
                    "text": api_content
                }
            }),
        )
        .await;

        send_notification(
            &mut service,
            "textDocument/didOpen",
            json!({
                "textDocument": {
                    "uri": types_uri,
                    "languageId": "wit",
                    "version": 1,
                    "text": types_content
                }
            }),
        )
        .await;

        let user_symbols = send_request(
            &mut service,
            21,
            "workspace/symbol",
            json!({ "query": "user" }),
        )
        .await;
        let user_symbols: Vec<SymbolInformation> = serde_json::from_value(user_symbols).unwrap();
        assert!(
            user_symbols
                .iter()
                .any(|symbol| symbol.name == "user" && symbol.location.uri.to_string() == types_uri),
            "workspace/symbol should include types from second root"
        );

        let local_symbols = send_request(
            &mut service,
            22,
            "workspace/symbol",
            json!({ "query": "local" }),
        )
        .await;
        let local_symbols: Vec<SymbolInformation> = serde_json::from_value(local_symbols).unwrap();
        assert!(
            local_symbols
                .iter()
                .any(|symbol| symbol.name == "local" && symbol.location.uri.to_string() == api_uri),
            "workspace/symbol should include types from first root"
        );

        let def = send_request(
            &mut service,
            23,
            "textDocument/definition",
            json!({
                "textDocument": { "uri": api_uri },
                "position": { "line": 5, "character": 24 }
            }),
        )
        .await;
        assert_eq!(def, serde_json::Value::Null);

        fs::remove_dir_all(root_a).ok();
        fs::remove_dir_all(root_b).ok();
    }
}
