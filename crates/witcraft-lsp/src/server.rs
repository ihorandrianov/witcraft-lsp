use crate::document::{DocumentStore, SharedDocumentStore};
use crate::workspace::{CrossFileResolver, ResolveResult, SharedWorkspaceManager, WorkspaceManager};
use std::path::PathBuf;
use std::sync::Arc;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};
use tracing::info;
use witcraft_syntax::{
    node_at, DefinitionKind, GlobalDefinition, NodeRef, PackageId, SyntaxKind, SymbolIndex,
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
}

impl WitLanguageServer {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents: Arc::new(DocumentStore::new()),
            workspace: Arc::new(WorkspaceManager::new()),
        }
    }

    async fn on_change(&self, uri: &str) {
        info!("on_change called for {}", uri);

        let diagnostics = self.documents.with_document_mut(uri, |doc| {
            info!("Inside with_document_mut closure, doc version: {}", doc.version);

            // First pass: collect all diagnostic data (ranges and messages)
            let (parse_errors, undefined_types, duplicates, unused_imports) = {
                info!("About to call parse(), content length: {}", doc.content.len());
                let parse = doc.parse();
                info!("Parse completed");
                let index = SymbolIndex::build(&parse.root);

                info!("Parse errors: {}", parse.errors.len());
                info!("References: {}", index.references().len());
                info!("Definitions: {}", index.definitions().len());

                // Extract package ID if present
                let package_id = parse.root.package.as_ref().map(PackageId::from_package_decl);

                // Update workspace with this file's definitions
                self.workspace.update_file_definitions(uri, &index, package_id.clone());

                let errors: Vec<_> = parse
                    .errors
                    .iter()
                    .map(|err| {
                        info!("Parse error: {} at {:?}", err.message, err.range);
                        (err.range, err.message.clone())
                    })
                    .collect();

                // Use cross-file resolver for undefined type detection
                let resolver = CrossFileResolver::new(&self.workspace);
                let undefined: Vec<_> = resolver
                    .find_undefined_types(uri, &index)
                    .into_iter()
                    .map(|u| {
                        info!("Undefined type: '{}'", u.name);
                        (u.range, u.name)
                    })
                    .collect();

                // Find duplicate definitions
                let duplicates: Vec<_> = index
                    .find_duplicate_definitions()
                    .into_iter()
                    .map(|d| (d.duplicate_range, d.name))
                    .collect();

                // Find unused imports
                let unused: Vec<_> = index
                    .find_unused_imports()
                    .into_iter()
                    .map(|u| (u.range, u.name))
                    .collect();

                (errors, undefined, duplicates, unused)
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

            for (range, name) in undefined_types {
                diagnostics.push(Diagnostic {
                    range: doc.range_to_lsp(range),
                    severity: Some(DiagnosticSeverity::ERROR),
                    message: format!("undefined type `{}`", name),
                    ..Default::default()
                });
            }

            for (range, name) in duplicates {
                diagnostics.push(Diagnostic {
                    range: doc.range_to_lsp(range),
                    severity: Some(DiagnosticSeverity::ERROR),
                    message: format!("duplicate definition `{}`", name),
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
            self.client
                .publish_diagnostics(Url::parse(uri).unwrap(), diags, None)
                .await;
            info!("Diagnostics published");
        } else {
            info!("No document found for {}", uri);
        }
    }

    /// Index all .wit files in the same directory as the given file.
    fn index_sibling_files(&self, uri: &str) {
        let Some(path) = uri.strip_prefix("file://") else {
            return;
        };
        let path = std::path::Path::new(path);
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
                let sibling_uri = format!("file://{}", entry_path.display());

                // Skip if already in documents (will be indexed via on_change)
                if self.documents.contains(&sibling_uri) {
                    continue;
                }

                // Read and index the file
                if let Ok(content) = std::fs::read_to_string(&entry_path) {
                    info!("Indexing sibling file: {}", sibling_uri);
                    let parse_result = witcraft_syntax::parse(&content);
                    let index = SymbolIndex::build(&parse_result.root);
                    let package_id = parse_result.root.package.as_ref().map(PackageId::from_package_decl);
                    self.workspace.update_file_definitions(&sibling_uri, &index, package_id);
                }
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
            .filter_map(|f| {
                f.uri.to_file_path().ok()
            })
            .collect();

        if !folders.is_empty() {
            info!("Initializing workspace with {} folders", folders.len());
            // We need interior mutability here - use unsafe or restructure
            // For now, we'll initialize on first file open
        } else if let Some(root_uri) = params.root_uri {
            if let Ok(path) = root_uri.to_file_path() {
                info!("Initializing workspace with root URI: {:?}", path);
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
                document_symbol_provider: Some(OneOf::Left(true)),
                code_action_provider: Some(CodeActionProviderCapability::Options(
                    CodeActionOptions {
                        code_action_kinds: Some(vec![
                            CodeActionKind::QUICKFIX,
                        ]),
                        ..Default::default()
                    },
                )),
                rename_provider: Some(OneOf::Right(RenameOptions {
                    prepare_provider: Some(true),
                    work_done_progress_options: Default::default(),
                })),
                workspace_symbol_provider: Some(OneOf::Left(true)),
                document_formatting_provider: Some(OneOf::Left(true)),
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
        self.documents
            .open(uri.clone(), params.text_document.version, params.text_document.text);

        // Index sibling .wit files for cross-file resolution
        self.index_sibling_files(&uri);

        self.on_change(&uri).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        info!("did_change for {}, version {}", uri, params.text_document.version);
        if let Some(change) = params.content_changes.into_iter().next() {
            info!("Updating document, content length: {}", change.text.len());
            let updated = self.documents
                .update(&uri, params.text_document.version, change.text);
            info!("Document update result: {}", updated);
            self.on_change(&uri).await;
        } else {
            info!("No content changes received");
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        self.documents.close(&uri);
        self.workspace.remove_file(&uri);
        self.client
            .publish_diagnostics(params.text_document.uri, vec![], None)
            .await;
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri.to_string();
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
                NodeRef::NamedType(named) => Some(named.name.name.clone()),
                NodeRef::Ident(ident) => {
                    if let Some(reference) = index.reference_at(offset) {
                        Some(reference.name.clone())
                    } else {
                        Some(ident.name.clone())
                    }
                }
                NodeRef::UsePath(path) => Some(path.name.name.clone()),
                _ => None,
            }?;

            // Try local definition first
            if let Some(def) = index.find_definition(&name) {
                return Some((
                    Location::new(
                        params.text_document_position_params.text_document.uri.clone(),
                        doc.range_to_lsp(def.name_range),
                    ),
                    true, // found locally
                ));
            }

            // Return name for cross-file lookup
            Some((
                Location::new(
                    Url::parse("file:///placeholder").unwrap(),
                    Range::default(),
                ),
                false, // need cross-file lookup
            ))
        });

        // If found locally, return it
        if let Some(Some((location, true))) = local_result {
            return Ok(Some(GotoDefinitionResponse::Scalar(location)));
        }

        // Try cross-file resolution
        let cross_file_result = self.documents.with_document_mut(&uri, |doc| {
            let offset = doc.position_to_offset(witcraft_syntax::Position::new(
                position.line,
                position.character,
            ))?;

            let parse = doc.parse();
            let index = SymbolIndex::build(&parse.root);
            let node = node_at(&parse.root, offset)?;

            let name = match node {
                NodeRef::NamedType(named) => Some(named.name.name.clone()),
                NodeRef::Ident(ident) => {
                    if let Some(reference) = index.reference_at(offset) {
                        Some(reference.name.clone())
                    } else {
                        Some(ident.name.clone())
                    }
                }
                NodeRef::UsePath(path) => Some(path.name.name.clone()),
                _ => None,
            }?;

            // Use cross-file resolver
            let resolver = CrossFileResolver::new(&self.workspace);
            match resolver.resolve_type(&uri, &name, &index) {
                crate::workspace::ResolveResult::Found(global_def) => {
                    // We found it in another file - need to get that file's line index
                    // For now, just return the raw range (will need adjustment)
                    let target_uri = Url::parse(&global_def.uri).ok()?;

                    // Try to get the target document's line index for proper range conversion
                    if let Some(target_range) = self.documents.with_document(&global_def.uri, |target_doc| {
                        target_doc.range_to_lsp(global_def.name_range)
                    }) {
                        Some(Location::new(target_uri, target_range))
                    } else {
                        // Fallback: use raw byte offsets converted to Position
                        // This is approximate but better than nothing
                        let start = witcraft_syntax::Position::new(0, global_def.name_range.start());
                        let end = witcraft_syntax::Position::new(0, global_def.name_range.end());
                        Some(Location::new(
                            target_uri,
                            Range::new(
                                tower_lsp::lsp_types::Position::new(start.line, start.column),
                                tower_lsp::lsp_types::Position::new(end.line, end.column),
                            ),
                        ))
                    }
                }
                _ => None,
            }
        });

        match cross_file_result {
            Some(Some(location)) => Ok(Some(GotoDefinitionResponse::Scalar(location))),
            _ => Ok(None),
        }
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri.to_string();
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
                    ResolveResult::Imported(import) => {
                        Some(format!(
                            "```wit\n{}\n```\n\n*Imported from `{}`*",
                            import.original_name, import.from_interface
                        ))
                    }
                    ResolveResult::Builtin => {
                        Some(format!("```wit\n{}\n```\n\n*Builtin type*", name))
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
                        reference.name.clone()
                    } else {
                        ident.name.clone()
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

        // First get the symbol name from the current position
        let name = self.documents.with_document_mut(&uri, |doc| {
            let offset = doc.position_to_offset(witcraft_syntax::Position::new(
                position.line,
                position.character,
            ))?;

            let parse = doc.parse();
            let node = node_at(&parse.root, offset)?;

            let name = match node {
                NodeRef::NamedType(named) => named.name.name.clone(),
                NodeRef::Ident(ident) => ident.name.clone(),
                NodeRef::Interface(iface) => iface.name.name.clone(),
                NodeRef::World(world) => world.name.name.clone(),
                NodeRef::Func(func) => func.name.name.clone(),
                NodeRef::Record(rec) => rec.name.name.clone(),
                NodeRef::Variant(var) => var.name.name.clone(),
                NodeRef::Enum(e) => e.name.name.clone(),
                NodeRef::Flags(f) => f.name.name.clone(),
                NodeRef::Resource(res) => res.name.name.clone(),
                NodeRef::TypeAlias(alias) => alias.name.name.clone(),
                _ => return None,
            };

            Some(name)
        }).flatten();

        let Some(name) = name else {
            return Ok(None);
        };

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
            if let Some(file_locations) = self.documents.with_document_mut(file_uri, |doc| {
                let parse = doc.parse();
                let index = SymbolIndex::build(&parse.root);
                let mut file_locs = Vec::new();

                // Include declaration if requested
                if include_declaration {
                    if let Some(def) = index.find_definition(&name) {
                        file_locs.push(Location::new(
                            Url::parse(file_uri).unwrap(),
                            doc.range_to_lsp(def.name_range),
                        ));
                    }
                }

                // Find all references
                for reference in index.references() {
                    if reference.name == name {
                        file_locs.push(Location::new(
                            Url::parse(file_uri).unwrap(),
                            doc.range_to_lsp(reference.range),
                        ));
                    }
                }

                if file_locs.is_empty() {
                    None
                } else {
                    Some(file_locs)
                }
            }).flatten() {
                locations.extend(file_locations);
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

    async fn completion(
        &self,
        params: CompletionParams,
    ) -> Result<Option<CompletionResponse>> {
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
                    label: def.name.clone(),
                    kind: Some(kind),
                    detail: def.parent.clone(),
                    ..Default::default()
                });
            }

            // Add imported types (via `use` statements)
            for import in index.imports() {
                items.push(CompletionItem {
                    label: import.local_name.clone(),
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
                    .map(|i| i.name.as_str())
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
                            name: iface.name.name.clone(),
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
                            name: world.name.name.clone(),
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

    async fn code_action(
        &self,
        params: CodeActionParams,
    ) -> Result<Option<CodeActionResponse>> {
        let uri = params.text_document.uri.to_string();
        let _range = params.range;

        // Collect code actions based on diagnostics
        let mut actions = Vec::new();

        // Get the document and build index
        let context = self.documents.with_document_mut(&uri, |doc| {
            let line_index = doc.line_index.clone();
            let parse = doc.parse();
            let index = SymbolIndex::build(&parse.root);

            // Helper to convert LSP Range to byte range
            let _lsp_to_offset = |pos: tower_lsp::lsp_types::Position| -> Option<u32> {
                line_index.offset(witcraft_syntax::Position::new(pos.line, pos.character))
            };

            // Find the first interface/world for insertion point
            let first_item_start = parse.root.items.first().map(|item| {
                match item {
                    witcraft_syntax::ast::Item::Interface(iface) => iface.range.start() + 1, // after '{'
                    witcraft_syntax::ast::Item::World(world) => world.range.start() + 1,
                    witcraft_syntax::ast::Item::TypeDef(td) => td.range().start(),
                }
            });

            (index, first_item_start, line_index.clone())
        });

        let Some((index, first_item_start, line_index)) = context else {
            return Ok(None);
        };

        // Process diagnostics to find actionable ones
        for diag in &params.context.diagnostics {
            // Handle "unused import" diagnostics
            if diag.message.starts_with("unused import") {
                if let Some(action) = self.make_remove_import_action(&uri, diag, &index, &line_index) {
                    actions.push(action);
                }
            }

            // Handle "undefined type" diagnostics
            if diag.message.starts_with("undefined type") {
                if let Some(action) = self.make_add_import_action(&uri, diag, &index, first_item_start, &line_index) {
                    actions.push(action);
                }
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
                        (ident.name.clone(), ident.range)
                    } else if index.reference_at(offset).is_some() {
                        // It's a reference - find the definition
                        let reference = index.reference_at(offset)?;
                        (reference.name.clone(), reference.range)
                    } else {
                        return None;
                    }
                }
                NodeRef::Interface(iface) => (iface.name.name.clone(), iface.name.range),
                NodeRef::World(world) => (world.name.name.clone(), world.name.range),
                NodeRef::Record(rec) => (rec.name.name.clone(), rec.name.range),
                NodeRef::Variant(var) => (var.name.name.clone(), var.name.range),
                NodeRef::Enum(e) => (e.name.name.clone(), e.name.range),
                NodeRef::Flags(f) => (f.name.name.clone(), f.name.range),
                NodeRef::Resource(res) => (res.name.name.clone(), res.name.range),
                NodeRef::TypeAlias(alias) => (alias.name.name.clone(), alias.name.range),
                NodeRef::Func(func) => (func.name.name.clone(), func.name.range),
                NodeRef::NamedType(named) => (named.name.name.clone(), named.name.range),
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

        // First, find the symbol at the position
        let symbol_name = self.documents.with_document_mut(&uri, |doc| {
            let offset = doc.position_to_offset(witcraft_syntax::Position::new(
                position.line,
                position.character,
            ))?;

            let parse = doc.parse();
            let node = node_at(&parse.root, offset)?;

            // Get the symbol name
            match node {
                NodeRef::Ident(ident) => Some(ident.name.clone()),
                NodeRef::Interface(iface) => Some(iface.name.name.clone()),
                NodeRef::World(world) => Some(world.name.name.clone()),
                NodeRef::Record(rec) => Some(rec.name.name.clone()),
                NodeRef::Variant(var) => Some(var.name.name.clone()),
                NodeRef::Enum(e) => Some(e.name.name.clone()),
                NodeRef::Flags(f) => Some(f.name.name.clone()),
                NodeRef::Resource(res) => Some(res.name.name.clone()),
                NodeRef::TypeAlias(alias) => Some(alias.name.name.clone()),
                NodeRef::Func(func) => Some(func.name.name.clone()),
                NodeRef::NamedType(named) => Some(named.name.name.clone()),
                _ => None,
            }
        }).flatten();

        let Some(old_name) = symbol_name else {
            return Ok(None);
        };

        // Get all files in the same package
        let package_files = self.workspace.files_in_same_package(&uri);
        let files_to_search: Vec<String> = if package_files.is_empty() {
            vec![uri.clone()]
        } else {
            package_files
        };

        let mut changes: std::collections::HashMap<Url, Vec<TextEdit>> = std::collections::HashMap::new();

        // Search in each file for occurrences
        for file_uri in &files_to_search {
            let file_edits = self.documents.with_document_mut(file_uri, |doc| {
                let parse = doc.parse();
                let index = SymbolIndex::build(&parse.root);
                let mut edits = Vec::new();

                // Find definition in this file
                if let Some(def) = index.find_definition(&old_name) {
                    edits.push(TextEdit {
                        range: doc.range_to_lsp(def.name_range),
                        new_text: new_name.clone(),
                    });
                }

                // Find all references in this file
                for reference in index.references() {
                    if reference.name == old_name {
                        edits.push(TextEdit {
                            range: doc.range_to_lsp(reference.range),
                            new_text: new_name.clone(),
                        });
                    }
                }

                // Find imports that reference this name
                for import in index.imports() {
                    if import.original_name == old_name {
                        // Rename the original name in the import statement
                        // For `use iface.{old-name as alias}`, we rename `old-name`
                        edits.push(TextEdit {
                            range: doc.range_to_lsp(import.original_name_range),
                            new_text: new_name.clone(),
                        });

                        // If there's no alias, also update usages (they use the original name)
                        // If there IS an alias, usages use the alias so we don't touch them
                        if import.local_name == old_name {
                            // No alias - local_name equals original_name
                            // References using this name will be caught by the references loop above
                        }
                    } else if import.local_name == old_name && import.original_name != old_name {
                        // The import has an alias that matches old_name
                        // e.g., `use iface.{something as old-name}`
                        // Rename the alias
                        edits.push(TextEdit {
                            range: doc.range_to_lsp(import.range),
                            new_text: new_name.clone(),
                        });
                    }
                }

                if edits.is_empty() {
                    None
                } else {
                    Some(edits)
                }
            }).flatten();

            if let Some(edits) = file_edits {
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

        // Get all definitions from the workspace
        let mut symbols = Vec::new();

        // Get all URIs from open documents
        let all_uris = self.documents.uris();

        // For each file, get its definitions
        for file_uri in &all_uris {
            // Get definitions from workspace index
            let defs = self.workspace.all_definitions(file_uri);

            for def in defs {
                // Filter by query (case-insensitive substring match)
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

                // Try to get proper LSP range from the document
                let location = if let Some(range) = self.documents.with_document(&def.uri, |doc| {
                    doc.range_to_lsp(def.name_range)
                }) {
                    Location::new(
                        Url::parse(&def.uri).unwrap_or_else(|_| Url::parse("file:///unknown").unwrap()),
                        range,
                    )
                } else {
                    // Document not open - use approximate range
                    Location::new(
                        Url::parse(&def.uri).unwrap_or_else(|_| Url::parse("file:///unknown").unwrap()),
                        Range::default(),
                    )
                };

                #[allow(deprecated)]
                symbols.push(SymbolInformation {
                    name: def.name.clone(),
                    kind,
                    tags: None,
                    deprecated: None,
                    location,
                    container_name: def.parent.clone(),
                });
            }
        }

        // Sort symbols by name for consistent ordering
        symbols.sort_by(|a, b| a.name.cmp(&b.name));

        if symbols.is_empty() {
            Ok(None)
        } else {
            Ok(Some(symbols))
        }
    }

    async fn formatting(
        &self,
        params: DocumentFormattingParams,
    ) -> Result<Option<Vec<TextEdit>>> {
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
}

impl WitLanguageServer {
    /// Create a code action to remove an unused import.
    fn make_remove_import_action(
        &self,
        uri: &str,
        diag: &Diagnostic,
        index: &SymbolIndex,
        line_index: &witcraft_syntax::LineIndex,
    ) -> Option<CodeActionOrCommand> {
        // Extract the import name from the diagnostic message
        let name = diag.message
            .strip_prefix("unused import `")?
            .strip_suffix('`')?;

        // Find the import in the index
        let import = index.find_import(name)?;

        // Count how many imports share the same use statement
        let siblings_count = index.imports()
            .iter()
            .filter(|i| i.use_statement_range == import.use_statement_range)
            .count();

        // Determine what range to delete
        let delete_range = if siblings_count == 1 {
            // Only import in this use statement - delete the whole statement
            import.use_statement_range
        } else {
            // Multiple imports - just delete this item
            // We'll delete the item_range, though this might leave a dangling comma
            import.item_range
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
        first_item_start: Option<u32>,
        line_index: &witcraft_syntax::LineIndex,
    ) -> Option<CodeActionOrCommand> {
        // Extract the type name from the diagnostic message
        let type_name = diag.message
            .strip_prefix("undefined type `")?
            .strip_suffix('`')?;

        // Find where this type is defined in the workspace
        let global_def = self.workspace.find_definition(uri, type_name)?;

        // Get the interface that contains this type
        let interface_name = global_def.parent.as_ref()?;

        // Check if we already have a use statement for this interface
        let existing_use = index.imports()
            .iter()
            .find(|i| i.from_interface == *interface_name);

        let (insert_pos, new_text) = if let Some(_existing) = existing_use {
            // Add to existing use statement - insert before the closing brace
            // Find the position just before the closing brace '}'
            // This is a simplification - ideally we'd parse the exact position
            // For now, we'll create a new use statement instead
            let insert_offset = first_item_start.unwrap_or(0);
            let pos = line_index.position(insert_offset);
            let insert_position = tower_lsp::lsp_types::Position {
                line: pos.line,
                character: pos.column,
            };
            (insert_position, format!("\n    use {}.{{{}}};", interface_name, type_name))
        } else {
            // Create a new use statement at the start of the interface
            let insert_offset = first_item_start.unwrap_or(0);
            let pos = line_index.position(insert_offset);
            let insert_position = tower_lsp::lsp_types::Position {
                line: pos.line,
                character: pos.column,
            };
            (insert_position, format!("\n    use {}.{{{}}};", interface_name, type_name))
        };

        // Create the text edit
        let edit = TextEdit {
            range: Range {
                start: insert_pos,
                end: insert_pos,
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
}

fn build_interface_symbols<F>(iface: &witcraft_syntax::ast::InterfaceDecl, range_to_lsp: &F) -> Vec<DocumentSymbol>
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
                    name: func.name.name.clone(),
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
                let names: Vec<_> = use_stmt.names.iter().map(|n| n.name.name.as_str()).collect();
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

fn build_world_symbols<F>(world: &witcraft_syntax::ast::WorldDecl, range_to_lsp: &F) -> Vec<DocumentSymbol>
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
                let names: Vec<_> = use_stmt.names.iter().map(|n| n.name.name.as_str()).collect();
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

fn build_typedef_symbol<F>(typedef: &witcraft_syntax::ast::TypeDef, range_to_lsp: &F) -> Option<DocumentSymbol>
where
    F: Fn(witcraft_syntax::TextRange) -> Range,
{
    let (name, kind, detail, range, name_range) = match typedef {
        witcraft_syntax::ast::TypeDef::Alias(alias) => (
            alias.name.name.clone(),
            SymbolKind::TYPE_PARAMETER,
            "type",
            alias.range,
            alias.name.range,
        ),
        witcraft_syntax::ast::TypeDef::Record(rec) => (
            rec.name.name.clone(),
            SymbolKind::STRUCT,
            "record",
            rec.range,
            rec.name.range,
        ),
        witcraft_syntax::ast::TypeDef::Variant(var) => (
            var.name.name.clone(),
            SymbolKind::ENUM,
            "variant",
            var.range,
            var.name.range,
        ),
        witcraft_syntax::ast::TypeDef::Enum(e) => (
            e.name.name.clone(),
            SymbolKind::ENUM,
            "enum",
            e.range,
            e.name.range,
        ),
        witcraft_syntax::ast::TypeDef::Flags(f) => (
            f.name.name.clone(),
            SymbolKind::ENUM,
            "flags",
            f.range,
            f.name.range,
        ),
        witcraft_syntax::ast::TypeDef::Resource(res) => (
            res.name.name.clone(),
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
