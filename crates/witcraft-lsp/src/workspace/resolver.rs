//! Cross-file symbol resolution.
//!
//! WIT spec-compliant: types must be explicitly imported via `use` statements.

use super::WorkspaceManager;
use witcraft_syntax::{GlobalDefinition, Import, SymbolIndex};

/// WIT builtin primitive types.
const PRIMITIVE_TYPES: &[&str] = &[
    "bool", "u8", "u16", "u32", "u64", "s8", "s16", "s32", "s64", "f32", "f64", "char", "string",
];

/// Resolves symbols across files in a workspace.
pub struct CrossFileResolver<'a> {
    workspace: &'a WorkspaceManager,
}

impl<'a> CrossFileResolver<'a> {
    pub fn new(workspace: &'a WorkspaceManager) -> Self {
        Self { workspace }
    }

    /// Resolve a type name to its definition.
    ///
    /// WIT spec-compliant resolution:
    /// 1. Check local definitions (types defined in the same interface)
    /// 2. Check imported names (via `use` statements)
    /// 3. Check builtin types
    pub fn resolve_type(
        &self,
        uri: &str,
        name: &str,
        local_index: &SymbolIndex,
    ) -> ResolveResult {
        // 1. Check local definitions first
        if let Some(def) = local_index.find_definition(name) {
            return ResolveResult::Found(GlobalDefinition::from_definition(def, uri));
        }

        // 2. Check if name was imported via `use` statement
        if let Some(import) = local_index.find_import(name) {
            // Try to resolve the imported type to its definition
            if let Some(def) = self.resolve_import(uri, import) {
                return ResolveResult::Found(def);
            }
            // Import exists but we couldn't find the target - still valid as imported
            return ResolveResult::Imported(import.clone());
        }

        // 3. Check if it's a builtin
        if is_builtin_type(name) {
            return ResolveResult::Builtin;
        }

        ResolveResult::NotFound
    }

    /// Resolve an import to its actual definition.
    fn resolve_import(&self, uri: &str, import: &Import) -> Option<GlobalDefinition> {
        // Look up the original name in the workspace
        // The import.from_interface tells us which interface to look in
        self.workspace
            .find_definition(uri, &import.original_name)
    }

    /// Check if a type reference is valid (defined or builtin).
    pub fn is_type_defined(&self, uri: &str, name: &str, local_index: &SymbolIndex) -> bool {
        !matches!(self.resolve_type(uri, name, local_index), ResolveResult::NotFound)
    }

    /// Find all references to a symbol across the package.
    pub fn find_all_references<F>(
        &self,
        uri: &str,
        name: &str,
        mut get_index: F,
    ) -> Vec<ReferenceLocation>
    where
        F: FnMut(&str) -> Option<SymbolIndex>,
    {
        let mut locations = Vec::new();

        // Get all files in the same package
        let files = self.workspace.files_in_same_package(uri);

        for file_uri in files {
            if let Some(index) = get_index(&file_uri) {
                // Check for definition
                if let Some(def) = index.find_definition(name) {
                    locations.push(ReferenceLocation {
                        uri: file_uri.clone(),
                        range: def.name_range,
                        is_definition: true,
                    });
                }

                // Check for references
                for reference in index.references() {
                    if &*reference.name == name {
                        locations.push(ReferenceLocation {
                            uri: file_uri.clone(),
                            range: reference.range,
                            is_definition: false,
                        });
                    }
                }
            }
        }

        locations
    }

    /// Get all undefined types in a file, considering cross-file definitions.
    pub fn find_undefined_types(&self, uri: &str, local_index: &SymbolIndex) -> Vec<UndefinedType> {
        let mut undefined = Vec::new();

        // Get all definitions in the workspace for suggestions
        let all_defs = self.workspace.all_definitions(uri);

        for reference in local_index.references() {
            if reference.kind == witcraft_syntax::ReferenceKind::Type && !reference.name.is_empty() {
                if matches!(
                    self.resolve_type(uri, &reference.name, local_index),
                    ResolveResult::NotFound
                ) {
                    let (similar_names, available_in) =
                        self.find_suggestions(&reference.name, &all_defs);

                    undefined.push(UndefinedType {
                        name: reference.name.to_string(),
                        range: reference.range,
                        similar_names,
                        available_in,
                    });
                }
            }
        }

        undefined
    }

    /// Find suggestions for an undefined type name.
    fn find_suggestions(
        &self,
        name: &str,
        all_defs: &[witcraft_syntax::GlobalDefinition],
    ) -> (Vec<SimilarName>, Vec<AvailableType>) {
        let mut similar_names = Vec::new();
        let mut available_in = Vec::new();

        for def in all_defs {
            // Skip interfaces and worlds - we're looking for types
            if matches!(
                def.kind,
                witcraft_syntax::DefinitionKind::Interface | witcraft_syntax::DefinitionKind::World
            ) {
                continue;
            }

            // Exact match - type exists but not imported
            if def.name.as_ref() == name {
                if let Some(parent) = &def.parent {
                    let file = def
                        .uri
                        .rsplit('/')
                        .next()
                        .unwrap_or(&def.uri)
                        .to_string();
                    available_in.push(AvailableType {
                        interface: parent.to_string(),
                        file,
                    });
                }
                continue;
            }

            // Check for similar names (possible typos)
            let similarity = name_similarity(name, &def.name);
            if similarity >= SIMILARITY_THRESHOLD {
                similar_names.push(SimilarName {
                    name: def.name.to_string(),
                    kind: def.kind,
                    interface: def.parent.as_ref().map(|s| s.to_string()),
                });
            }
        }

        // Sort similar names by relevance (highest similarity first)
        similar_names.sort_by(|a, b| {
            let sim_a = name_similarity(name, &a.name);
            let sim_b = name_similarity(name, &b.name);
            sim_b.partial_cmp(&sim_a).unwrap_or(std::cmp::Ordering::Equal)
        });

        // Limit to top 3 suggestions
        similar_names.truncate(3);

        (similar_names, available_in)
    }
}

/// Result of resolving a symbol.
#[derive(Debug, Clone)]
pub enum ResolveResult {
    /// Symbol was found, here's its definition.
    Found(GlobalDefinition),
    /// Symbol was imported but we couldn't find the definition (still valid).
    Imported(Import),
    /// Symbol is a builtin type (no location).
    Builtin,
    /// Symbol was not found.
    NotFound,
}

/// Location of a reference.
#[derive(Debug, Clone)]
pub struct ReferenceLocation {
    pub uri: String,
    pub range: witcraft_syntax::TextRange,
    pub is_definition: bool,
}

/// An undefined type reference with suggestions for fixing.
#[derive(Debug, Clone)]
pub struct UndefinedType {
    pub name: String,
    pub range: witcraft_syntax::TextRange,
    /// Similar names that might be typos (e.g., "User" when "user" exists).
    pub similar_names: Vec<SimilarName>,
    /// Interfaces where this type is defined but not imported.
    pub available_in: Vec<AvailableType>,
}

/// A similar name suggestion for typo correction.
#[derive(Debug, Clone)]
pub struct SimilarName {
    pub name: String,
    pub kind: witcraft_syntax::DefinitionKind,
    /// Which interface contains this definition, if any.
    pub interface: Option<String>,
}

/// Information about where a type is available.
#[derive(Debug, Clone)]
pub struct AvailableType {
    pub interface: String,
    pub file: String,
}

/// Check if a name is a WIT builtin type.
pub fn is_builtin_type(name: &str) -> bool {
    PRIMITIVE_TYPES.contains(&name)
}

/// Calculate similarity between two strings (0.0 to 1.0).
/// Uses a simple algorithm based on common characters and length.
fn name_similarity(a: &str, b: &str) -> f32 {
    if a == b {
        return 1.0;
    }

    let a_lower = a.to_lowercase();
    let b_lower = b.to_lowercase();

    // Case-insensitive exact match is very high similarity
    if a_lower == b_lower {
        return 0.95;
    }

    // Calculate Levenshtein distance
    let distance = levenshtein_distance(&a_lower, &b_lower);
    let max_len = a.len().max(b.len());

    if max_len == 0 {
        return 1.0;
    }

    // Convert distance to similarity (0.0 to 1.0)
    1.0 - (distance as f32 / max_len as f32)
}

/// Simple Levenshtein distance implementation.
fn levenshtein_distance(a: &str, b: &str) -> usize {
    let a_chars: Vec<char> = a.chars().collect();
    let b_chars: Vec<char> = b.chars().collect();
    let a_len = a_chars.len();
    let b_len = b_chars.len();

    if a_len == 0 {
        return b_len;
    }
    if b_len == 0 {
        return a_len;
    }

    // Use two rows instead of full matrix for space efficiency
    let mut prev_row: Vec<usize> = (0..=b_len).collect();
    let mut curr_row: Vec<usize> = vec![0; b_len + 1];

    for (i, a_char) in a_chars.iter().enumerate() {
        curr_row[0] = i + 1;

        for (j, b_char) in b_chars.iter().enumerate() {
            let cost = if a_char == b_char { 0 } else { 1 };
            curr_row[j + 1] = (prev_row[j + 1] + 1) // deletion
                .min(curr_row[j] + 1) // insertion
                .min(prev_row[j] + cost); // substitution
        }

        std::mem::swap(&mut prev_row, &mut curr_row);
    }

    prev_row[b_len]
}

/// Minimum similarity threshold for suggesting a name (0.0 to 1.0).
const SIMILARITY_THRESHOLD: f32 = 0.6;

#[cfg(test)]
mod tests {
    use super::*;
    use witcraft_syntax::parse;

    #[test]
    fn test_is_builtin_type() {
        assert!(is_builtin_type("u32"));
        assert!(is_builtin_type("string"));
        assert!(is_builtin_type("bool"));
        assert!(!is_builtin_type("user"));
        assert!(!is_builtin_type("my-type"));
    }

    #[test]
    fn test_resolver_local() {
        let workspace = WorkspaceManager::new();
        let resolver = CrossFileResolver::new(&workspace);

        let source = r#"interface types {
            record user {
                id: u64,
                name: string,
            }
            get-user: func() -> user;
        }"#;

        let result = parse(source);
        let index = witcraft_syntax::SymbolIndex::build(&result.root);

        // Local definition should be found
        let resolve = resolver.resolve_type("file:///types.wit", "user", &index);
        assert!(matches!(resolve, ResolveResult::Found(_)));

        // Builtin should be recognized
        let resolve = resolver.resolve_type("file:///types.wit", "u64", &index);
        assert!(matches!(resolve, ResolveResult::Builtin));

        // Unknown type should not be found
        let resolve = resolver.resolve_type("file:///types.wit", "unknown", &index);
        assert!(matches!(resolve, ResolveResult::NotFound));
    }

    #[test]
    fn test_find_undefined_types() {
        let workspace = WorkspaceManager::new();
        let resolver = CrossFileResolver::new(&workspace);

        let source = r#"interface api {
            get-user: func(id: u64) -> user;
            get-post: func(id: u64) -> post;
        }"#;

        let result = parse(source);
        let index = witcraft_syntax::SymbolIndex::build(&result.root);

        let undefined = resolver.find_undefined_types("file:///api.wit", &index);

        assert_eq!(undefined.len(), 2);
        let names: Vec<_> = undefined.iter().map(|u| &*u.name).collect();
        assert!(names.contains(&"user"));
        assert!(names.contains(&"post"));
    }

    #[test]
    fn test_spec_compliant_resolution() {
        // WIT spec: types must be explicitly imported via `use` statements
        let types_source = r#"interface types {
            record user {
                id: u64,
                name: string,
            }
            record post {
                id: u64,
                author: user,
            }
            enum status {
                active,
                inactive,
            }
        }"#;

        // api.wit imports user and post via `use`, but NOT status
        let api_source = r#"interface api {
            use types.{user, post};
            get-user: func(id: u64) -> user;
            get-post: func(id: u64) -> post;
            get-status: func() -> status;
            get-unknown: func() -> nonexistent;
        }"#;

        // Parse both files
        let types_result = parse(types_source);
        let api_result = parse(api_source);

        let types_index = witcraft_syntax::SymbolIndex::build(&types_result.root);
        let api_index = witcraft_syntax::SymbolIndex::build(&api_result.root);

        // Verify imports were indexed
        assert_eq!(api_index.imports().len(), 2, "should have 2 imports: user, post");
        assert!(api_index.find_import("user").is_some(), "user should be imported");
        assert!(api_index.find_import("post").is_some(), "post should be imported");
        assert!(api_index.find_import("status").is_none(), "status should NOT be imported");

        // Set up workspace with both files
        let workspace = WorkspaceManager::new();
        workspace.update_file_definitions("file:///project/types.wit", &types_index, None);
        workspace.update_file_definitions("file:///project/api.wit", &api_index, None);

        let resolver = CrossFileResolver::new(&workspace);

        // 'user' should be found (imported and defined in types.wit)
        let resolve = resolver.resolve_type("file:///project/api.wit", "user", &api_index);
        assert!(
            matches!(resolve, ResolveResult::Found(_) | ResolveResult::Imported(_)),
            "user should be found via import"
        );

        // 'post' should be found (imported and defined in types.wit)
        let resolve = resolver.resolve_type("file:///project/api.wit", "post", &api_index);
        assert!(
            matches!(resolve, ResolveResult::Found(_) | ResolveResult::Imported(_)),
            "post should be found via import"
        );

        // 'status' is NOT imported, so it should NOT be found (spec-compliant)
        let resolve = resolver.resolve_type("file:///project/api.wit", "status", &api_index);
        assert!(
            matches!(resolve, ResolveResult::NotFound),
            "status should NOT be found (not imported)"
        );

        // 'nonexistent' should NOT be found
        let resolve = resolver.resolve_type("file:///project/api.wit", "nonexistent", &api_index);
        assert!(matches!(resolve, ResolveResult::NotFound), "nonexistent should not be found");

        // 'u64' should be recognized as builtin
        let resolve = resolver.resolve_type("file:///project/api.wit", "u64", &api_index);
        assert!(matches!(resolve, ResolveResult::Builtin), "u64 should be builtin");

        // Check undefined types - both 'status' and 'nonexistent' should be undefined
        let undefined = resolver.find_undefined_types("file:///project/api.wit", &api_index);
        let names: Vec<_> = undefined.iter().map(|u| &*u.name).collect();
        assert!(names.contains(&"status"), "status should be undefined (not imported)");
        assert!(names.contains(&"nonexistent"), "nonexistent should be undefined");
        assert_eq!(undefined.len(), 2, "should have 2 undefined types");
    }

    #[test]
    fn test_import_with_alias() {
        let source = r#"interface api {
            use types.{user as my-user};
            get-user: func() -> my-user;
        }"#;

        let result = parse(source);
        let index = witcraft_syntax::SymbolIndex::build(&result.root);

        // Verify import with alias
        assert_eq!(index.imports().len(), 1);
        let import = index.find_import("my-user").unwrap();
        assert_eq!(import.local_name.as_ref(), "my-user");
        assert_eq!(import.original_name.as_ref(), "user");
        assert_eq!(import.from_interface.as_ref(), "types");

        // Original name should not be in scope
        assert!(index.find_import("user").is_none());
    }

    #[test]
    fn test_levenshtein_distance() {
        assert_eq!(levenshtein_distance("", ""), 0);
        assert_eq!(levenshtein_distance("abc", "abc"), 0);
        assert_eq!(levenshtein_distance("abc", "ab"), 1);
        assert_eq!(levenshtein_distance("abc", "abcd"), 1);
        assert_eq!(levenshtein_distance("abc", "adc"), 1);
        assert_eq!(levenshtein_distance("kitten", "sitting"), 3);
    }

    #[test]
    fn test_name_similarity() {
        assert_eq!(name_similarity("user", "user"), 1.0);
        assert!(name_similarity("user", "User") > 0.9);
        assert!(name_similarity("user", "users") > 0.7);
        assert!(name_similarity("user", "usr") > 0.5);
        assert!(name_similarity("user", "completely-different") < 0.3);
    }

    #[test]
    fn test_undefined_type_with_similar_name() {
        let types_source = r#"interface types {
            record user { id: u64 }
        }"#;

        let api_source = r#"interface api {
            get-user: func() -> User;
        }"#;

        let types_result = parse(types_source);
        let types_index = witcraft_syntax::SymbolIndex::build(&types_result.root);

        let api_result = parse(api_source);
        let api_index = witcraft_syntax::SymbolIndex::build(&api_result.root);

        let workspace = WorkspaceManager::new();
        workspace.update_file_definitions("file:///types.wit", &types_index, None);
        workspace.update_file_definitions("file:///api.wit", &api_index, None);

        let resolver = CrossFileResolver::new(&workspace);
        let undefined = resolver.find_undefined_types("file:///api.wit", &api_index);

        assert_eq!(undefined.len(), 1);
        assert_eq!(undefined[0].name, "User");
        assert!(!undefined[0].similar_names.is_empty());
        assert_eq!(undefined[0].similar_names[0].name, "user");
    }

    #[test]
    fn test_undefined_type_available_in_interface() {
        let types_source = r#"interface types {
            record user { id: u64 }
        }"#;

        let api_source = r#"interface api {
            get-user: func() -> user;
        }"#;

        let types_result = parse(types_source);
        let types_index = witcraft_syntax::SymbolIndex::build(&types_result.root);

        let api_result = parse(api_source);
        let api_index = witcraft_syntax::SymbolIndex::build(&api_result.root);

        let workspace = WorkspaceManager::new();
        workspace.update_file_definitions("file:///types.wit", &types_index, None);
        workspace.update_file_definitions("file:///api.wit", &api_index, None);

        let resolver = CrossFileResolver::new(&workspace);
        let undefined = resolver.find_undefined_types("file:///api.wit", &api_index);

        assert_eq!(undefined.len(), 1);
        assert_eq!(undefined[0].name, "user");
        assert!(!undefined[0].available_in.is_empty());
        assert_eq!(undefined[0].available_in[0].interface, "types");
    }
}
