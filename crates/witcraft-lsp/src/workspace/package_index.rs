//! Package-level symbol index for cross-file lookups.

use std::collections::{HashMap, HashSet};
use witcraft_syntax::{DefinitionKind, GlobalDefinition, PackageId, SymbolIndex};

/// Aggregates definitions across all files in a package.
#[derive(Debug, Default)]
pub struct PackageIndex {
    /// The package identifier (from package declaration).
    package_id: Option<PackageId>,
    /// Set of file URIs in this package.
    files: HashSet<String>,
    /// Map from definition name to all definitions with that name.
    definitions: HashMap<String, Vec<GlobalDefinition>>,
    /// Map from interface name to the types it exports (for `use` resolution).
    interface_exports: HashMap<String, HashSet<String>>,
}

impl PackageIndex {
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the package identifier.
    pub fn set_package_id(&mut self, id: PackageId) {
        self.package_id = Some(id);
    }

    /// Get the package identifier.
    pub fn package_id(&self) -> Option<&PackageId> {
        self.package_id.as_ref()
    }

    /// Add a file to this package.
    pub fn add_file(&mut self, uri: String) {
        self.files.insert(uri);
    }

    /// Remove a file from this package and its definitions.
    pub fn remove_file(&mut self, uri: &str) {
        self.files.remove(uri);

        // Remove all definitions from this file
        for defs in self.definitions.values_mut() {
            defs.retain(|d| d.uri != uri);
        }

        // Clean up empty entries
        self.definitions.retain(|_, v| !v.is_empty());

        // Remove interface exports from this file
        // (we'd need to track which file each export came from to do this properly,
        // but for now we'll just rebuild exports when the file is re-indexed)
    }

    /// Get all files in this package.
    pub fn files(&self) -> &HashSet<String> {
        &self.files
    }

    /// Update definitions for a file from its symbol index.
    pub fn update_file_definitions(&mut self, uri: &str, index: &SymbolIndex) {
        // Remove old definitions from this file
        for defs in self.definitions.values_mut() {
            defs.retain(|d| d.uri != uri);
        }

        // Add new definitions
        for def in index.definitions() {
            let global_def = GlobalDefinition::from_definition(def, uri);

            self.definitions
                .entry(def.name.to_string())
                .or_default()
                .push(global_def);

            // Track interface exports
            if def.kind == DefinitionKind::Interface {
                // For now, we don't track what's exported from each interface.
                // This would require parsing the interface items.
            } else if let Some(parent) = &def.parent {
                // This is a type/func inside an interface - it's an export
                self.interface_exports
                    .entry(parent.to_string())
                    .or_default()
                    .insert(def.name.to_string());
            }
        }

        // Clean up empty entries
        self.definitions.retain(|_, v| !v.is_empty());
    }

    /// Find a definition by name. Returns the first match.
    pub fn find_definition(&self, name: &str) -> Option<&GlobalDefinition> {
        self.definitions.get(name).and_then(|defs| defs.first())
    }

    /// Find all definitions with the given name.
    pub fn find_all_definitions(&self, name: &str) -> Vec<GlobalDefinition> {
        self.definitions
            .get(name)
            .map(|defs| defs.clone())
            .unwrap_or_default()
    }

    /// Get all definitions in this package.
    pub fn all_definitions(&self) -> Vec<GlobalDefinition> {
        self.definitions.values().flatten().cloned().collect()
    }

    /// Check if an interface exports a given name.
    pub fn interface_exports(&self, interface: &str, name: &str) -> bool {
        self.interface_exports
            .get(interface)
            .is_some_and(|exports| exports.contains(name))
    }

    /// Get all exports from an interface.
    pub fn get_interface_exports(&self, interface: &str) -> Option<&HashSet<String>> {
        self.interface_exports.get(interface)
    }

    /// Get all interface names in this package.
    pub fn interfaces(&self) -> Vec<String> {
        self.definitions
            .values()
            .flatten()
            .filter(|d| d.kind == DefinitionKind::Interface)
            .map(|d| d.name.to_string())
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;
    use witcraft_syntax::{Definition, TextRange, parse};

    fn make_definition(name: &str, kind: DefinitionKind) -> Definition {
        Definition {
            name: Arc::from(name),
            kind,
            range: TextRange::new(0, 10),
            name_range: TextRange::new(0, name.len() as u32),
            parent: None,
        }
    }

    #[test]
    fn test_package_index_basic() {
        let mut index = PackageIndex::new();
        index.add_file("file:///types.wit".into());

        assert!(index.files().contains("file:///types.wit"));
    }

    #[test]
    fn test_package_index_with_parse() {
        let source = r#"interface types {
            record user {
                id: u64,
                name: string,
            }
        }"#;

        let result = parse(source);
        let symbol_index = witcraft_syntax::SymbolIndex::build(&result.root);

        let mut pkg_index = PackageIndex::new();
        pkg_index.add_file("file:///types.wit".into());
        pkg_index.update_file_definitions("file:///types.wit", &symbol_index);

        // Should find the interface and record
        assert!(pkg_index.find_definition("types").is_some());
        assert!(pkg_index.find_definition("user").is_some());

        let user_def = pkg_index.find_definition("user").unwrap();
        assert_eq!(user_def.uri, "file:///types.wit");
        assert_eq!(user_def.kind, DefinitionKind::Record);
    }

    #[test]
    fn test_package_index_cross_file() {
        let types_source = r#"interface types {
            record user {
                id: u64,
            }
        }"#;

        let api_source = r#"interface api {
            get-user: func(id: u64) -> user;
        }"#;

        let types_result = parse(types_source);
        let types_index = witcraft_syntax::SymbolIndex::build(&types_result.root);

        let api_result = parse(api_source);
        let api_index = witcraft_syntax::SymbolIndex::build(&api_result.root);

        let mut pkg_index = PackageIndex::new();
        pkg_index.add_file("file:///types.wit".into());
        pkg_index.add_file("file:///api.wit".into());
        pkg_index.update_file_definitions("file:///types.wit", &types_index);
        pkg_index.update_file_definitions("file:///api.wit", &api_index);

        // Should find definitions from both files
        assert!(pkg_index.find_definition("types").is_some());
        assert!(pkg_index.find_definition("user").is_some());
        assert!(pkg_index.find_definition("api").is_some());
        assert!(pkg_index.find_definition("get-user").is_some());

        // Verify file URIs are correct
        let user_def = pkg_index.find_definition("user").unwrap();
        assert_eq!(user_def.uri, "file:///types.wit");

        let api_def = pkg_index.find_definition("api").unwrap();
        assert_eq!(api_def.uri, "file:///api.wit");
    }

    #[test]
    fn test_remove_file() {
        let source = r#"interface types {
            record user { id: u64, }
        }"#;

        let result = parse(source);
        let symbol_index = witcraft_syntax::SymbolIndex::build(&result.root);

        let mut pkg_index = PackageIndex::new();
        pkg_index.add_file("file:///types.wit".into());
        pkg_index.update_file_definitions("file:///types.wit", &symbol_index);

        assert!(pkg_index.find_definition("user").is_some());

        pkg_index.remove_file("file:///types.wit");

        assert!(pkg_index.find_definition("user").is_none());
        assert!(!pkg_index.files().contains("file:///types.wit"));
    }
}
