//! Workspace management for cross-file analysis.
//!
//! Provides package-level symbol resolution to enable cross-file go-to-definition,
//! find-references, and accurate "undefined type" diagnostics.

#![allow(dead_code)]

mod package_index;
mod resolver;

pub use package_index::PackageIndex;
pub use resolver::{AvailableType, CrossFileResolver, ResolveResult, SimilarName, UndefinedType};

use dashmap::DashMap;
use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tracing::info;
use witcraft_syntax::{GlobalDefinition, PackageId, SymbolIndex};

/// Manages workspace-level state including package indices and file mappings.
#[derive(Debug)]
pub struct WorkspaceManager {
    /// Map from package directory to its index.
    packages: DashMap<PathBuf, PackageIndex>,
    /// Map from file URI to its package directory.
    file_to_package: DashMap<String, PathBuf>,
    /// Root folders of the workspace.
    root_folders: Vec<PathBuf>,
}

impl WorkspaceManager {
    pub fn new() -> Self {
        Self {
            packages: DashMap::new(),
            file_to_package: DashMap::new(),
            root_folders: Vec::new(),
        }
    }

    /// Initialize the workspace with the given root folders.
    pub fn initialize(&mut self, folders: Vec<PathBuf>) {
        self.root_folders = folders.clone();

        for folder in folders {
            self.scan_folder(&folder);
        }
    }

    /// Scan a folder for .wit files and group them into packages.
    fn scan_folder(&self, folder: &Path) {
        info!("Scanning folder: {:?}", folder);

        let wit_files = find_wit_files(folder);
        info!("Found {} .wit files", wit_files.len());

        // Group files by their parent directory (each directory is a package)
        let mut dirs: HashSet<PathBuf> = HashSet::new();
        for file in &wit_files {
            if let Some(parent) = file.parent() {
                dirs.insert(parent.to_path_buf());
            }
        }

        for dir in dirs {
            self.ensure_package_index(&dir);

            // Register all .wit files in this directory with the package
            for file in &wit_files {
                if file.parent() == Some(dir.as_path()) {
                    let uri = path_to_uri(file);
                    self.file_to_package.insert(uri.clone(), dir.clone());
                    if let Some(mut pkg) = self.packages.get_mut(&dir) {
                        pkg.add_file(uri);
                    }
                }
            }
        }
    }

    /// Ensure a package index exists for the given directory.
    fn ensure_package_index(&self, dir: &Path) {
        if !self.packages.contains_key(dir) {
            self.packages.insert(dir.to_path_buf(), PackageIndex::new());
        }
    }

    /// Get the package directory for a file URI.
    pub fn package_for_file(&self, uri: &str) -> Option<PathBuf> {
        self.file_to_package.get(uri).map(|r| r.clone())
    }

    /// Get the package index for a file URI.
    pub fn package_index_for_file(&self, uri: &str) -> Option<dashmap::mapref::one::Ref<'_, PathBuf, PackageIndex>> {
        let pkg_dir = self.file_to_package.get(uri)?;
        self.packages.get(pkg_dir.value())
    }

    /// Get a mutable reference to the package index for a file URI.
    pub fn package_index_for_file_mut(&self, uri: &str) -> Option<dashmap::mapref::one::RefMut<'_, PathBuf, PackageIndex>> {
        let pkg_dir = self.file_to_package.get(uri)?;
        self.packages.get_mut(pkg_dir.value())
    }

    /// Update definitions for a file from its symbol index.
    pub fn update_file_definitions(&self, uri: &str, index: &SymbolIndex, package_id: Option<PackageId>) {
        // Ensure file is registered with a package
        if self.file_to_package.get(uri).is_none() {
            // Infer package directory from URI
            if let Some(path) = uri_to_path(uri) {
                if let Some(parent) = path.parent() {
                    self.ensure_package_index(parent);
                    self.file_to_package.insert(uri.to_string(), parent.to_path_buf());
                    if let Some(mut pkg) = self.packages.get_mut(parent) {
                        pkg.add_file(uri.to_string());
                    }
                }
            }
        }

        // Update the package index
        if let Some(mut pkg) = self.package_index_for_file_mut(uri) {
            if let Some(id) = package_id {
                pkg.set_package_id(id);
            }
            pkg.update_file_definitions(uri, index);
        }
    }

    /// Remove a file from the workspace.
    pub fn remove_file(&self, uri: &str) {
        if let Some((_, pkg_dir)) = self.file_to_package.remove(uri) {
            if let Some(mut pkg) = self.packages.get_mut(&pkg_dir) {
                pkg.remove_file(uri);
            }
        }
    }

    /// Get all file URIs in the same package as the given file.
    pub fn files_in_same_package(&self, uri: &str) -> Vec<String> {
        if let Some(pkg) = self.package_index_for_file(uri) {
            pkg.files().iter().cloned().collect()
        } else {
            vec![]
        }
    }

    /// Find a definition by name across all files in the same package.
    pub fn find_definition(&self, uri: &str, name: &str) -> Option<GlobalDefinition> {
        self.package_index_for_file(uri)?.find_definition(name).cloned()
    }

    /// Find all definitions with the given name in the package.
    pub fn find_all_definitions(&self, uri: &str, name: &str) -> Vec<GlobalDefinition> {
        if let Some(pkg) = self.package_index_for_file(uri) {
            pkg.find_all_definitions(name)
        } else {
            vec![]
        }
    }

    /// Get all definitions in the package.
    pub fn all_definitions(&self, uri: &str) -> Vec<GlobalDefinition> {
        if let Some(pkg) = self.package_index_for_file(uri) {
            pkg.all_definitions()
        } else {
            vec![]
        }
    }
}

impl Default for WorkspaceManager {
    fn default() -> Self {
        Self::new()
    }
}

pub type SharedWorkspaceManager = Arc<WorkspaceManager>;

/// Find all .wit files recursively in a directory.
fn find_wit_files(dir: &Path) -> Vec<PathBuf> {
    let mut files = Vec::new();

    if let Ok(entries) = std::fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                // Skip hidden directories and common non-source directories
                if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
                    if !name.starts_with('.') && name != "target" && name != "node_modules" {
                        files.extend(find_wit_files(&path));
                    }
                }
            } else if path.extension().is_some_and(|ext| ext == "wit") {
                files.push(path);
            }
        }
    }

    files
}

/// Convert a file path to a file:// URI.
fn path_to_uri(path: &Path) -> String {
    format!("file://{}", path.display())
}

/// Convert a file:// URI to a path.
fn uri_to_path(uri: &str) -> Option<PathBuf> {
    if uri.starts_with("file://") {
        Some(PathBuf::from(&uri[7..]))
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_uri_conversion() {
        let path = PathBuf::from("/home/user/project/types.wit");
        let uri = path_to_uri(&path);
        assert_eq!(uri, "file:///home/user/project/types.wit");

        let back = uri_to_path(&uri).unwrap();
        assert_eq!(back, path);
    }

    #[test]
    fn test_workspace_manager_new() {
        let manager = WorkspaceManager::new();
        assert!(manager.root_folders.is_empty());
        assert_eq!(manager.packages.len(), 0);
    }
}
