//! Symbol index for WIT files.
//!
//! Provides definition tracking and lookup for go-to-definition and other features.

use crate::TextRange;
use crate::ast::*;
use std::collections::HashMap;
use std::sync::Arc;

/// Identifies a WIT package by namespace, name, and optional version.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PackageId {
    pub namespace: String,
    pub name: String,
    pub version: Option<(u32, u32, u32)>,
}

impl PackageId {
    pub fn new(namespace: impl Into<String>, name: impl Into<String>) -> Self {
        Self {
            namespace: namespace.into(),
            name: name.into(),
            version: None,
        }
    }

    pub fn with_version(mut self, major: u32, minor: u32, patch: u32) -> Self {
        self.version = Some((major, minor, patch));
        self
    }

    /// Parse from a package declaration.
    pub fn from_package_decl(decl: &PackageDecl) -> Self {
        let namespace = decl
            .namespace
            .iter()
            .map(|i| &*i.name)
            .collect::<Vec<_>>()
            .join(":");
        Self {
            namespace,
            name: decl.name.name.to_string(),
            version: decl.version.as_ref().map(|v| (v.major, v.minor, v.patch)),
        }
    }
}

impl std::fmt::Display for PackageId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.namespace, self.name)?;
        if let Some((major, minor, patch)) = self.version {
            write!(f, "@{}.{}.{}", major, minor, patch)?;
        }
        Ok(())
    }
}

/// A symbol definition with file location for cross-file lookups.
#[derive(Debug, Clone)]
pub struct GlobalDefinition {
    pub name: Arc<str>,
    pub kind: DefinitionKind,
    pub range: TextRange,
    pub name_range: TextRange,
    pub parent: Option<Arc<str>>,
    /// URI of the file containing this definition.
    pub uri: String,
}

impl GlobalDefinition {
    /// Convert a local Definition to a GlobalDefinition by adding the URI.
    pub fn from_definition(def: &Definition, uri: impl Into<String>) -> Self {
        Self {
            name: def.name.clone(),
            kind: def.kind,
            range: def.range,
            name_range: def.name_range,
            parent: def.parent.clone(),
            uri: uri.into(),
        }
    }
}

/// Kind of symbol definition.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DefinitionKind {
    Interface,
    World,
    TypeAlias,
    Record,
    Variant,
    Enum,
    Flags,
    Resource,
    Function,
}

/// A symbol definition.
#[derive(Debug, Clone)]
pub struct Definition {
    pub name: Arc<str>,
    pub kind: DefinitionKind,
    pub range: TextRange,
    /// The range of just the name (for highlighting).
    pub name_range: TextRange,
    /// Parent scope (e.g., interface name for functions/types).
    pub parent: Option<Arc<str>>,
}

/// A symbol reference (where a symbol is used).
#[derive(Debug, Clone)]
pub struct Reference {
    pub name: Arc<str>,
    pub range: TextRange,
    pub kind: ReferenceKind,
}

/// Kind of reference.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReferenceKind {
    /// A type reference (e.g., in `x: my-type`)
    Type,
    /// An interface reference (e.g., in `use iface.{...}`)
    Interface,
}

/// An imported name from a `use` statement.
#[derive(Debug, Clone)]
pub struct Import {
    /// The name as it appears in the importing scope (may be aliased).
    pub local_name: Arc<str>,
    /// The original name in the source interface.
    pub original_name: Arc<str>,
    /// The interface being imported from.
    pub from_interface: Arc<str>,
    /// Range of the local name in the `use` statement.
    pub range: TextRange,
    /// Range of the original name in the `use` statement (before `as` if aliased).
    pub original_name_range: TextRange,
    /// Range of the full import item (name and optional alias).
    pub item_range: TextRange,
    /// Range of the entire `use` statement.
    pub use_statement_range: TextRange,
}

/// Symbol index built from a parsed WIT file.
#[derive(Debug, Default)]
pub struct SymbolIndex {
    definitions: Vec<Definition>,
    def_by_name: HashMap<Arc<str>, usize>,
    references: Vec<Reference>,
    /// Names imported via `use` statements.
    imports: Vec<Import>,
}

impl SymbolIndex {
    /// Build an index from a parsed source file.
    pub fn build(source: &SourceFile) -> Self {
        let mut index = Self::default();
        index.index_source_file(source);
        index
    }

    /// Find definition by name. O(1) lookup via HashMap.
    pub fn find_definition(&self, name: &str) -> Option<&Definition> {
        self.def_by_name
            .get(name)
            .map(|&idx| &self.definitions[idx])
    }

    fn add_definition(&mut self, def: Definition) {
        let idx = self.definitions.len();
        self.def_by_name.insert(def.name.clone(), idx);
        self.definitions.push(def);
    }

    /// Find all definitions.
    pub fn definitions(&self) -> &[Definition] {
        &self.definitions
    }

    /// Find reference at a given position (byte offset).
    pub fn reference_at(&self, offset: u32) -> Option<&Reference> {
        self.references.iter().find(|r| r.range.contains(offset))
    }

    /// Find all references.
    pub fn references(&self) -> &[Reference] {
        &self.references
    }

    /// Find all imports.
    pub fn imports(&self) -> &[Import] {
        &self.imports
    }

    /// Check if a name is available in scope (either defined locally or imported).
    pub fn is_name_in_scope(&self, name: &str) -> bool {
        self.find_definition(name).is_some() || self.find_import(name).is_some()
    }

    /// Find an import by its local name.
    pub fn find_import(&self, local_name: &str) -> Option<&Import> {
        self.imports.iter().find(|i| &*i.local_name == local_name)
    }

    pub fn definition_at(&self, offset: u32) -> Option<&Definition> {
        if let Some(reference) = self.reference_at(offset) {
            return self.find_definition(&reference.name);
        }
        self.definitions
            .iter()
            .find(|d| d.name_range.contains(offset))
    }

    fn index_source_file(&mut self, source: &SourceFile) {
        for item in &source.items {
            self.index_item(item);
        }
    }

    fn index_item(&mut self, item: &Item) {
        match item {
            Item::Interface(iface) => self.index_interface(iface),
            Item::World(world) => self.index_world(world),
            Item::TypeDef(typedef) => self.index_typedef(typedef, None),
        }
    }

    fn index_interface(&mut self, iface: &InterfaceDecl) {
        self.add_definition(Definition {
            name: iface.name.name.clone(),
            kind: DefinitionKind::Interface,
            range: iface.range,
            name_range: iface.name.range,
            parent: None,
        });

        let parent = Some(iface.name.name.clone());
        for item in &iface.items {
            match item {
                InterfaceItem::TypeDef(typedef) => self.index_typedef(typedef, parent.clone()),
                InterfaceItem::Func(func) => self.index_func(func, parent.clone()),
                InterfaceItem::Use(use_stmt) => self.index_interface_use(use_stmt),
            }
        }
    }

    fn index_world(&mut self, world: &WorldDecl) {
        self.add_definition(Definition {
            name: world.name.name.clone(),
            kind: DefinitionKind::World,
            range: world.range,
            name_range: world.name.range,
            parent: None,
        });

        let parent = Some(world.name.name.clone());
        for item in &world.items {
            match item {
                WorldItem::TypeDef(typedef) => self.index_typedef(typedef, parent.clone()),
                WorldItem::Use(use_stmt) => self.index_interface_use(use_stmt),
                WorldItem::Import(import) => self.index_extern_kind(&import.kind),
                WorldItem::Export(export) => self.index_extern_kind(&export.kind),
                WorldItem::Include(_) => {}
            }
        }
    }

    fn index_typedef(&mut self, typedef: &TypeDef, parent: Option<Arc<str>>) {
        let (name, kind, range, name_range) = match typedef {
            TypeDef::Alias(a) => {
                self.index_type(&a.ty);
                (
                    a.name.name.clone(),
                    DefinitionKind::TypeAlias,
                    a.range,
                    a.name.range,
                )
            }
            TypeDef::Record(r) => {
                for field in &r.fields {
                    self.index_type(&field.ty);
                }
                (
                    r.name.name.clone(),
                    DefinitionKind::Record,
                    r.range,
                    r.name.range,
                )
            }
            TypeDef::Variant(v) => {
                for case in &v.cases {
                    if let Some(ty) = &case.ty {
                        self.index_type(ty);
                    }
                }
                (
                    v.name.name.clone(),
                    DefinitionKind::Variant,
                    v.range,
                    v.name.range,
                )
            }
            TypeDef::Enum(e) => (
                e.name.name.clone(),
                DefinitionKind::Enum,
                e.range,
                e.name.range,
            ),
            TypeDef::Flags(f) => (
                f.name.name.clone(),
                DefinitionKind::Flags,
                f.range,
                f.name.range,
            ),
            TypeDef::Resource(r) => {
                self.index_resource(r, parent.clone());
                (
                    r.name.name.clone(),
                    DefinitionKind::Resource,
                    r.range,
                    r.name.range,
                )
            }
        };

        self.add_definition(Definition {
            name,
            kind,
            range,
            name_range,
            parent,
        });
    }

    fn index_resource(&mut self, resource: &ResourceDecl, parent: Option<Arc<str>>) {
        let resource_parent: Arc<str> = parent
            .map(|p| Arc::from(format!("{}.{}", p, resource.name.name)))
            .unwrap_or_else(|| resource.name.name.clone());

        for item in &resource.items {
            match item {
                ResourceItem::Constructor(c) => {
                    for param in &c.params {
                        self.index_type(&param.ty);
                    }
                }
                ResourceItem::Method(m) => {
                    self.add_definition(Definition {
                        name: m.name.name.clone(),
                        kind: DefinitionKind::Function,
                        range: m.range,
                        name_range: m.name.range,
                        parent: Some(resource_parent.clone()),
                    });
                    self.index_func_signature(&m.sig);
                }
                ResourceItem::Static(s) => {
                    self.add_definition(Definition {
                        name: s.name.name.clone(),
                        kind: DefinitionKind::Function,
                        range: s.range,
                        name_range: s.name.range,
                        parent: Some(resource_parent.clone()),
                    });
                    self.index_func_signature(&s.sig);
                }
            }
        }
    }

    fn index_func(&mut self, func: &FuncDecl, parent: Option<Arc<str>>) {
        self.add_definition(Definition {
            name: func.name.name.clone(),
            kind: DefinitionKind::Function,
            range: func.range,
            name_range: func.name.range,
            parent,
        });
        self.index_func_signature(&func.sig);
    }

    fn index_func_signature(&mut self, sig: &FuncSignature) {
        for param in &sig.params {
            self.index_type(&param.ty);
        }
        match &sig.results {
            FuncResults::None => {}
            FuncResults::Anon(ty) => self.index_type(ty),
            FuncResults::Named(params) => {
                for param in params {
                    self.index_type(&param.ty);
                }
            }
        }
    }

    fn index_interface_use(&mut self, use_stmt: &InterfaceUse) {
        // Record reference to the interface being imported from
        self.references.push(Reference {
            name: use_stmt.path.name.name.clone(),
            range: use_stmt.path.name.range,
            kind: ReferenceKind::Interface,
        });

        // Record each imported name
        let from_interface = use_stmt.path.name.name.clone();
        for item in &use_stmt.names {
            let local_name = item
                .alias
                .as_ref()
                .map(|a| a.name.clone())
                .unwrap_or_else(|| item.name.name.clone());
            let range = item
                .alias
                .as_ref()
                .map(|a| a.range)
                .unwrap_or(item.name.range);

            self.imports.push(Import {
                local_name,
                original_name: item.name.name.clone(),
                from_interface: from_interface.clone(),
                range,
                original_name_range: item.name.range,
                item_range: item.range,
                use_statement_range: use_stmt.range,
            });
        }
    }

    fn index_extern_kind(&mut self, kind: &ExternKind) {
        match kind {
            ExternKind::Path(path) => {
                self.references.push(Reference {
                    name: path.name.name.clone(),
                    range: path.name.range,
                    kind: ReferenceKind::Interface,
                });
            }
            ExternKind::Interface(items) => {
                for item in items {
                    match item {
                        InterfaceItem::TypeDef(typedef) => self.index_typedef(typedef, None),
                        InterfaceItem::Func(func) => self.index_func(func, None),
                        InterfaceItem::Use(use_stmt) => self.index_interface_use(use_stmt),
                    }
                }
            }
            ExternKind::Func(sig) => {
                self.index_func_signature(sig);
            }
        }
    }

    fn index_type(&mut self, ty: &Type) {
        match ty {
            Type::Named(named) => {
                self.references.push(Reference {
                    name: named.name.name.clone(),
                    range: named.name.range,
                    kind: ReferenceKind::Type,
                });
            }
            Type::Primitive(_) => {}
            Type::List(list) => self.index_type(&list.element),
            Type::Option(opt) => self.index_type(&opt.inner),
            Type::Result(result) => {
                if let Some(ok) = &result.ok {
                    self.index_type(ok);
                }
                if let Some(err) = &result.err {
                    self.index_type(err);
                }
            }
            Type::Tuple(tuple) => {
                for element in &tuple.elements {
                    self.index_type(element);
                }
            }
            Type::Borrow(handle) => {
                self.references.push(Reference {
                    name: handle.resource.name.clone(),
                    range: handle.resource.range,
                    kind: ReferenceKind::Type,
                });
            }
            Type::Own(handle) => {
                self.references.push(Reference {
                    name: handle.resource.name.clone(),
                    range: handle.resource.range,
                    kind: ReferenceKind::Type,
                });
            }
            Type::Future(future) => {
                if let Some(inner) = &future.inner {
                    self.index_type(inner);
                }
            }
            Type::Stream(stream) => {
                if let Some(inner) = &stream.inner {
                    self.index_type(inner);
                }
            }
        }
    }

    /// Find duplicate definitions (same name in same scope).
    pub fn find_duplicate_definitions(&self) -> Vec<DuplicateDefinition> {
        use std::collections::HashMap;

        let mut by_name: HashMap<&str, Vec<&Definition>> = HashMap::new();
        for def in &self.definitions {
            by_name.entry(&def.name).or_default().push(def);
        }

        let mut duplicates = Vec::new();
        for (name, defs) in by_name {
            if defs.len() > 1 {
                // Group by parent scope to find duplicates in same scope
                let mut by_scope: HashMap<Option<&Arc<str>>, Vec<&Definition>> = HashMap::new();
                for def in defs {
                    by_scope.entry(def.parent.as_ref()).or_default().push(def);
                }

                for (_, scope_defs) in by_scope {
                    if scope_defs.len() > 1 {
                        let first = scope_defs[0];
                        for dup in &scope_defs[1..] {
                            duplicates.push(DuplicateDefinition {
                                name: name.to_string(),
                                first_range: first.name_range,
                                duplicate_range: dup.name_range,
                            });
                        }
                    }
                }
            }
        }

        duplicates
    }

    /// Find unused imports (imported but never referenced).
    pub fn find_unused_imports(&self) -> Vec<UnusedImport> {
        let mut unused = Vec::new();

        for import in &self.imports {
            // Check if the imported name is used anywhere in references
            let is_used = self
                .references
                .iter()
                .any(|r| r.name == import.local_name && r.kind == ReferenceKind::Type);

            if !is_used {
                unused.push(UnusedImport {
                    name: import.local_name.to_string(),
                    range: import.range,
                });
            }
        }

        unused
    }
}

/// A duplicate definition in the same scope.
#[derive(Debug, Clone)]
pub struct DuplicateDefinition {
    pub name: String,
    pub first_range: TextRange,
    pub duplicate_range: TextRange,
}

/// An unused import.
#[derive(Debug, Clone)]
pub struct UnusedImport {
    pub name: String,
    pub range: TextRange,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse;

    #[test]
    fn test_index_interface() {
        let source = r#"
            interface api {
                get-user: func(id: u64) -> string;
            }
        "#;
        let result = parse(source);
        let index = SymbolIndex::build(&result.root);

        assert_eq!(index.definitions().len(), 2); // interface + function

        let iface = index.find_definition("api").unwrap();
        assert_eq!(iface.kind, DefinitionKind::Interface);

        let func = index.find_definition("get-user").unwrap();
        assert_eq!(func.kind, DefinitionKind::Function);
        assert_eq!(func.parent.as_deref(), Some("api"));
    }

    #[test]
    fn test_index_type_reference() {
        let source = r#"
            interface api {
                record user {
                    name: string,
                }
                get-user: func(id: u64) -> user;
            }
        "#;
        let result = parse(source);
        let index = SymbolIndex::build(&result.root);

        // Find the reference to `user` in the return type
        let refs: Vec<_> = index
            .references()
            .iter()
            .filter(|r| &*r.name == "user")
            .collect();
        assert_eq!(refs.len(), 1);

        // Should be able to find the definition
        let def = index.find_definition("user").unwrap();
        assert_eq!(def.kind, DefinitionKind::Record);
    }

    #[test]
    fn test_definition_at_position() {
        let source = "interface api { record user { name: string, } }";
        let result = parse(source);
        let index = SymbolIndex::build(&result.root);

        // Find the position of "user" type name (around byte 23)
        let user_def = index.find_definition("user").unwrap();

        // Should find definition when clicking on the name
        let def = index.definition_at(user_def.name_range.start()).unwrap();
        assert_eq!(def.name.as_ref(), "user");
    }

    #[test]
    fn test_goto_definition_from_reference() {
        // Real-world example: clicking on `user` in `-> user` should go to the record definition
        let source = r#"interface api {
    record user {
        name: string,
    }
    get-user: func(id: u64) -> user;
}"#;
        let result = parse(source);
        let index = SymbolIndex::build(&result.root);

        // Find the reference to `user` in the return type
        let user_refs: Vec<_> = index
            .references()
            .iter()
            .filter(|r| &*r.name == "user")
            .collect();
        assert_eq!(user_refs.len(), 1);

        let user_ref = user_refs[0];

        // Clicking on the reference should find the definition
        let def = index.definition_at(user_ref.range.start()).unwrap();
        assert_eq!(def.name.as_ref(), "user");
        assert_eq!(def.kind, DefinitionKind::Record);
    }

    #[test]
    fn test_complex_example() {
        let source = r#"interface types {
    record user {
        id: u64,
        name: string,
    }
    record post {
        author: user,
    }
    variant response {
        success(user),
        error(string),
    }
}

interface api {
    get-user: func(id: u64) -> user;
}"#;
        let result = parse(source);
        let index = SymbolIndex::build(&result.root);

        // Should have definitions for: types, user, post, response, api, get-user
        assert!(index.find_definition("types").is_some());
        assert!(index.find_definition("user").is_some());
        assert!(index.find_definition("post").is_some());
        assert!(index.find_definition("response").is_some());
        assert!(index.find_definition("api").is_some());
        assert!(index.find_definition("get-user").is_some());

        // Should have references to user (in post.author, response.success, api.get-user return)
        let user_refs: Vec<_> = index
            .references()
            .iter()
            .filter(|r| &*r.name == "user")
            .collect();
        assert_eq!(user_refs.len(), 3);
    }

    #[test]
    fn test_undefined_type_detection() {
        let source = r#"interface api {
    get-user: func(id: u64) -> user;
}"#;
        let result = parse(source);
        let index = SymbolIndex::build(&result.root);

        // 'user' should be referenced but not defined
        let user_refs: Vec<_> = index
            .references()
            .iter()
            .filter(|r| &*r.name == "user" && r.kind == ReferenceKind::Type)
            .collect();
        assert_eq!(user_refs.len(), 1, "should have 1 reference to 'user'");

        // 'user' should NOT be in definitions
        assert!(
            index.find_definition("user").is_none(),
            "'user' should not be defined"
        );
    }

    #[test]
    fn test_duplicate_definitions() {
        let source = r#"interface api {
    record user { name: string, }
    record user { id: u64, }
}"#;
        let result = parse(source);
        let index = SymbolIndex::build(&result.root);

        let duplicates = index.find_duplicate_definitions();
        assert_eq!(duplicates.len(), 1);
        assert_eq!(duplicates[0].name, "user");
    }

    #[test]
    fn test_no_duplicates_different_scopes() {
        let source = r#"
interface a { record user { name: string, } }
interface b { record user { id: u64, } }
"#;
        let result = parse(source);
        let index = SymbolIndex::build(&result.root);

        // Different interfaces, so these are not duplicates
        let duplicates = index.find_duplicate_definitions();
        assert_eq!(duplicates.len(), 0);
    }

    #[test]
    fn test_unused_imports() {
        let source = r#"interface api {
    use types.{user, post};
    get-user: func(id: u64) -> user;
}"#;
        let result = parse(source);
        let index = SymbolIndex::build(&result.root);

        let unused = index.find_unused_imports();
        // 'user' is used in the return type, 'post' is not
        assert_eq!(unused.len(), 1);
        assert_eq!(unused[0].name, "post");
    }

    #[test]
    fn test_all_imports_used() {
        let source = r#"interface api {
    use types.{user, post};
    get-user: func(id: u64) -> user;
    get-post: func(id: u64) -> post;
}"#;
        let result = parse(source);
        let index = SymbolIndex::build(&result.root);

        let unused = index.find_unused_imports();
        assert_eq!(unused.len(), 0);
    }
}
