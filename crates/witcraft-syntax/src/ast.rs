use crate::TextRange;
use smallvec::SmallVec;
use std::sync::Arc;

/// SmallVec for very small collections (gates, namespace segments).
pub type SmallVec2<T> = SmallVec<[T; 2]>;
/// SmallVec for small collections (params, use names).
pub type SmallVec4<T> = SmallVec<[T; 4]>;
/// SmallVec for medium collections (record fields, enum/variant cases).
pub type SmallVec8<T> = SmallVec<[T; 8]>;

/// Feature gate annotation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Gate {
    /// @since(version = x.y.z)
    Since(SinceGate),
    /// @unstable(feature = name)
    Unstable(UnstableGate),
    /// @deprecated(version = x.y.z)
    Deprecated(DeprecatedGate),
}

impl Gate {
    pub fn range(&self) -> TextRange {
        match self {
            Gate::Since(g) => g.range,
            Gate::Unstable(g) => g.range,
            Gate::Deprecated(g) => g.range,
        }
    }
}

/// @since(version = x.y.z)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SinceGate {
    pub version: Version,
    pub range: TextRange,
}

/// @unstable(feature = name)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnstableGate {
    pub feature: Ident,
    pub range: TextRange,
}

/// @deprecated(version = x.y.z)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DeprecatedGate {
    pub version: Version,
    pub range: TextRange,
}

/// Collection of gates that can be attached to an item.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Gates {
    pub gates: SmallVec2<Gate>,
}

impl Gates {
    pub fn is_empty(&self) -> bool {
        self.gates.is_empty()
    }
}

/// An identifier with its source location.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident {
    pub name: Arc<str>,
    pub range: TextRange,
}

impl Ident {
    pub fn new(name: impl AsRef<str>, range: TextRange) -> Self {
        Self {
            name: Arc::from(name.as_ref()),
            range,
        }
    }
}

/// A documentation comment attached to an item.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DocComment {
    pub text: String,
    pub range: TextRange,
}

/// Root of the AST - represents a single .wit file.
#[derive(Debug, Clone, PartialEq)]
pub struct SourceFile {
    pub package: Option<PackageDecl>,
    pub uses: Vec<TopLevelUse>,
    pub items: Vec<Item>,
    pub nested_packages: Vec<NestedPackage>,
    pub range: TextRange,
}

impl Default for SourceFile {
    fn default() -> Self {
        Self {
            package: None,
            uses: Vec::new(),
            items: Vec::new(),
            nested_packages: Vec::new(),
            range: TextRange::default(),
        }
    }
}

/// Package declaration: `package foo:bar@1.0.0;`
///
/// With nested namespaces (🪺 feature):
/// - `package foo:bar:baz` has namespace `[foo, bar]` and name `baz`
/// - `package foo:bar/quux` has namespace `[foo]`, name `bar`, nested `[quux]`
/// - `package foo:bar:baz/quux/deep` combines both
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PackageDecl {
    pub docs: Option<DocComment>,
    /// Namespace segments (at least one). E.g., `foo:bar:baz` has `[foo, bar]`.
    pub namespace: SmallVec2<Ident>,
    /// Package name. E.g., `foo:bar:baz` has name `baz`.
    pub name: Ident,
    /// Nested package segments (zero or more). E.g., `foo:bar/quux/deep` has `[quux, deep]`.
    pub nested: SmallVec2<Ident>,
    pub version: Option<Version>,
    pub range: TextRange,
}

/// Semantic version: `1.0.0`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Version {
    pub major: u32,
    pub minor: u32,
    pub patch: u32,
    pub range: TextRange,
}

/// Nested package definition: `package foo:bar { ... }`
#[derive(Debug, Clone, PartialEq)]
pub struct NestedPackage {
    pub package: PackageDecl,
    pub uses: Vec<TopLevelUse>,
    pub items: Vec<Item>,
    pub range: TextRange,
}

/// Top-level use: `use foo:bar/baz@1.0.0;`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TopLevelUse {
    pub docs: Option<DocComment>,
    pub path: UsePath,
    pub range: TextRange,
}

/// A use path: `foo:bar/interface-name@1.0.0`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UsePath {
    pub namespace: Option<Ident>,
    pub package: Option<Ident>,
    pub name: Ident,
    pub version: Option<Version>,
    pub range: TextRange,
}

/// Top-level item in a WIT file.
#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Interface(InterfaceDecl),
    World(WorldDecl),
    TypeDef(TypeDef),
}

impl Item {
    pub fn range(&self) -> TextRange {
        match self {
            Item::Interface(i) => i.range,
            Item::World(w) => w.range,
            Item::TypeDef(t) => t.range(),
        }
    }

    pub fn docs(&self) -> Option<&DocComment> {
        match self {
            Item::Interface(i) => i.docs.as_ref(),
            Item::World(w) => w.docs.as_ref(),
            Item::TypeDef(t) => t.docs(),
        }
    }

    pub fn name(&self) -> Option<&Ident> {
        match self {
            Item::Interface(i) => Some(&i.name),
            Item::World(w) => Some(&w.name),
            Item::TypeDef(t) => t.name(),
        }
    }
}

/// Interface declaration: `interface foo { ... }`
#[derive(Debug, Clone, PartialEq)]
pub struct InterfaceDecl {
    pub gates: Gates,
    pub docs: Option<DocComment>,
    pub name: Ident,
    pub items: Vec<InterfaceItem>,
    pub range: TextRange,
}

/// World declaration: `world foo { ... }`
#[derive(Debug, Clone, PartialEq)]
pub struct WorldDecl {
    pub gates: Gates,
    pub docs: Option<DocComment>,
    pub name: Ident,
    pub items: Vec<WorldItem>,
    pub range: TextRange,
}

/// Item inside an interface.
#[derive(Debug, Clone, PartialEq)]
pub enum InterfaceItem {
    TypeDef(TypeDef),
    Func(FuncDecl),
    Use(InterfaceUse),
}

impl InterfaceItem {
    pub fn range(&self) -> TextRange {
        match self {
            InterfaceItem::TypeDef(t) => t.range(),
            InterfaceItem::Func(f) => f.range,
            InterfaceItem::Use(u) => u.range,
        }
    }

    pub fn docs(&self) -> Option<&DocComment> {
        match self {
            InterfaceItem::TypeDef(t) => t.docs(),
            InterfaceItem::Func(f) => f.docs.as_ref(),
            InterfaceItem::Use(u) => u.docs.as_ref(),
        }
    }
}

/// Item inside a world.
#[derive(Debug, Clone, PartialEq)]
pub enum WorldItem {
    Import(ImportDecl),
    Export(ExportDecl),
    Include(IncludeDecl),
    TypeDef(TypeDef),
    Use(InterfaceUse),
}

impl WorldItem {
    pub fn range(&self) -> TextRange {
        match self {
            WorldItem::Import(i) => i.range,
            WorldItem::Export(e) => e.range,
            WorldItem::Include(i) => i.range,
            WorldItem::TypeDef(t) => t.range(),
            WorldItem::Use(u) => u.range,
        }
    }
}

/// Import declaration in a world: `import foo: interface { ... }`
#[derive(Debug, Clone, PartialEq)]
pub struct ImportDecl {
    pub docs: Option<DocComment>,
    pub name: Ident,
    pub kind: ExternKind,
    pub range: TextRange,
}

/// Export declaration in a world: `export foo: interface { ... }`
#[derive(Debug, Clone, PartialEq)]
pub struct ExportDecl {
    pub docs: Option<DocComment>,
    pub name: Ident,
    pub kind: ExternKind,
    pub range: TextRange,
}

/// Include declaration: `include other-world;` or `include other-world with { a as b };`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IncludeDecl {
    pub docs: Option<DocComment>,
    pub path: UsePath,
    pub with: SmallVec4<IncludeNameItem>,
    pub range: TextRange,
}

/// A rename in include's with clause: `old-name as new-name`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IncludeNameItem {
    pub name: Ident,
    pub alias: Ident,
    pub range: TextRange,
}

/// What an import/export refers to.
#[derive(Debug, Clone, PartialEq)]
pub enum ExternKind {
    /// Reference to another interface: `import foo;`
    Path(UsePath),
    /// Inline interface definition: `export bar: interface { ... }`
    Interface(Vec<InterfaceItem>),
    /// Inline function: `import baz: func(...)`
    Func(FuncSignature),
}

/// Use statement inside interface: `use types.{foo, bar};`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InterfaceUse {
    pub docs: Option<DocComment>,
    pub path: UsePath,
    pub names: SmallVec4<UseNameItem>,
    pub range: TextRange,
}

/// A single name in a use statement: `foo` or `foo as bar`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UseNameItem {
    pub name: Ident,
    pub alias: Option<Ident>,
    pub range: TextRange,
}

/// Function declaration: `foo: func(x: u32) -> string;`
#[derive(Debug, Clone, PartialEq)]
pub struct FuncDecl {
    pub gates: Gates,
    pub docs: Option<DocComment>,
    pub name: Ident,
    pub sig: FuncSignature,
    pub range: TextRange,
}

/// Function signature: `func(x: u32, y: string) -> result<T, E>` or `async func(...)`
#[derive(Debug, Clone, PartialEq)]
pub struct FuncSignature {
    pub is_async: bool,
    pub params: SmallVec4<Param>,
    pub results: FuncResults,
    pub range: TextRange,
}

/// A function parameter: `name: type`
#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub name: Ident,
    pub ty: Type,
    pub range: TextRange,
}

/// Function results.
#[derive(Debug, Clone, PartialEq)]
pub enum FuncResults {
    /// No return value
    None,
    /// Single anonymous return: `-> T`
    Anon(Type),
    /// Named returns: `-> (a: T, b: U)`
    Named(SmallVec4<Param>),
}

/// Type definitions.
#[derive(Debug, Clone, PartialEq)]
pub enum TypeDef {
    /// Type alias: `type foo = bar;`
    Alias(TypeAlias),
    /// Record type: `record foo { ... }`
    Record(RecordDecl),
    /// Variant type: `variant foo { ... }`
    Variant(VariantDecl),
    /// Enum type: `enum foo { ... }`
    Enum(EnumDecl),
    /// Flags type: `flags foo { ... }`
    Flags(FlagsDecl),
    /// Resource type: `resource foo { ... }`
    Resource(ResourceDecl),
}

impl TypeDef {
    pub fn range(&self) -> TextRange {
        match self {
            TypeDef::Alias(a) => a.range,
            TypeDef::Record(r) => r.range,
            TypeDef::Variant(v) => v.range,
            TypeDef::Enum(e) => e.range,
            TypeDef::Flags(f) => f.range,
            TypeDef::Resource(r) => r.range,
        }
    }

    pub fn docs(&self) -> Option<&DocComment> {
        match self {
            TypeDef::Alias(a) => a.docs.as_ref(),
            TypeDef::Record(r) => r.docs.as_ref(),
            TypeDef::Variant(v) => v.docs.as_ref(),
            TypeDef::Enum(e) => e.docs.as_ref(),
            TypeDef::Flags(f) => f.docs.as_ref(),
            TypeDef::Resource(r) => r.docs.as_ref(),
        }
    }

    pub fn name(&self) -> Option<&Ident> {
        match self {
            TypeDef::Alias(a) => Some(&a.name),
            TypeDef::Record(r) => Some(&r.name),
            TypeDef::Variant(v) => Some(&v.name),
            TypeDef::Enum(e) => Some(&e.name),
            TypeDef::Flags(f) => Some(&f.name),
            TypeDef::Resource(r) => Some(&r.name),
        }
    }
}

/// Type alias: `type foo = bar;`
#[derive(Debug, Clone, PartialEq)]
pub struct TypeAlias {
    pub gates: Gates,
    pub docs: Option<DocComment>,
    pub name: Ident,
    pub ty: Type,
    pub range: TextRange,
}

/// Record type: `record foo { field: type, ... }`
#[derive(Debug, Clone, PartialEq)]
pub struct RecordDecl {
    pub gates: Gates,
    pub docs: Option<DocComment>,
    pub name: Ident,
    pub fields: SmallVec8<RecordField>,
    pub range: TextRange,
}

/// A record field: `name: type`
#[derive(Debug, Clone, PartialEq)]
pub struct RecordField {
    pub docs: Option<DocComment>,
    pub name: Ident,
    pub ty: Type,
    pub range: TextRange,
}

/// Variant type: `variant foo { case1(T), case2, ... }`
#[derive(Debug, Clone, PartialEq)]
pub struct VariantDecl {
    pub gates: Gates,
    pub docs: Option<DocComment>,
    pub name: Ident,
    pub cases: SmallVec8<VariantCase>,
    pub range: TextRange,
}

/// A variant case: `name` or `name(type)`
#[derive(Debug, Clone, PartialEq)]
pub struct VariantCase {
    pub docs: Option<DocComment>,
    pub name: Ident,
    pub ty: Option<Type>,
    pub range: TextRange,
}

/// Enum type: `enum foo { a, b, c }`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumDecl {
    pub gates: Gates,
    pub docs: Option<DocComment>,
    pub name: Ident,
    pub cases: SmallVec8<EnumCase>,
    pub range: TextRange,
}

/// An enum case: just a name.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumCase {
    pub docs: Option<DocComment>,
    pub name: Ident,
    pub range: TextRange,
}

/// Flags type: `flags foo { a, b, c }`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FlagsDecl {
    pub gates: Gates,
    pub docs: Option<DocComment>,
    pub name: Ident,
    pub flags: SmallVec8<FlagCase>,
    pub range: TextRange,
}

/// A flag case: just a name.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FlagCase {
    pub docs: Option<DocComment>,
    pub name: Ident,
    pub range: TextRange,
}

/// Resource type: `resource foo { ... }`
#[derive(Debug, Clone, PartialEq)]
pub struct ResourceDecl {
    pub gates: Gates,
    pub docs: Option<DocComment>,
    pub name: Ident,
    pub items: Vec<ResourceItem>,
    pub range: TextRange,
}

/// Item inside a resource.
#[derive(Debug, Clone, PartialEq)]
pub enum ResourceItem {
    Constructor(ConstructorDecl),
    Method(MethodDecl),
    Static(StaticDecl),
}

impl ResourceItem {
    pub fn range(&self) -> TextRange {
        match self {
            ResourceItem::Constructor(c) => c.range,
            ResourceItem::Method(m) => m.range,
            ResourceItem::Static(s) => s.range,
        }
    }
}

/// Resource constructor: `constructor(x: T);` or `constructor(x: T) -> result<R>;`
#[derive(Debug, Clone, PartialEq)]
pub struct ConstructorDecl {
    pub gates: Gates,
    pub docs: Option<DocComment>,
    pub params: SmallVec4<Param>,
    pub result: Option<Type>,
    pub range: TextRange,
}

/// Resource method: `foo: func(...);`
#[derive(Debug, Clone, PartialEq)]
pub struct MethodDecl {
    pub gates: Gates,
    pub docs: Option<DocComment>,
    pub name: Ident,
    pub sig: FuncSignature,
    pub range: TextRange,
}

/// Static resource method: `foo: static func(...);`
#[derive(Debug, Clone, PartialEq)]
pub struct StaticDecl {
    pub gates: Gates,
    pub docs: Option<DocComment>,
    pub name: Ident,
    pub sig: FuncSignature,
    pub range: TextRange,
}

/// A type reference.
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    /// A named type: `foo` or `pkg:name/iface.type`
    Named(NamedType),
    /// Primitive types: `u32`, `string`, etc.
    Primitive(PrimitiveType),
    /// List type: `list<T>`
    List(Box<ListType>),
    /// Option type: `option<T>`
    Option(Box<OptionType>),
    /// Result type: `result<T, E>` or `result<_, E>` or `result<T>`
    Result(Box<ResultType>),
    /// Tuple type: `tuple<T, U, V>`
    Tuple(Box<TupleType>),
    /// Borrow handle: `borrow<T>`
    Borrow(Box<HandleType>),
    /// Own handle: `own<T>`
    Own(Box<HandleType>),
    /// Future type: `future<T>` or `future`
    Future(Box<FutureType>),
    /// Stream type: `stream<T>` or `stream`
    Stream(Box<StreamType>),
}

impl Type {
    pub fn range(&self) -> TextRange {
        match self {
            Type::Named(n) => n.range,
            Type::Primitive(p) => p.range,
            Type::List(l) => l.range,
            Type::Option(o) => o.range,
            Type::Result(r) => r.range,
            Type::Tuple(t) => t.range,
            Type::Borrow(h) => h.range,
            Type::Own(h) => h.range,
            Type::Future(f) => f.range,
            Type::Stream(s) => s.range,
        }
    }
}

/// A named type reference.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NamedType {
    pub name: Ident,
    pub range: TextRange,
}

/// Primitive types.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrimitiveKind {
    Bool,
    U8,
    U16,
    U32,
    U64,
    S8,
    S16,
    S32,
    S64,
    F32,
    F64,
    Char,
    String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PrimitiveType {
    pub kind: PrimitiveKind,
    pub range: TextRange,
}

/// List type: `list<T>`
#[derive(Debug, Clone, PartialEq)]
pub struct ListType {
    pub element: Type,
    pub range: TextRange,
}

/// Option type: `option<T>`
#[derive(Debug, Clone, PartialEq)]
pub struct OptionType {
    pub inner: Type,
    pub range: TextRange,
}

/// Result type: `result<T, E>`, `result<_, E>`, `result<T>`, or `result`
#[derive(Debug, Clone, PartialEq)]
pub struct ResultType {
    pub ok: Option<Type>,
    pub err: Option<Type>,
    pub range: TextRange,
}

/// Tuple type: `tuple<T, U, V>`
#[derive(Debug, Clone, PartialEq)]
pub struct TupleType {
    pub elements: SmallVec4<Type>,
    pub range: TextRange,
}

/// Handle type for borrow/own.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HandleType {
    pub resource: Ident,
    pub range: TextRange,
}

/// Future type: `future<T>` or `future`
#[derive(Debug, Clone, PartialEq)]
pub struct FutureType {
    pub inner: Option<Box<Type>>,
    pub range: TextRange,
}

/// Stream type: `stream<T>` or `stream`
#[derive(Debug, Clone, PartialEq)]
pub struct StreamType {
    pub inner: Option<Box<Type>>,
    pub range: TextRange,
}
