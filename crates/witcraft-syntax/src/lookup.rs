use crate::TextRange;
use crate::ast::*;

/// A reference to an AST node found at a position.
///
/// Used for hover, go-to-definition, and other position-based queries.
#[derive(Debug, Clone)]
pub enum NodeRef<'a> {
    // Declarations
    Package(&'a PackageDecl),
    Interface(&'a InterfaceDecl),
    World(&'a WorldDecl),
    Func(&'a FuncDecl),
    TypeAlias(&'a TypeAlias),
    Record(&'a RecordDecl),
    Variant(&'a VariantDecl),
    Enum(&'a EnumDecl),
    Flags(&'a FlagsDecl),
    Resource(&'a ResourceDecl),

    // Fields/cases
    RecordField(&'a RecordField),
    VariantCase(&'a VariantCase),
    EnumCase(&'a EnumCase),
    FlagCase(&'a FlagCase),
    Param(&'a Param),

    // Resource items
    Constructor(&'a ConstructorDecl),
    Method(&'a MethodDecl),
    Static(&'a StaticDecl),

    // Types
    Type(&'a Type),
    NamedType(&'a NamedType),

    // Other
    Ident(&'a Ident),
    UsePath(&'a UsePath),
    Import(&'a ImportDecl),
    Export(&'a ExportDecl),
}

impl<'a> NodeRef<'a> {
    /// Get the range of this node.
    pub fn range(&self) -> TextRange {
        match self {
            NodeRef::Package(n) => n.range,
            NodeRef::Interface(n) => n.range,
            NodeRef::World(n) => n.range,
            NodeRef::Func(n) => n.range,
            NodeRef::TypeAlias(n) => n.range,
            NodeRef::Record(n) => n.range,
            NodeRef::Variant(n) => n.range,
            NodeRef::Enum(n) => n.range,
            NodeRef::Flags(n) => n.range,
            NodeRef::Resource(n) => n.range,
            NodeRef::RecordField(n) => n.range,
            NodeRef::VariantCase(n) => n.range,
            NodeRef::EnumCase(n) => n.range,
            NodeRef::FlagCase(n) => n.range,
            NodeRef::Param(n) => n.range,
            NodeRef::Constructor(n) => n.range,
            NodeRef::Method(n) => n.range,
            NodeRef::Static(n) => n.range,
            NodeRef::Type(n) => n.range(),
            NodeRef::NamedType(n) => n.range,
            NodeRef::Ident(n) => n.range,
            NodeRef::UsePath(n) => n.range,
            NodeRef::Import(n) => n.range,
            NodeRef::Export(n) => n.range,
        }
    }

    /// Get documentation for this node, if any.
    pub fn docs(&self) -> Option<&'a DocComment> {
        match self {
            NodeRef::Package(n) => n.docs.as_ref(),
            NodeRef::Interface(n) => n.docs.as_ref(),
            NodeRef::World(n) => n.docs.as_ref(),
            NodeRef::Func(n) => n.docs.as_ref(),
            NodeRef::TypeAlias(n) => n.docs.as_ref(),
            NodeRef::Record(n) => n.docs.as_ref(),
            NodeRef::Variant(n) => n.docs.as_ref(),
            NodeRef::Enum(n) => n.docs.as_ref(),
            NodeRef::Flags(n) => n.docs.as_ref(),
            NodeRef::Resource(n) => n.docs.as_ref(),
            NodeRef::RecordField(n) => n.docs.as_ref(),
            NodeRef::VariantCase(n) => n.docs.as_ref(),
            NodeRef::EnumCase(n) => n.docs.as_ref(),
            NodeRef::FlagCase(n) => n.docs.as_ref(),
            NodeRef::Constructor(n) => n.docs.as_ref(),
            NodeRef::Method(n) => n.docs.as_ref(),
            NodeRef::Static(n) => n.docs.as_ref(),
            NodeRef::Import(n) => n.docs.as_ref(),
            NodeRef::Export(n) => n.docs.as_ref(),
            _ => None,
        }
    }
}

/// Find the most specific node at a given offset.
pub fn node_at(source: &SourceFile, offset: u32) -> Option<NodeRef<'_>> {
    if let Some(pkg) = &source.package {
        if pkg.range.contains(offset) {
            for ns in &pkg.namespace {
                if ns.range.contains(offset) {
                    return Some(NodeRef::Ident(ns));
                }
            }
            if pkg.name.range.contains(offset) {
                return Some(NodeRef::Ident(&pkg.name));
            }
            for nested in &pkg.nested {
                if nested.range.contains(offset) {
                    return Some(NodeRef::Ident(nested));
                }
            }
            return Some(NodeRef::Package(pkg));
        }
    }

    for item in &source.items {
        if !item.range().contains(offset) {
            continue;
        }

        match item {
            Item::Interface(iface) => {
                if let Some(node) = node_at_interface(iface, offset) {
                    return Some(node);
                }
                return Some(NodeRef::Interface(iface));
            }
            Item::World(world) => {
                if let Some(node) = node_at_world(world, offset) {
                    return Some(node);
                }
                return Some(NodeRef::World(world));
            }
            Item::TypeDef(typedef) => {
                if let Some(node) = node_at_typedef(typedef, offset) {
                    return Some(node);
                }
            }
        }
    }

    None
}

fn node_at_interface<'a>(iface: &'a InterfaceDecl, offset: u32) -> Option<NodeRef<'a>> {
    if iface.name.range.contains(offset) {
        return Some(NodeRef::Ident(&iface.name));
    }

    for item in &iface.items {
        if !item.range().contains(offset) {
            continue;
        }

        match item {
            InterfaceItem::TypeDef(td) => {
                return node_at_typedef(td, offset);
            }
            InterfaceItem::Func(func) => {
                if let Some(node) = node_at_func(func, offset) {
                    return Some(node);
                }
                return Some(NodeRef::Func(func));
            }
            InterfaceItem::Use(u) => {
                if u.path.range.contains(offset) {
                    return Some(NodeRef::UsePath(&u.path));
                }
            }
        }
    }

    None
}

fn node_at_world<'a>(world: &'a WorldDecl, offset: u32) -> Option<NodeRef<'a>> {
    if world.name.range.contains(offset) {
        return Some(NodeRef::Ident(&world.name));
    }

    for item in &world.items {
        if !item.range().contains(offset) {
            continue;
        }

        match item {
            WorldItem::Import(imp) => {
                if imp.name.range.contains(offset) {
                    return Some(NodeRef::Ident(&imp.name));
                }
                return Some(NodeRef::Import(imp));
            }
            WorldItem::Export(exp) => {
                if exp.name.range.contains(offset) {
                    return Some(NodeRef::Ident(&exp.name));
                }
                return Some(NodeRef::Export(exp));
            }
            WorldItem::TypeDef(td) => {
                return node_at_typedef(td, offset);
            }
            WorldItem::Include(_) | WorldItem::Use(_) => {}
        }
    }

    None
}

fn node_at_typedef<'a>(td: &'a TypeDef, offset: u32) -> Option<NodeRef<'a>> {
    match td {
        TypeDef::Alias(alias) => {
            if alias.name.range.contains(offset) {
                return Some(NodeRef::Ident(&alias.name));
            }
            if let Some(node) = node_at_type(&alias.ty, offset) {
                return Some(node);
            }
            Some(NodeRef::TypeAlias(alias))
        }
        TypeDef::Record(rec) => {
            if rec.name.range.contains(offset) {
                return Some(NodeRef::Ident(&rec.name));
            }
            for field in &rec.fields {
                if field.range.contains(offset) {
                    if field.name.range.contains(offset) {
                        return Some(NodeRef::Ident(&field.name));
                    }
                    if let Some(node) = node_at_type(&field.ty, offset) {
                        return Some(node);
                    }
                    return Some(NodeRef::RecordField(field));
                }
            }
            Some(NodeRef::Record(rec))
        }
        TypeDef::Variant(var) => {
            if var.name.range.contains(offset) {
                return Some(NodeRef::Ident(&var.name));
            }
            for case in &var.cases {
                if case.range.contains(offset) {
                    if case.name.range.contains(offset) {
                        return Some(NodeRef::Ident(&case.name));
                    }
                    if let Some(ty) = &case.ty {
                        if let Some(node) = node_at_type(ty, offset) {
                            return Some(node);
                        }
                    }
                    return Some(NodeRef::VariantCase(case));
                }
            }
            Some(NodeRef::Variant(var))
        }
        TypeDef::Enum(e) => {
            if e.name.range.contains(offset) {
                return Some(NodeRef::Ident(&e.name));
            }
            for case in &e.cases {
                if case.range.contains(offset) {
                    return Some(NodeRef::EnumCase(case));
                }
            }
            Some(NodeRef::Enum(e))
        }
        TypeDef::Flags(f) => {
            if f.name.range.contains(offset) {
                return Some(NodeRef::Ident(&f.name));
            }
            for flag in &f.flags {
                if flag.range.contains(offset) {
                    return Some(NodeRef::FlagCase(flag));
                }
            }
            Some(NodeRef::Flags(f))
        }
        TypeDef::Resource(res) => {
            if res.name.range.contains(offset) {
                return Some(NodeRef::Ident(&res.name));
            }
            for item in &res.items {
                if item.range().contains(offset) {
                    match item {
                        ResourceItem::Constructor(c) => {
                            for param in &c.params {
                                if let Some(node) = node_at_param(param, offset) {
                                    return Some(node);
                                }
                            }
                            return Some(NodeRef::Constructor(c));
                        }
                        ResourceItem::Method(m) => {
                            if m.name.range.contains(offset) {
                                return Some(NodeRef::Ident(&m.name));
                            }
                            return Some(NodeRef::Method(m));
                        }
                        ResourceItem::Static(s) => {
                            if s.name.range.contains(offset) {
                                return Some(NodeRef::Ident(&s.name));
                            }
                            return Some(NodeRef::Static(s));
                        }
                    }
                }
            }
            Some(NodeRef::Resource(res))
        }
    }
}

fn node_at_func<'a>(func: &'a FuncDecl, offset: u32) -> Option<NodeRef<'a>> {
    if func.name.range.contains(offset) {
        return Some(NodeRef::Ident(&func.name));
    }

    for param in &func.sig.params {
        if let Some(node) = node_at_param(param, offset) {
            return Some(node);
        }
    }

    if let FuncResults::Anon(ty) = &func.sig.results {
        if let Some(node) = node_at_type(ty, offset) {
            return Some(node);
        }
    }

    if let FuncResults::Named(params) = &func.sig.results {
        for param in params {
            if let Some(node) = node_at_param(param, offset) {
                return Some(node);
            }
        }
    }

    None
}

fn node_at_param<'a>(param: &'a Param, offset: u32) -> Option<NodeRef<'a>> {
    if !param.range.contains(offset) {
        return None;
    }

    if param.name.range.contains(offset) {
        return Some(NodeRef::Ident(&param.name));
    }

    if let Some(node) = node_at_type(&param.ty, offset) {
        return Some(node);
    }

    Some(NodeRef::Param(param))
}

fn node_at_type<'a>(ty: &'a Type, offset: u32) -> Option<NodeRef<'a>> {
    if !ty.range().contains(offset) {
        return None;
    }

    match ty {
        Type::Named(named) => {
            if named.name.range.contains(offset) {
                return Some(NodeRef::Ident(&named.name));
            }
            Some(NodeRef::NamedType(named))
        }
        Type::List(list) => node_at_type(&list.element, offset).or(Some(NodeRef::Type(ty))),
        Type::Option(opt) => node_at_type(&opt.inner, offset).or(Some(NodeRef::Type(ty))),
        Type::Result(res) => {
            if let Some(ok) = &res.ok {
                if let Some(node) = node_at_type(ok, offset) {
                    return Some(node);
                }
            }
            if let Some(err) = &res.err {
                if let Some(node) = node_at_type(err, offset) {
                    return Some(node);
                }
            }
            Some(NodeRef::Type(ty))
        }
        Type::Tuple(tuple) => {
            for elem in &tuple.elements {
                if let Some(node) = node_at_type(elem, offset) {
                    return Some(node);
                }
            }
            Some(NodeRef::Type(ty))
        }
        Type::Borrow(handle) | Type::Own(handle) => {
            if handle.resource.range.contains(offset) {
                return Some(NodeRef::Ident(&handle.resource));
            }
            Some(NodeRef::Type(ty))
        }
        Type::Primitive(_) => Some(NodeRef::Type(ty)),
        Type::Future(future) => {
            if let Some(inner) = &future.inner {
                if let Some(node) = node_at_type(inner, offset) {
                    return Some(node);
                }
            }
            Some(NodeRef::Type(ty))
        }
        Type::Stream(stream) => {
            if let Some(inner) = &stream.inner {
                if let Some(node) = node_at_type(inner, offset) {
                    return Some(node);
                }
            }
            Some(NodeRef::Type(ty))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn node_at_empty() {
        let source = SourceFile::default();
        assert!(node_at(&source, 0).is_none());
    }
}
