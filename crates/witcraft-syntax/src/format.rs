use crate::ast::*;
use crate::lexer::Token;
use crate::parse::ParseResult;
use crate::SyntaxKind;

/// A comment extracted from source.
#[derive(Debug, Clone)]
struct Comment {
    text: String,
    start: u32,
    #[allow(dead_code)] // May be used for future features like blank line preservation
    end: u32,
}

/// Formatter for WIT source files.
pub struct Formatter<'a> {
    source: Option<&'a str>,
    comments: Vec<Comment>,
    comment_idx: usize,
    output: String,
    indent: usize,
    indent_str: &'static str,
}

impl Default for Formatter<'_> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> Formatter<'a> {
    pub fn new() -> Self {
        Self {
            source: None,
            comments: Vec::new(),
            comment_idx: 0,
            output: String::new(),
            indent: 0,
            indent_str: "  ",
        }
    }

    /// Create a formatter with source and tokens for comment preservation.
    fn with_comments(source: &'a str, tokens: &[Token]) -> Self {
        let comments = tokens
            .iter()
            .filter(|t| t.kind == SyntaxKind::LineComment || t.kind == SyntaxKind::BlockComment)
            .map(|t| Comment {
                text: t.range.slice(source).to_string(),
                start: t.range.start(),
                end: t.range.end(),
            })
            .collect();

        Self {
            source: Some(source),
            comments,
            comment_idx: 0,
            output: String::new(),
            indent: 0,
            indent_str: "  ",
        }
    }

    /// Format a source file and return the formatted string (no comment preservation).
    pub fn format(source: &SourceFile) -> String {
        let mut f = Self::new();
        f.format_source_file(source);
        f.output
    }

    /// Format with comment preservation using ParseResult.
    pub fn format_with_comments(source: &'a str, result: &ParseResult) -> String {
        let mut f = Self::with_comments(source, &result.tokens);
        f.format_source_file(&result.root);
        // Emit any trailing comments at the end
        f.emit_remaining_comments();
        f.output
    }

    fn write(&mut self, s: &str) {
        self.output.push_str(s);
    }

    fn writeln(&mut self, s: &str) {
        self.write_indent();
        self.output.push_str(s);
        self.output.push('\n');
    }

    fn newline(&mut self) {
        self.output.push('\n');
    }

    fn write_indent(&mut self) {
        for _ in 0..self.indent {
            self.output.push_str(self.indent_str);
        }
    }

    fn indent(&mut self) {
        self.indent += 1;
    }

    fn dedent(&mut self) {
        self.indent = self.indent.saturating_sub(1);
    }

    /// Emit all comments that appear before the given byte offset.
    fn emit_comments_before(&mut self, before: u32) {
        while self.comment_idx < self.comments.len() {
            if self.comments[self.comment_idx].start >= before {
                break;
            }
            let text = self.comments[self.comment_idx].text.clone();
            self.write_indent();
            self.write(&text);
            self.newline();
            self.comment_idx += 1;
        }
    }

    /// Emit any remaining comments at the end of the file.
    fn emit_remaining_comments(&mut self) {
        while self.comment_idx < self.comments.len() {
            let text = self.comments[self.comment_idx].text.clone();
            self.write_indent();
            self.write(&text);
            self.newline();
            self.comment_idx += 1;
        }
    }

    /// Check if position `a` and `b` are on the same line in the source.
    fn same_line(&self, a: u32, b: u32) -> bool {
        let Some(source) = self.source else {
            return false;
        };
        let start = a.min(b) as usize;
        let end = a.max(b) as usize;
        if end > source.len() {
            return false;
        }
        !source[start..end].contains('\n')
    }

    /// Try to emit a trailing comment (comment on same line after node_end).
    /// Returns true if a trailing comment was emitted.
    fn emit_trailing_comment(&mut self, node_end: u32) -> bool {
        if self.comment_idx >= self.comments.len() {
            return false;
        }
        let comment = &self.comments[self.comment_idx];
        // Comment must start after node_end and be on the same line
        if comment.start >= node_end && self.same_line(node_end, comment.start) {
            let text = comment.text.clone();
            self.write(" ");
            self.write(&text);
            self.comment_idx += 1;
            return true;
        }
        false
    }

    fn format_source_file(&mut self, source: &SourceFile) {
        // Package declaration
        if let Some(pkg) = &source.package {
            self.emit_comments_before(pkg.range.start());
            self.format_package_decl(pkg);
            if !source.uses.is_empty() || !source.items.is_empty() {
                self.newline();
            }
        }

        // Top-level uses
        for (i, use_stmt) in source.uses.iter().enumerate() {
            self.emit_comments_before(use_stmt.range.start());
            if i > 0 {
                // No blank lines between consecutive uses
            }
            self.format_top_level_use(use_stmt);
        }

        if !source.uses.is_empty() && !source.items.is_empty() {
            self.newline();
        }

        // Items
        for (i, item) in source.items.iter().enumerate() {
            self.emit_comments_before(item.range().start());
            if i > 0 {
                self.newline();
            }
            self.format_item(item);
        }

        // Nested packages
        for (i, nested) in source.nested_packages.iter().enumerate() {
            self.emit_comments_before(nested.range.start());
            if i > 0 || !source.items.is_empty() {
                self.newline();
            }
            self.format_nested_package(nested);
        }
    }

    fn format_doc_comment(&mut self, docs: &DocComment) {
        for line in docs.text.lines() {
            self.write_indent();
            self.write("///");
            if !line.is_empty() {
                self.write(" ");
                self.write(line);
            }
            self.newline();
        }
    }

    fn format_gates(&mut self, gates: &Gates) {
        for gate in &gates.gates {
            self.write_indent();
            match gate {
                Gate::Since(g) => {
                    self.write("@since(version = ");
                    self.format_version(&g.version);
                    self.write(")");
                }
                Gate::Unstable(g) => {
                    self.write("@unstable(feature = ");
                    self.write(&g.feature.name);
                    self.write(")");
                }
                Gate::Deprecated(g) => {
                    self.write("@deprecated(version = ");
                    self.format_version(&g.version);
                    self.write(")");
                }
            }
            self.newline();
        }
    }

    fn format_package_decl(&mut self, pkg: &PackageDecl) {
        if let Some(docs) = &pkg.docs {
            self.format_doc_comment(docs);
        }
        self.write_indent();
        self.write("package ");

        // Namespace segments
        for (i, ns) in pkg.namespace.iter().enumerate() {
            if i > 0 {
                self.write(":");
            }
            self.write(&ns.name);
        }
        self.write(":");
        self.write(&pkg.name.name);

        // Nested segments
        for nested in &pkg.nested {
            self.write("/");
            self.write(&nested.name);
        }

        // Version
        if let Some(ver) = &pkg.version {
            self.write("@");
            self.format_version(ver);
        }

        self.write(";\n");
    }

    fn format_version(&mut self, ver: &Version) {
        self.write(&format!("{}.{}.{}", ver.major, ver.minor, ver.patch));
    }

    fn format_nested_package(&mut self, nested: &NestedPackage) {
        if let Some(docs) = &nested.package.docs {
            self.format_doc_comment(docs);
        }
        self.write_indent();
        self.write("package ");

        // Namespace segments
        for (i, ns) in nested.package.namespace.iter().enumerate() {
            if i > 0 {
                self.write(":");
            }
            self.write(&ns.name);
        }
        self.write(":");
        self.write(&nested.package.name.name);

        if let Some(ver) = &nested.package.version {
            self.write("@");
            self.format_version(ver);
        }

        self.write(" {\n");
        self.indent();

        for use_stmt in &nested.uses {
            self.emit_comments_before(use_stmt.range.start());
            self.format_top_level_use(use_stmt);
        }

        if !nested.uses.is_empty() && !nested.items.is_empty() {
            self.newline();
        }

        for (i, item) in nested.items.iter().enumerate() {
            self.emit_comments_before(item.range().start());
            if i > 0 {
                self.newline();
            }
            self.format_item(item);
        }

        self.dedent();
        self.writeln("}");
    }

    fn format_top_level_use(&mut self, use_stmt: &TopLevelUse) {
        if let Some(docs) = &use_stmt.docs {
            self.format_doc_comment(docs);
        }
        self.write_indent();
        self.write("use ");
        self.format_use_path(&use_stmt.path);
        self.write(";\n");
    }

    fn format_use_path(&mut self, path: &UsePath) {
        if let Some(ns) = &path.namespace {
            self.write(&ns.name);
            self.write(":");
        }
        if let Some(pkg) = &path.package {
            self.write(&pkg.name);
            self.write("/");
        }
        self.write(&path.name.name);
        if let Some(ver) = &path.version {
            self.write("@");
            self.format_version(ver);
        }
    }

    fn format_item(&mut self, item: &Item) {
        match item {
            Item::Interface(iface) => self.format_interface(iface),
            Item::World(world) => self.format_world(world),
            Item::TypeDef(td) => self.format_typedef(td),
        }
    }

    fn format_interface(&mut self, iface: &InterfaceDecl) {
        if let Some(docs) = &iface.docs {
            self.format_doc_comment(docs);
        }
        self.format_gates(&iface.gates);
        self.write_indent();
        self.write("interface ");
        self.write(&iface.name.name);
        self.write(" {\n");
        self.indent();

        for (i, item) in iface.items.iter().enumerate() {
            self.emit_comments_before(item.range().start());
            if i > 0 {
                // Add blank line between different item types or after uses
                let prev = &iface.items[i - 1];
                if needs_blank_line_between(prev, item) {
                    self.newline();
                }
            }
            self.format_interface_item(item);
        }

        self.dedent();
        self.writeln("}");
    }

    fn format_interface_item(&mut self, item: &InterfaceItem) {
        match item {
            InterfaceItem::TypeDef(td) => self.format_typedef(td),
            InterfaceItem::Func(func) => self.format_func(func),
            InterfaceItem::Use(u) => self.format_interface_use(u),
        }
    }

    fn format_interface_use(&mut self, use_stmt: &InterfaceUse) {
        if let Some(docs) = &use_stmt.docs {
            self.format_doc_comment(docs);
        }
        self.write_indent();
        self.write("use ");
        self.format_use_path(&use_stmt.path);
        self.write(".{");

        for (i, name) in use_stmt.names.iter().enumerate() {
            if i > 0 {
                self.write(", ");
            }
            self.write(&name.name.name);
            if let Some(alias) = &name.alias {
                self.write(" as ");
                self.write(&alias.name);
            }
        }

        self.write("};\n");
    }

    fn format_world(&mut self, world: &WorldDecl) {
        if let Some(docs) = &world.docs {
            self.format_doc_comment(docs);
        }
        self.format_gates(&world.gates);
        self.write_indent();
        self.write("world ");
        self.write(&world.name.name);
        self.write(" {\n");
        self.indent();

        for (i, item) in world.items.iter().enumerate() {
            self.emit_comments_before(item.range().start());
            if i > 0 {
                let prev = &world.items[i - 1];
                if needs_blank_line_between_world_items(prev, item) {
                    self.newline();
                }
            }
            self.format_world_item(item);
        }

        self.dedent();
        self.writeln("}");
    }

    fn format_world_item(&mut self, item: &WorldItem) {
        match item {
            WorldItem::Import(imp) => self.format_import(imp),
            WorldItem::Export(exp) => self.format_export(exp),
            WorldItem::Include(inc) => self.format_include(inc),
            WorldItem::TypeDef(td) => self.format_typedef(td),
            WorldItem::Use(u) => self.format_interface_use(u),
        }
    }

    fn format_import(&mut self, imp: &ImportDecl) {
        if let Some(docs) = &imp.docs {
            self.format_doc_comment(docs);
        }
        self.write_indent();
        self.write("import ");
        self.write(&imp.name.name);
        self.format_extern_kind(&imp.kind);
    }

    fn format_export(&mut self, exp: &ExportDecl) {
        if let Some(docs) = &exp.docs {
            self.format_doc_comment(docs);
        }
        self.write_indent();
        self.write("export ");
        self.write(&exp.name.name);
        self.format_extern_kind(&exp.kind);
    }

    fn format_extern_kind(&mut self, kind: &ExternKind) {
        match kind {
            ExternKind::Path(path) => {
                self.write(": ");
                self.format_use_path(path);
                self.write(";\n");
            }
            ExternKind::Interface(items) => {
                self.write(": interface {\n");
                self.indent();
                for item in items {
                    self.emit_comments_before(item.range().start());
                    self.format_interface_item(item);
                }
                self.dedent();
                self.writeln("}");
            }
            ExternKind::Func(sig) => {
                self.write(": ");
                self.format_func_signature(sig);
                self.write(";\n");
            }
        }
    }

    fn format_include(&mut self, inc: &IncludeDecl) {
        if let Some(docs) = &inc.docs {
            self.format_doc_comment(docs);
        }
        self.write_indent();
        self.write("include ");
        self.format_use_path(&inc.path);

        if !inc.with.is_empty() {
            self.write(" with {\n");
            self.indent();
            for item in &inc.with {
                self.write_indent();
                self.write(&item.name.name);
                self.write(" as ");
                self.write(&item.alias.name);
                self.write(",\n");
            }
            self.dedent();
            self.write_indent();
            self.write("}");
        }

        self.write(";\n");
    }

    fn format_typedef(&mut self, td: &TypeDef) {
        match td {
            TypeDef::Alias(alias) => self.format_type_alias(alias),
            TypeDef::Record(rec) => self.format_record(rec),
            TypeDef::Variant(var) => self.format_variant(var),
            TypeDef::Enum(e) => self.format_enum(e),
            TypeDef::Flags(f) => self.format_flags(f),
            TypeDef::Resource(r) => self.format_resource(r),
        }
    }

    fn format_type_alias(&mut self, alias: &TypeAlias) {
        if let Some(docs) = &alias.docs {
            self.format_doc_comment(docs);
        }
        self.format_gates(&alias.gates);
        self.write_indent();
        self.write("type ");
        self.write(&alias.name.name);
        self.write(" = ");
        self.format_type(&alias.ty);
        self.write(";");
        self.emit_trailing_comment(alias.range.end());
        self.newline();
    }

    fn format_record(&mut self, rec: &RecordDecl) {
        if let Some(docs) = &rec.docs {
            self.format_doc_comment(docs);
        }
        self.format_gates(&rec.gates);
        self.write_indent();
        self.write("record ");
        self.write(&rec.name.name);
        self.write(" {\n");
        self.indent();

        for field in &rec.fields {
            self.emit_comments_before(field.range.start());
            if let Some(docs) = &field.docs {
                self.format_doc_comment(docs);
            }
            self.write_indent();
            self.write(&field.name.name);
            self.write(": ");
            self.format_type(&field.ty);
            self.write(",");
            self.emit_trailing_comment(field.range.end());
            self.newline();
        }

        self.dedent();
        self.writeln("}");
    }

    fn format_variant(&mut self, var: &VariantDecl) {
        if let Some(docs) = &var.docs {
            self.format_doc_comment(docs);
        }
        self.format_gates(&var.gates);
        self.write_indent();
        self.write("variant ");
        self.write(&var.name.name);
        self.write(" {\n");
        self.indent();

        for case in &var.cases {
            self.emit_comments_before(case.range.start());
            if let Some(docs) = &case.docs {
                self.format_doc_comment(docs);
            }
            self.write_indent();
            self.write(&case.name.name);
            if let Some(ty) = &case.ty {
                self.write("(");
                self.format_type(ty);
                self.write(")");
            }
            self.write(",");
            self.emit_trailing_comment(case.range.end());
            self.newline();
        }

        self.dedent();
        self.writeln("}");
    }

    fn format_enum(&mut self, e: &EnumDecl) {
        if let Some(docs) = &e.docs {
            self.format_doc_comment(docs);
        }
        self.format_gates(&e.gates);
        self.write_indent();
        self.write("enum ");
        self.write(&e.name.name);
        self.write(" {\n");
        self.indent();

        for case in &e.cases {
            self.emit_comments_before(case.range.start());
            if let Some(docs) = &case.docs {
                self.format_doc_comment(docs);
            }
            self.write_indent();
            self.write(&case.name.name);
            self.write(",");
            self.emit_trailing_comment(case.range.end());
            self.newline();
        }

        self.dedent();
        self.writeln("}");
    }

    fn format_flags(&mut self, f: &FlagsDecl) {
        if let Some(docs) = &f.docs {
            self.format_doc_comment(docs);
        }
        self.format_gates(&f.gates);
        self.write_indent();
        self.write("flags ");
        self.write(&f.name.name);
        self.write(" {\n");
        self.indent();

        for flag in &f.flags {
            self.emit_comments_before(flag.range.start());
            if let Some(docs) = &flag.docs {
                self.format_doc_comment(docs);
            }
            self.write_indent();
            self.write(&flag.name.name);
            self.write(",");
            self.emit_trailing_comment(flag.range.end());
            self.newline();
        }

        self.dedent();
        self.writeln("}");
    }

    fn format_resource(&mut self, r: &ResourceDecl) {
        if let Some(docs) = &r.docs {
            self.format_doc_comment(docs);
        }
        self.format_gates(&r.gates);
        self.write_indent();
        self.write("resource ");
        self.write(&r.name.name);

        if r.items.is_empty() {
            self.write(";\n");
        } else {
            self.write(" {\n");
            self.indent();

            for item in &r.items {
                self.emit_comments_before(item.range().start());
                self.format_resource_item(item);
            }

            self.dedent();
            self.writeln("}");
        }
    }

    fn format_resource_item(&mut self, item: &ResourceItem) {
        match item {
            ResourceItem::Constructor(c) => self.format_constructor(c),
            ResourceItem::Method(m) => self.format_method(m),
            ResourceItem::Static(s) => self.format_static(s),
        }
    }

    fn format_constructor(&mut self, c: &ConstructorDecl) {
        if let Some(docs) = &c.docs {
            self.format_doc_comment(docs);
        }
        self.format_gates(&c.gates);
        self.write_indent();
        self.write("constructor(");
        self.format_params(&c.params);
        self.write(")");
        if let Some(result) = &c.result {
            self.write(" -> ");
            self.format_type(result);
        }
        self.write(";");
        self.emit_trailing_comment(c.range.end());
        self.newline();
    }

    fn format_method(&mut self, m: &MethodDecl) {
        if let Some(docs) = &m.docs {
            self.format_doc_comment(docs);
        }
        self.format_gates(&m.gates);
        self.write_indent();
        self.write(&m.name.name);
        self.write(": ");
        self.format_func_signature(&m.sig);
        self.write(";");
        self.emit_trailing_comment(m.range.end());
        self.newline();
    }

    fn format_static(&mut self, s: &StaticDecl) {
        if let Some(docs) = &s.docs {
            self.format_doc_comment(docs);
        }
        self.format_gates(&s.gates);
        self.write_indent();
        self.write(&s.name.name);
        self.write(": static ");
        self.format_func_signature(&s.sig);
        self.write(";");
        self.emit_trailing_comment(s.range.end());
        self.newline();
    }

    fn format_func(&mut self, func: &FuncDecl) {
        if let Some(docs) = &func.docs {
            self.format_doc_comment(docs);
        }
        self.format_gates(&func.gates);
        self.write_indent();
        self.write(&func.name.name);
        self.write(": ");
        self.format_func_signature(&func.sig);
        self.write(";");
        self.emit_trailing_comment(func.range.end());
        self.newline();
    }

    fn format_func_signature(&mut self, sig: &FuncSignature) {
        if sig.is_async {
            self.write("async ");
        }
        self.write("func(");
        self.format_params(&sig.params);
        self.write(")");

        match &sig.results {
            FuncResults::None => {}
            FuncResults::Anon(ty) => {
                self.write(" -> ");
                self.format_type(ty);
            }
            FuncResults::Named(params) => {
                self.write(" -> (");
                self.format_params(params);
                self.write(")");
            }
        }
    }

    fn format_params(&mut self, params: &[Param]) {
        for (i, param) in params.iter().enumerate() {
            if i > 0 {
                self.write(", ");
            }
            self.write(&param.name.name);
            self.write(": ");
            self.format_type(&param.ty);
        }
    }

    fn format_type(&mut self, ty: &Type) {
        match ty {
            Type::Named(n) => self.write(&n.name.name),
            Type::Primitive(p) => self.write(primitive_name(p.kind)),
            Type::List(l) => {
                self.write("list<");
                self.format_type(&l.element);
                self.write(">");
            }
            Type::Option(o) => {
                self.write("option<");
                self.format_type(&o.inner);
                self.write(">");
            }
            Type::Result(r) => {
                self.write("result");
                match (&r.ok, &r.err) {
                    (None, None) => {}
                    (Some(ok), None) => {
                        self.write("<");
                        self.format_type(ok);
                        self.write(">");
                    }
                    (None, Some(err)) => {
                        self.write("<_, ");
                        self.format_type(err);
                        self.write(">");
                    }
                    (Some(ok), Some(err)) => {
                        self.write("<");
                        self.format_type(ok);
                        self.write(", ");
                        self.format_type(err);
                        self.write(">");
                    }
                }
            }
            Type::Tuple(t) => {
                self.write("tuple<");
                for (i, elem) in t.elements.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.format_type(elem);
                }
                self.write(">");
            }
            Type::Borrow(h) => {
                self.write("borrow<");
                self.write(&h.resource.name);
                self.write(">");
            }
            Type::Own(h) => {
                self.write("own<");
                self.write(&h.resource.name);
                self.write(">");
            }
            Type::Future(f) => {
                self.write("future");
                if let Some(inner) = &f.inner {
                    self.write("<");
                    self.format_type(inner);
                    self.write(">");
                }
            }
            Type::Stream(s) => {
                self.write("stream");
                if let Some(inner) = &s.inner {
                    self.write("<");
                    self.format_type(inner);
                    self.write(">");
                }
            }
        }
    }
}

fn primitive_name(kind: PrimitiveKind) -> &'static str {
    match kind {
        PrimitiveKind::Bool => "bool",
        PrimitiveKind::U8 => "u8",
        PrimitiveKind::U16 => "u16",
        PrimitiveKind::U32 => "u32",
        PrimitiveKind::U64 => "u64",
        PrimitiveKind::S8 => "s8",
        PrimitiveKind::S16 => "s16",
        PrimitiveKind::S32 => "s32",
        PrimitiveKind::S64 => "s64",
        PrimitiveKind::F32 => "f32",
        PrimitiveKind::F64 => "f64",
        PrimitiveKind::Char => "char",
        PrimitiveKind::String => "string",
    }
}

fn needs_blank_line_between(prev: &InterfaceItem, current: &InterfaceItem) -> bool {
    match (prev, current) {
        // After use statements, add blank line before non-use items
        (InterfaceItem::Use(_), InterfaceItem::TypeDef(_) | InterfaceItem::Func(_)) => true,
        // Between type defs and functions
        (InterfaceItem::TypeDef(_), InterfaceItem::Func(_)) => true,
        (InterfaceItem::Func(_), InterfaceItem::TypeDef(_)) => true,
        _ => false,
    }
}

fn needs_blank_line_between_world_items(prev: &WorldItem, current: &WorldItem) -> bool {
    match (prev, current) {
        // After use statements
        (WorldItem::Use(_), WorldItem::Import(_) | WorldItem::Export(_) | WorldItem::Include(_) | WorldItem::TypeDef(_)) => true,
        // Between imports and exports
        (WorldItem::Import(_), WorldItem::Export(_)) => true,
        (WorldItem::Export(_), WorldItem::Import(_)) => true,
        // Type defs separated from imports/exports
        (WorldItem::TypeDef(_), WorldItem::Import(_) | WorldItem::Export(_)) => true,
        (WorldItem::Import(_) | WorldItem::Export(_), WorldItem::TypeDef(_)) => true,
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse;

    fn format_str(input: &str) -> String {
        let result = parse(input);
        Formatter::format(&result.root)
    }

    #[test]
    fn format_package() {
        let input = "package   foo:bar@1.0.0;";
        let output = format_str(input);
        assert_eq!(output, "package foo:bar@1.0.0;\n");
    }

    #[test]
    fn format_interface() {
        let input = r#"
interface  types{
    type foo = u32;
  record user {
name: string,
    age:u32,
  }
}
"#;
        let output = format_str(input);
        assert_eq!(
            output,
            r#"interface types {
  type foo = u32;
  record user {
    name: string,
    age: u32,
  }
}
"#
        );
    }

    #[test]
    fn format_func() {
        let input = "interface api { greet:func(name:string)->string; }";
        let output = format_str(input);
        assert_eq!(
            output,
            r#"interface api {
  greet: func(name: string) -> string;
}
"#
        );
    }

    #[test]
    fn format_world() {
        let input = r#"world my-world{import foo:func();export bar:func()->u32;}"#;
        let output = format_str(input);
        assert_eq!(
            output,
            r#"world my-world {
  import foo: func();

  export bar: func() -> u32;
}
"#
        );
    }

    #[test]
    fn format_idempotent() {
        let input = r#"
package example:types@0.1.0;

interface types {
    record user {
        id: u64,
        name: string,
    }

    enum status {
        pending,
        active,
    }

    greet: func(name: string) -> string;
}
"#;
        let first = format_str(input);
        let second = format_str(&first);
        assert_eq!(first, second, "Formatting should be idempotent");
    }

    #[test]
    fn format_doc_comments() {
        let input = r#"
/// Package docs
package foo:bar;

/// Interface docs
interface api {
    /// Function docs
    greet: func(name: string);
}
"#;
        let output = format_str(input);
        assert!(output.contains("/// Package docs"));
        assert!(output.contains("/// Interface docs"));
        assert!(output.contains("/// Function docs"));
    }

    #[test]
    fn format_resource() {
        let input = r#"interface api { resource conn { constructor(url: string); read: func() -> list<u8>; } }"#;
        let output = format_str(input);
        assert_eq!(
            output,
            r#"interface api {
  resource conn {
    constructor(url: string);
    read: func() -> list<u8>;
  }
}
"#
        );
    }

    #[test]
    fn format_use_statement() {
        let input = r#"interface api { use types.{user, status as s}; }"#;
        let output = format_str(input);
        assert_eq!(
            output,
            r#"interface api {
  use types.{user, status as s};
}
"#
        );
    }

    #[test]
    fn format_variant() {
        let input = r#"interface api { variant err { not-found(string), timeout, } }"#;
        let output = format_str(input);
        assert_eq!(
            output,
            r#"interface api {
  variant err {
    not-found(string),
    timeout,
  }
}
"#
        );
    }

    #[test]
    fn format_complex_types() {
        let input = r#"interface api { type x = result<list<option<u32>>, string>; }"#;
        let output = format_str(input);
        assert_eq!(
            output,
            r#"interface api {
  type x = result<list<option<u32>>, string>;
}
"#
        );
    }

    // ==================== Comment preservation tests ====================

    fn format_with_comments_str(input: &str) -> String {
        let result = parse(input);
        Formatter::format_with_comments(input, &result)
    }

    #[test]
    fn format_preserves_line_comments() {
        let input = r#"// File header comment
interface api {
    // This is a comment about the function
    greet: func(name: string);
}
"#;
        let output = format_with_comments_str(input);
        assert!(output.contains("// File header comment"), "Header comment missing");
        assert!(output.contains("// This is a comment about the function"), "Function comment missing");
    }

    #[test]
    fn format_preserves_block_comments() {
        let input = r#"/* Block comment at top */
interface api {
    /* Inline block comment */
    greet: func(name: string);
}
"#;
        let output = format_with_comments_str(input);
        assert!(output.contains("/* Block comment at top */"), "Top block comment missing");
        assert!(output.contains("/* Inline block comment */"), "Inline block comment missing");
    }

    #[test]
    fn format_preserves_comments_in_record() {
        let input = r#"interface api {
    record user {
        // User's unique ID
        id: u64,
        // User's display name
        name: string,
    }
}
"#;
        let output = format_with_comments_str(input);
        assert!(output.contains("// User's unique ID"), "ID comment missing");
        assert!(output.contains("// User's display name"), "Name comment missing");
    }

    #[test]
    fn format_preserves_comments_in_enum() {
        let input = r#"interface api {
    enum status {
        // Request is waiting
        pending,
        // Request is being processed
        active,
    }
}
"#;
        let output = format_with_comments_str(input);
        assert!(output.contains("// Request is waiting"), "Pending comment missing");
        assert!(output.contains("// Request is being processed"), "Active comment missing");
    }

    #[test]
    fn format_preserves_comments_in_world() {
        let input = r#"world my-world {
    // Import the logging interface
    import logger: func(msg: string);

    // Export the main function
    export run: func();
}
"#;
        let output = format_with_comments_str(input);
        assert!(output.contains("// Import the logging interface"), "Import comment missing");
        assert!(output.contains("// Export the main function"), "Export comment missing");
    }

    #[test]
    fn format_with_comments_idempotent() {
        let input = r#"// Package comment
package foo:bar;

// Interface comment
interface api {
    // Function comment
    greet: func(name: string);
}
"#;
        let first = format_with_comments_str(input);
        let second = format_with_comments_str(&first);
        assert_eq!(first, second, "Comment-preserving formatting should be idempotent");
    }

    // ==================== Trailing comment tests ====================

    #[test]
    fn format_preserves_trailing_comments_in_record() {
        let input = r#"interface api {
    record user {
        id: u64, // User's unique ID
        name: string, // Display name
    }
}
"#;
        let output = format_with_comments_str(input);
        assert!(output.contains("id: u64, // User's unique ID"), "ID trailing comment missing");
        assert!(output.contains("name: string, // Display name"), "Name trailing comment missing");
    }

    #[test]
    fn format_preserves_trailing_comments_in_enum() {
        let input = r#"interface api {
    enum status {
        pending, // Waiting to start
        active, // Currently running
        done, // Completed
    }
}
"#;
        let output = format_with_comments_str(input);
        assert!(output.contains("pending, // Waiting to start"), "Pending trailing comment missing");
        assert!(output.contains("active, // Currently running"), "Active trailing comment missing");
        assert!(output.contains("done, // Completed"), "Done trailing comment missing");
    }

    #[test]
    fn format_preserves_trailing_comments_on_func() {
        let input = r#"interface api {
    greet: func(name: string) -> string; // Say hello
    goodbye: func(); // Say goodbye
}
"#;
        let output = format_with_comments_str(input);
        assert!(output.contains("; // Say hello"), "greet trailing comment missing");
        assert!(output.contains("; // Say goodbye"), "goodbye trailing comment missing");
    }

    #[test]
    fn format_preserves_trailing_comments_in_flags() {
        let input = r#"interface api {
    flags permissions {
        read, // Can read
        write, // Can write
        execute, // Can execute
    }
}
"#;
        let output = format_with_comments_str(input);
        assert!(output.contains("read, // Can read"), "read trailing comment missing");
        assert!(output.contains("write, // Can write"), "write trailing comment missing");
    }

    #[test]
    fn format_preserves_trailing_comments_in_variant() {
        let input = r#"interface api {
    variant my-result {
        success(string), // Success case
        failure(u32), // Error code
    }
}
"#;
        let output = format_with_comments_str(input);
        assert!(output.contains("success(string), // Success case"), "success trailing comment missing");
        assert!(output.contains("failure(u32), // Error code"), "failure trailing comment missing");
    }

    #[test]
    fn format_preserves_mixed_leading_and_trailing_comments() {
        let input = r#"interface api {
    record user {
        // The user's unique identifier
        id: u64, // Must be positive
        // The user's display name
        name: string, // Cannot be empty
    }
}
"#;
        let output = format_with_comments_str(input);
        // Leading comments
        assert!(output.contains("// The user's unique identifier"), "ID leading comment missing");
        assert!(output.contains("// The user's display name"), "Name leading comment missing");
        // Trailing comments
        assert!(output.contains("// Must be positive"), "ID trailing comment missing");
        assert!(output.contains("// Cannot be empty"), "Name trailing comment missing");
    }

    #[test]
    fn format_trailing_comments_idempotent() {
        let input = r#"interface api {
    record user {
        id: u64, // User ID
        name: string, // User name
    }
}
"#;
        let first = format_with_comments_str(input);
        let second = format_with_comments_str(&first);
        assert_eq!(first, second, "Trailing comment formatting should be idempotent");
    }
}
