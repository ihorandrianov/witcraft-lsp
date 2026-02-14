use crate::ast::*;
use crate::lexer::{Token, lex};
use crate::parse::{ErrorKind, ParseError, ParseResult};
use crate::{SyntaxKind, TextRange};
use smallvec::smallvec;

pub fn parse(source: &str) -> ParseResult {
    let tokens = lex(source);
    let mut parser = Parser::new(source, tokens);
    let root = parser.parse_source_file();
    ParseResult {
        root,
        errors: parser.errors,
        tokens: parser.tokens,
    }
}

struct Parser<'a> {
    source: &'a str,
    tokens: Vec<Token>,
    pos: usize,
    current_cache: Token,
    errors: Vec<ParseError>,
}

impl<'a> Parser<'a> {
    fn new(source: &'a str, tokens: Vec<Token>) -> Self {
        let eof = Token {
            kind: SyntaxKind::Eof,
            range: TextRange::new(source.len() as u32, source.len() as u32),
        };
        let current_cache = Self::find_non_trivia(&tokens, 0).unwrap_or(eof);
        Self {
            source,
            tokens,
            pos: 0,
            current_cache,
            errors: Vec::new(),
        }
    }

    fn find_non_trivia(tokens: &[Token], start: usize) -> Option<Token> {
        tokens[start..]
            .iter()
            .find(|t| !t.kind.is_trivia())
            .copied()
    }

    fn current(&self) -> Token {
        self.current_cache
    }

    fn at(&self, kind: SyntaxKind) -> bool {
        self.current_cache.kind == kind
    }

    fn at_any(&self, kinds: &[SyntaxKind]) -> bool {
        kinds.contains(&self.current_cache.kind)
    }

    fn at_eof(&self) -> bool {
        self.current_cache.kind == SyntaxKind::Eof
    }

    fn skip_trivia_peek(&self, mut n: usize) -> Token {
        let mut idx = self.pos;
        while idx < self.tokens.len() {
            let tok = self.tokens[idx];
            if tok.kind.is_trivia() {
                idx += 1;
            } else if n == 0 {
                return tok;
            } else {
                n -= 1;
                idx += 1;
            }
        }
        Token {
            kind: SyntaxKind::Eof,
            range: TextRange::new(self.source.len() as u32, self.source.len() as u32),
        }
    }

    fn peek(&self, n: usize) -> Token {
        self.skip_trivia_peek(n)
    }

    fn bump(&mut self) -> Token {
        while self.pos < self.tokens.len() && self.tokens[self.pos].kind.is_trivia() {
            self.pos += 1;
        }
        if self.pos < self.tokens.len() {
            let tok = self.tokens[self.pos];
            self.pos += 1;
            self.current_cache = Self::find_non_trivia(&self.tokens, self.pos).unwrap_or(Token {
                kind: SyntaxKind::Eof,
                range: TextRange::new(self.source.len() as u32, self.source.len() as u32),
            });
            tok
        } else {
            Token {
                kind: SyntaxKind::Eof,
                range: TextRange::new(self.source.len() as u32, self.source.len() as u32),
            }
        }
    }

    fn eat(&mut self, kind: SyntaxKind) -> Option<Token> {
        if self.at(kind) {
            Some(self.bump())
        } else {
            None
        }
    }

    fn expect(&mut self, kind: SyntaxKind) -> Option<Token> {
        if self.at(kind) {
            Some(self.bump())
        } else {
            let cur = self.current();
            let found = self.token_description(cur);
            self.errors.push(ParseError::new(
                format!("expected {}, found {}", Self::kind_description(kind), found),
                cur.range,
                ErrorKind::ExpectedToken(kind),
            ));
            None
        }
    }

    fn expect_closing(
        &mut self,
        close: SyntaxKind,
        construct: &str,
        opened_at: TextRange,
    ) -> Option<Token> {
        if self.at(close) {
            Some(self.bump())
        } else {
            let cur = self.current();
            self.errors
                .push(ParseError::unclosed(construct, opened_at, cur.range));
            None
        }
    }

    fn text(&self, range: TextRange) -> &'a str {
        range.slice(self.source)
    }

    fn token_description(&self, token: Token) -> String {
        match token.kind {
            SyntaxKind::Eof => "end of file".to_string(),
            SyntaxKind::Error => {
                // Error tokens may not be on char boundaries, so use get safely
                let start = token.range.start() as usize;
                let end = token.range.end() as usize;
                if let Some(text) = self.source.get(start..end) {
                    format!("unexpected character `{}`", text)
                } else {
                    "unexpected character".to_string()
                }
            }
            SyntaxKind::Ident => {
                let text = self.text(token.range);
                format!("identifier `{}`", text)
            }
            SyntaxKind::Integer => {
                let text = self.text(token.range);
                format!("integer `{}`", text)
            }
            _ if token.kind.is_keyword() => {
                let text = self.text(token.range);
                format!("keyword `{}`", text)
            }
            _ => format!("`{}`", self.text(token.range)),
        }
    }

    fn kind_description(kind: SyntaxKind) -> &'static str {
        match kind {
            SyntaxKind::Ident => "identifier",
            SyntaxKind::Integer => "integer",
            SyntaxKind::LParen => "`(`",
            SyntaxKind::RParen => "`)`",
            SyntaxKind::LBrace => "`{`",
            SyntaxKind::RBrace => "`}`",
            SyntaxKind::LAngle => "`<`",
            SyntaxKind::RAngle => "`>`",
            SyntaxKind::Colon => "`:`",
            SyntaxKind::Semicolon => "`;`",
            SyntaxKind::Comma => "`,`",
            SyntaxKind::Dot => "`.`",
            SyntaxKind::Eq => "`=`",
            SyntaxKind::At => "`@`",
            SyntaxKind::Slash => "`/`",
            SyntaxKind::Star => "`*`",
            SyntaxKind::Arrow => "`->`",
            SyntaxKind::Underscore => "`_`",
            _ => "token",
        }
    }

    fn error_expected(&mut self, expected: &str) {
        let cur = self.current();
        let found = self.token_description(cur);
        self.errors.push(ParseError::new(
            format!("expected {}, found {}", expected, found),
            cur.range,
            ErrorKind::Other,
        ));
    }

    fn recover_to(&mut self, recovery: &[SyntaxKind]) {
        while !self.at_eof() && !self.at_any(recovery) {
            self.bump();
        }
    }

    fn skip_to_next_item(&mut self) {
        // Don't include RBrace - it doesn't start a new item and would cause infinite loops
        self.recover_to(&[
            SyntaxKind::InterfaceKw,
            SyntaxKind::WorldKw,
            SyntaxKind::UseKw,
            SyntaxKind::PackageKw,
        ]);
    }

    fn collect_doc_comment(&mut self) -> Option<DocComment> {
        let mut idx = self.pos;
        let mut doc_range: Option<TextRange> = None;

        while idx < self.tokens.len() {
            let tok = self.tokens[idx];
            match tok.kind {
                SyntaxKind::Whitespace | SyntaxKind::LineComment | SyntaxKind::BlockComment => {
                    idx += 1;
                }
                SyntaxKind::DocComment => {
                    doc_range = Some(match doc_range {
                        Some(r) => TextRange::new(r.start(), tok.range.end()),
                        None => tok.range,
                    });
                    idx += 1;
                }
                _ => break,
            }
        }

        doc_range.map(|range| {
            let text = self.text(range).to_string();
            DocComment { text, range }
        })
    }

    fn parse_source_file(&mut self) -> SourceFile {
        let start = self.current().range.start();

        // Try to parse an optional standalone package declaration at the start
        // A standalone package decl ends with `;`, a nested package ends with `{`
        let mut package = None;
        let mut nested_packages = Vec::new();

        if self.at(SyntaxKind::PackageKw) {
            let docs = self.collect_doc_comment();
            if self.is_nested_package() {
                nested_packages.push(self.parse_nested_package(docs));
            } else {
                package = Some(self.parse_package_decl(docs));
            }
        }

        let mut uses = Vec::new();
        let mut items = Vec::new();

        while !self.at_eof() {
            // Skip stray closing braces that might have been left over from error recovery
            if self.at(SyntaxKind::RBrace) {
                self.error_expected("interface, world, or type definition");
                self.bump();
                continue;
            }

            if self.at(SyntaxKind::UseKw) {
                let docs = self.collect_doc_comment();
                uses.push(self.parse_top_level_use(docs));
                continue;
            }

            if self.at(SyntaxKind::PackageKw) {
                let docs = self.collect_doc_comment();
                nested_packages.push(self.parse_nested_package(docs));
                continue;
            }

            let gates = self.parse_gates();
            let docs = self.collect_doc_comment();
            if let Some(item) = self.parse_item(gates, docs) {
                items.push(item);
            } else if !self.at_eof() {
                self.error_expected("interface, world, or type definition");
                self.skip_to_next_item();
            }
        }

        let end = if let Some(last) = nested_packages.last() {
            last.range.end()
        } else if let Some(last) = items.last() {
            last.range().end()
        } else if let Some(last) = uses.last() {
            last.range.end()
        } else if let Some(ref pkg) = package {
            pkg.range.end()
        } else {
            start
        };

        SourceFile {
            package,
            uses,
            items,
            nested_packages,
            range: TextRange::new(start, end),
        }
    }

    /// Check if the current `package` keyword starts a nested package definition.
    /// Returns true if it's followed by `namespace:name { ... }` pattern.
    fn is_nested_package(&self) -> bool {
        let mut depth = 0;
        let mut i = 1;

        loop {
            let kind = self.peek(i).kind;
            match kind {
                SyntaxKind::LBrace => return depth == 0,
                SyntaxKind::Semicolon => return false,
                SyntaxKind::LAngle => {
                    depth += 1;
                    i += 1;
                }
                SyntaxKind::RAngle => {
                    depth -= 1;
                    i += 1;
                }
                SyntaxKind::Eof => {
                    return false;
                }
                _ => {
                    i += 1;
                }
            }
        }
    }

    /// Parse the package header: `package (ns:)+ name (/nested)* @version`
    /// Does not consume trailing semicolon or opening brace.
    ///
    /// Grammar: `'package' ( id ':' )+ id ( '/' id )* ('@' valid-semver)?`
    ///
    /// Returns (namespace_segments, name, nested_segments, version, start_pos, docs)
    fn parse_package_header(
        &mut self,
        docs: Option<DocComment>,
    ) -> (
        SmallVec2<Ident>,
        Ident,
        SmallVec2<Ident>,
        Option<Version>,
        u32,
        Option<DocComment>,
    ) {
        let start = self.bump().range.start();

        let mut segments: SmallVec2<Ident> = smallvec![];
        let first = if self.at(SyntaxKind::Ident) {
            self.parse_ident()
                .unwrap_or_else(|| Ident::new("", TextRange::new(start, start)))
        } else {
            self.error_expected("package namespace (e.g., `wasi` in `wasi:http@1.0.0`)");
            Ident::new("", TextRange::new(start, start))
        };
        segments.push(first);

        if !self.eat(SyntaxKind::Colon).is_some() {
            self.error_expected("`:` after namespace");
        }

        loop {
            let ident = if self.at(SyntaxKind::Ident) {
                self.parse_ident().unwrap_or_else(|| {
                    Ident::new(
                        "",
                        TextRange::new(self.current().range.start(), self.current().range.start()),
                    )
                })
            } else if self.at(SyntaxKind::At)
                || self.at(SyntaxKind::Slash)
                || self.at(SyntaxKind::Semicolon)
                || self.at(SyntaxKind::LBrace)
            {
                self.error_expected("package name");
                Ident::new(
                    "",
                    TextRange::new(self.current().range.start(), self.current().range.start()),
                )
            } else {
                self.error_expected("package name (e.g., `http` in `wasi:http`)");
                Ident::new(
                    "",
                    TextRange::new(self.current().range.start(), self.current().range.start()),
                )
            };

            if self.eat(SyntaxKind::Colon).is_some() {
                segments.push(ident);
            } else {
                let namespace = segments;
                let name = ident;

                let mut nested: SmallVec2<Ident> = smallvec![];
                while self.eat(SyntaxKind::Slash).is_some() {
                    if self.at(SyntaxKind::Ident) {
                        if let Some(ident) = self.parse_ident() {
                            nested.push(ident);
                        }
                    } else {
                        self.error_expected("nested package name after `/`");
                        break;
                    }
                }

                let version = if self.at(SyntaxKind::At) {
                    Some(self.parse_version())
                } else if self.at(SyntaxKind::Integer) || self.at(SyntaxKind::Dot) {
                    self.errors.push(ParseError::new(
                        "missing `@` before version (e.g., `@1.0.0`)",
                        self.current().range,
                        ErrorKind::Other,
                    ));
                    Some(self.parse_version_without_at())
                } else {
                    None
                };

                return (namespace, name, nested, version, start, docs);
            }
        }
    }

    fn parse_package_decl(&mut self, docs: Option<DocComment>) -> PackageDecl {
        let (namespace, name, nested, version, start, docs) = self.parse_package_header(docs);

        let end = self
            .expect(SyntaxKind::Semicolon)
            .map(|t| t.range.end())
            .unwrap_or_else(|| self.current().range.start());

        PackageDecl {
            docs,
            namespace,
            name,
            nested,
            version,
            range: TextRange::new(start, end),
        }
    }

    fn parse_nested_package(&mut self, docs: Option<DocComment>) -> NestedPackage {
        let (namespace, name, nested, version, start, docs) = self.parse_package_header(docs);

        let pkg_end = self.current().range.start();
        let package = PackageDecl {
            docs,
            namespace,
            name,
            nested,
            version,
            range: TextRange::new(start, pkg_end),
        };

        self.expect(SyntaxKind::LBrace);

        let mut uses = Vec::new();
        while self.at(SyntaxKind::UseKw) {
            let docs = self.collect_doc_comment();
            uses.push(self.parse_top_level_use(docs));
        }

        let mut items = Vec::new();
        while !self.at(SyntaxKind::RBrace) && !self.at_eof() {
            let gates = self.parse_gates();
            let docs = self.collect_doc_comment();
            if let Some(item) = self.parse_item(gates, docs) {
                items.push(item);
            } else if !self.at(SyntaxKind::RBrace) && !self.at_eof() {
                self.error_expected("interface, world, or type definition");
                self.skip_to_next_item();
            }
        }

        let end = self
            .expect(SyntaxKind::RBrace)
            .map(|t| t.range.end())
            .unwrap_or_else(|| self.current().range.start());

        NestedPackage {
            package,
            uses,
            items,
            range: TextRange::new(start, end),
        }
    }

    fn parse_version(&mut self) -> Version {
        let start = self.bump().range.start();

        let major = if self.at(SyntaxKind::Integer) {
            self.parse_integer().unwrap_or(0)
        } else {
            self.error_expected("major version number (e.g., `1` in `@1.0.0`)");
            0
        };

        if !self.eat(SyntaxKind::Dot).is_some() {
            self.error_expected("`.` after major version (format: `@major.minor.patch`)");
        }

        let minor = if self.at(SyntaxKind::Integer) {
            self.parse_integer().unwrap_or(0)
        } else {
            self.error_expected("minor version number (e.g., `0` in `@1.0.0`)");
            0
        };

        if !self.eat(SyntaxKind::Dot).is_some() {
            self.error_expected("`.` after minor version (format: `@major.minor.patch`)");
        }

        let patch = if self.at(SyntaxKind::Integer) {
            self.parse_integer().unwrap_or(0)
        } else {
            self.error_expected("patch version number (e.g., `0` in `@1.0.0`)");
            0
        };

        Version {
            major,
            minor,
            patch,
            range: TextRange::new(
                start,
                self.tokens
                    .get(self.pos.saturating_sub(1))
                    .map(|t| t.range.end())
                    .unwrap_or(start),
            ),
        }
    }

    fn parse_version_without_at(&mut self) -> Version {
        let start = self.current().range.start();
        self.eat(SyntaxKind::Dot);

        let major = self.parse_integer().unwrap_or(0);
        self.eat(SyntaxKind::Dot);
        let minor = self.parse_integer().unwrap_or(0);
        self.eat(SyntaxKind::Dot);
        let patch = self.parse_integer().unwrap_or(0);

        Version {
            major,
            minor,
            patch,
            range: TextRange::new(
                start,
                self.tokens
                    .get(self.pos.saturating_sub(1))
                    .map(|t| t.range.end())
                    .unwrap_or(start),
            ),
        }
    }

    fn parse_integer(&mut self) -> Option<u32> {
        if self.at(SyntaxKind::Integer) {
            let tok = self.bump();
            self.text(tok.range).parse().ok()
        } else {
            self.error_expected("integer");
            None
        }
    }

    fn parse_top_level_use(&mut self, docs: Option<DocComment>) -> TopLevelUse {
        let start = self.bump().range.start();
        let path = self.parse_use_path();
        let end = self
            .expect(SyntaxKind::Semicolon)
            .map(|t| t.range.end())
            .unwrap_or_else(|| path.range.end());

        TopLevelUse {
            docs,
            path,
            range: TextRange::new(start, end),
        }
    }

    fn parse_use_path(&mut self) -> UsePath {
        let start = self.current().range.start();

        let first = self
            .parse_ident()
            .unwrap_or_else(|| Ident::new("", TextRange::new(start, start)));

        let (namespace, package, name) = if self.at(SyntaxKind::Colon) {
            self.bump();
            let pkg = self
                .parse_ident()
                .unwrap_or_else(|| Ident::new("", TextRange::new(start, start)));
            if self.at(SyntaxKind::Slash) {
                self.bump();
                let iface = self
                    .parse_ident()
                    .unwrap_or_else(|| Ident::new("", TextRange::new(start, start)));
                (Some(first), Some(pkg), iface)
            } else {
                (Some(first), None, pkg)
            }
        } else if self.at(SyntaxKind::Slash) {
            self.bump();
            let iface = self
                .parse_ident()
                .unwrap_or_else(|| Ident::new("", TextRange::new(start, start)));
            (None, Some(first), iface)
        } else {
            (None, None, first)
        };

        let version = if self.at(SyntaxKind::At) {
            Some(self.parse_version())
        } else {
            None
        };

        let end = version
            .as_ref()
            .map(|v| v.range.end())
            .unwrap_or(name.range.end());

        UsePath {
            namespace,
            package,
            name,
            version,
            range: TextRange::new(start, end),
        }
    }

    fn parse_item(&mut self, gates: Gates, docs: Option<DocComment>) -> Option<Item> {
        match self.current().kind {
            SyntaxKind::InterfaceKw => {
                Some(Item::Interface(self.parse_interface_decl(gates, docs)))
            }
            SyntaxKind::WorldKw => Some(Item::World(self.parse_world_decl(gates, docs))),
            SyntaxKind::TypeKw
            | SyntaxKind::RecordKw
            | SyntaxKind::VariantKw
            | SyntaxKind::EnumKw
            | SyntaxKind::FlagsKw
            | SyntaxKind::ResourceKw => Some(Item::TypeDef(self.parse_typedef(gates, docs))),
            _ => None,
        }
    }

    fn parse_interface_decl(&mut self, gates: Gates, docs: Option<DocComment>) -> InterfaceDecl {
        let start = self.bump().range.start();
        let name = self
            .parse_ident()
            .unwrap_or_else(|| Ident::new("", TextRange::new(start, start)));

        let lbrace = self.expect(SyntaxKind::LBrace);
        let mut items = Vec::new();

        while !self.at(SyntaxKind::RBrace) && !self.at_eof() {
            let item_gates = self.parse_gates();
            let docs = self.collect_doc_comment();
            if let Some(item) = self.parse_interface_item(item_gates, docs) {
                items.push(item);
            } else if !self.at(SyntaxKind::RBrace) && !self.at_eof() {
                self.error_expected("interface item (use, type, func, resource, etc.)");
                self.recover_to(&[SyntaxKind::RBrace, SyntaxKind::Semicolon, SyntaxKind::Ident]);
                if self.at(SyntaxKind::Semicolon) {
                    self.bump();
                }
            }
        }

        let end = if let Some(open) = lbrace {
            self.expect_closing(SyntaxKind::RBrace, "interface", open.range)
        } else {
            self.expect(SyntaxKind::RBrace)
        }
        .map(|t| t.range.end())
        .unwrap_or(start);

        InterfaceDecl {
            gates,
            docs,
            name,
            items,
            range: TextRange::new(start, end),
        }
    }

    fn parse_interface_item(
        &mut self,
        gates: Gates,
        docs: Option<DocComment>,
    ) -> Option<InterfaceItem> {
        match self.current().kind {
            SyntaxKind::UseKw => Some(InterfaceItem::Use(self.parse_interface_use(docs))),
            SyntaxKind::TypeKw
            | SyntaxKind::RecordKw
            | SyntaxKind::VariantKw
            | SyntaxKind::EnumKw
            | SyntaxKind::FlagsKw
            | SyntaxKind::ResourceKw => {
                Some(InterfaceItem::TypeDef(self.parse_typedef(gates, docs)))
            }
            SyntaxKind::Ident => Some(InterfaceItem::Func(self.parse_func_decl(gates, docs))),
            _ => None,
        }
    }

    fn parse_interface_use(&mut self, docs: Option<DocComment>) -> InterfaceUse {
        let start = self.bump().range.start();
        let path = self.parse_use_path();

        let mut names: SmallVec4<UseNameItem> = smallvec![];
        if self.at(SyntaxKind::Dot) {
            self.bump();
            self.expect(SyntaxKind::LBrace);

            while !self.at(SyntaxKind::RBrace) && !self.at_eof() {
                names.push(self.parse_use_name_item());
                if !self.eat(SyntaxKind::Comma).is_some() {
                    break;
                }
            }

            self.expect(SyntaxKind::RBrace);
        }

        let end = self
            .expect(SyntaxKind::Semicolon)
            .map(|t| t.range.end())
            .unwrap_or_else(|| self.current().range.start());

        InterfaceUse {
            docs,
            path,
            names,
            range: TextRange::new(start, end),
        }
    }

    fn parse_use_name_item(&mut self) -> UseNameItem {
        let start = self.current().range.start();
        let name = self
            .parse_ident()
            .unwrap_or_else(|| Ident::new("", TextRange::new(start, start)));

        let alias = if self.at(SyntaxKind::AsKw) {
            self.bump();
            Some(
                self.parse_ident()
                    .unwrap_or_else(|| Ident::new("", TextRange::new(start, start))),
            )
        } else {
            None
        };

        let end = alias
            .as_ref()
            .map(|a| a.range.end())
            .unwrap_or(name.range.end());

        UseNameItem {
            name,
            alias,
            range: TextRange::new(start, end),
        }
    }

    fn parse_world_decl(&mut self, gates: Gates, docs: Option<DocComment>) -> WorldDecl {
        let start = self.bump().range.start();
        let name = self
            .parse_ident()
            .unwrap_or_else(|| Ident::new("", TextRange::new(start, start)));

        let lbrace = self.expect(SyntaxKind::LBrace);
        let mut items = Vec::new();

        while !self.at(SyntaxKind::RBrace) && !self.at_eof() {
            let item_gates = self.parse_gates();
            let docs = self.collect_doc_comment();
            if let Some(item) = self.parse_world_item(item_gates, docs) {
                items.push(item);
            } else if !self.at(SyntaxKind::RBrace) && !self.at_eof() {
                self.error_expected("world item (import, export, use, include, etc.)");
                self.recover_to(&[SyntaxKind::RBrace, SyntaxKind::Semicolon]);
                self.eat(SyntaxKind::Semicolon);
            }
        }

        let end = if let Some(open) = lbrace {
            self.expect_closing(SyntaxKind::RBrace, "world", open.range)
        } else {
            self.expect(SyntaxKind::RBrace)
        }
        .map(|t| t.range.end())
        .unwrap_or(start);

        WorldDecl {
            gates,
            docs,
            name,
            items,
            range: TextRange::new(start, end),
        }
    }

    fn parse_world_item(&mut self, gates: Gates, docs: Option<DocComment>) -> Option<WorldItem> {
        match self.current().kind {
            SyntaxKind::ImportKw => Some(WorldItem::Import(self.parse_import_decl(docs))),
            SyntaxKind::ExportKw => Some(WorldItem::Export(self.parse_export_decl(docs))),
            SyntaxKind::IncludeKw => Some(WorldItem::Include(self.parse_include_decl(docs))),
            SyntaxKind::UseKw => Some(WorldItem::Use(self.parse_interface_use(docs))),
            SyntaxKind::TypeKw
            | SyntaxKind::RecordKw
            | SyntaxKind::VariantKw
            | SyntaxKind::EnumKw
            | SyntaxKind::FlagsKw
            | SyntaxKind::ResourceKw => Some(WorldItem::TypeDef(self.parse_typedef(gates, docs))),
            _ => None,
        }
    }

    fn parse_import_decl(&mut self, docs: Option<DocComment>) -> ImportDecl {
        let start = self.bump().range.start();

        let first = self
            .parse_ident()
            .unwrap_or_else(|| Ident::new("", TextRange::new(start, start)));

        let (name, kind) = if self.at(SyntaxKind::Colon) {
            let after_colon = self.peek(1);
            if after_colon.kind == SyntaxKind::FuncKw
                || after_colon.kind == SyntaxKind::InterfaceKw
                || after_colon.kind == SyntaxKind::AsyncKw
            {
                self.bump();
                (first, self.parse_extern_kind())
            } else {
                let path = self.continue_parse_path(first);
                let name = Ident::new(path.name.name.clone(), path.name.range);
                (name, ExternKind::Path(path))
            }
        } else if self.at(SyntaxKind::Slash) {
            let path = self.continue_parse_path(first);
            let name = Ident::new(path.name.name.clone(), path.name.range);
            (name, ExternKind::Path(path))
        } else {
            let path = UsePath {
                namespace: None,
                package: None,
                name: first.clone(),
                version: None,
                range: first.range,
            };
            (first, ExternKind::Path(path))
        };

        let end = self
            .expect(SyntaxKind::Semicolon)
            .map(|t| t.range.end())
            .unwrap_or_else(|| self.current().range.start());

        ImportDecl {
            docs,
            name,
            kind,
            range: TextRange::new(start, end),
        }
    }

    fn continue_parse_path(&mut self, first: Ident) -> UsePath {
        let start = first.range.start();

        let (namespace, package, name) = if self.at(SyntaxKind::Colon) {
            self.bump();
            let pkg = self
                .parse_ident()
                .unwrap_or_else(|| Ident::new("", TextRange::new(start, start)));
            if self.at(SyntaxKind::Slash) {
                self.bump();
                let iface = self
                    .parse_ident()
                    .unwrap_or_else(|| Ident::new("", TextRange::new(start, start)));
                (Some(first), Some(pkg), iface)
            } else {
                (Some(first), None, pkg)
            }
        } else if self.at(SyntaxKind::Slash) {
            self.bump();
            let iface = self
                .parse_ident()
                .unwrap_or_else(|| Ident::new("", TextRange::new(start, start)));
            (None, Some(first), iface)
        } else {
            (None, None, first)
        };

        let version = if self.at(SyntaxKind::At) {
            Some(self.parse_version())
        } else {
            None
        };

        let end = version
            .as_ref()
            .map(|v| v.range.end())
            .unwrap_or(name.range.end());

        UsePath {
            namespace,
            package,
            name,
            version,
            range: TextRange::new(start, end),
        }
    }

    fn parse_export_decl(&mut self, docs: Option<DocComment>) -> ExportDecl {
        let start = self.bump().range.start();

        let first = self
            .parse_ident()
            .unwrap_or_else(|| Ident::new("", TextRange::new(start, start)));

        let (name, kind) = if self.at(SyntaxKind::Colon) {
            let after_colon = self.peek(1);
            if after_colon.kind == SyntaxKind::FuncKw
                || after_colon.kind == SyntaxKind::InterfaceKw
                || after_colon.kind == SyntaxKind::AsyncKw
            {
                self.bump();
                (first, self.parse_extern_kind())
            } else {
                let path = self.continue_parse_path(first);
                let name = Ident::new(path.name.name.clone(), path.name.range);
                (name, ExternKind::Path(path))
            }
        } else if self.at(SyntaxKind::Slash) {
            let path = self.continue_parse_path(first);
            let name = Ident::new(path.name.name.clone(), path.name.range);
            (name, ExternKind::Path(path))
        } else {
            let path = UsePath {
                namespace: None,
                package: None,
                name: first.clone(),
                version: None,
                range: first.range,
            };
            (first, ExternKind::Path(path))
        };

        let end = self
            .expect(SyntaxKind::Semicolon)
            .map(|t| t.range.end())
            .unwrap_or_else(|| self.current().range.start());

        ExportDecl {
            docs,
            name,
            kind,
            range: TextRange::new(start, end),
        }
    }

    fn parse_extern_kind(&mut self) -> ExternKind {
        match self.current().kind {
            SyntaxKind::InterfaceKw => {
                self.bump();
                self.expect(SyntaxKind::LBrace);

                let mut items = Vec::new();
                while !self.at(SyntaxKind::RBrace) && !self.at_eof() {
                    let item_gates = self.parse_gates();
                    let docs = self.collect_doc_comment();
                    if let Some(item) = self.parse_interface_item(item_gates, docs) {
                        items.push(item);
                    } else {
                        break;
                    }
                }

                self.expect(SyntaxKind::RBrace);
                ExternKind::Interface(items)
            }
            SyntaxKind::FuncKw | SyntaxKind::AsyncKw => {
                ExternKind::Func(self.parse_func_signature())
            }
            _ => ExternKind::Path(self.parse_use_path()),
        }
    }

    fn parse_include_decl(&mut self, docs: Option<DocComment>) -> IncludeDecl {
        let start = self.bump().range.start();
        let path = self.parse_use_path();

        let with: SmallVec4<IncludeNameItem> = if self.at(SyntaxKind::WithKw) {
            self.bump();
            self.expect(SyntaxKind::LBrace);

            let mut items: SmallVec4<IncludeNameItem> = smallvec![];
            while !self.at(SyntaxKind::RBrace) && !self.at_eof() {
                items.push(self.parse_include_name_item());
                if self.eat(SyntaxKind::Comma).is_none() {
                    break;
                }
            }

            self.expect(SyntaxKind::RBrace);
            items
        } else {
            smallvec![]
        };

        let end = self
            .expect(SyntaxKind::Semicolon)
            .map(|t| t.range.end())
            .unwrap_or_else(|| path.range.end());

        IncludeDecl {
            docs,
            path,
            with,
            range: TextRange::new(start, end),
        }
    }

    fn parse_include_name_item(&mut self) -> IncludeNameItem {
        let start = self.current().range.start();
        let name = self
            .parse_ident()
            .unwrap_or_else(|| Ident::new("", TextRange::new(start, start)));

        self.expect(SyntaxKind::AsKw);

        let alias = self
            .parse_ident()
            .unwrap_or_else(|| Ident::new("", TextRange::new(start, start)));

        let end = alias.range.end();

        IncludeNameItem {
            name,
            alias,
            range: TextRange::new(start, end),
        }
    }

    fn parse_typedef(&mut self, gates: Gates, docs: Option<DocComment>) -> TypeDef {
        match self.current().kind {
            SyntaxKind::TypeKw => TypeDef::Alias(self.parse_type_alias(gates, docs)),
            SyntaxKind::RecordKw => TypeDef::Record(self.parse_record(gates, docs)),
            SyntaxKind::VariantKw => TypeDef::Variant(self.parse_variant(gates, docs)),
            SyntaxKind::EnumKw => TypeDef::Enum(self.parse_enum(gates, docs)),
            SyntaxKind::FlagsKw => TypeDef::Flags(self.parse_flags(gates, docs)),
            SyntaxKind::ResourceKw => TypeDef::Resource(self.parse_resource(gates, docs)),
            _ => {
                self.error_expected(
                    "type definition (type, record, variant, enum, flags, resource)",
                );
                let start = self.current().range.start();
                TypeDef::Alias(TypeAlias {
                    gates,
                    docs,
                    name: Ident::new("", TextRange::new(start, start)),
                    ty: Type::Named(NamedType {
                        name: Ident::new("", TextRange::new(start, start)),
                        range: TextRange::new(start, start),
                    }),
                    range: TextRange::new(start, start),
                })
            }
        }
    }

    fn parse_type_alias(&mut self, gates: Gates, docs: Option<DocComment>) -> TypeAlias {
        let start = self.bump().range.start();
        let name = self
            .parse_ident()
            .unwrap_or_else(|| Ident::new("", TextRange::new(start, start)));
        self.expect(SyntaxKind::Eq);
        let ty = self.parse_type();
        let end = self
            .expect(SyntaxKind::Semicolon)
            .map(|t| t.range.end())
            .unwrap_or_else(|| ty.range().end());

        TypeAlias {
            gates,
            docs,
            name,
            ty,
            range: TextRange::new(start, end),
        }
    }

    fn parse_record(&mut self, gates: Gates, docs: Option<DocComment>) -> RecordDecl {
        let start = self.bump().range.start();
        let name = self
            .parse_ident()
            .unwrap_or_else(|| Ident::new("", TextRange::new(start, start)));

        let lbrace = self.expect(SyntaxKind::LBrace);
        let mut fields: SmallVec8<RecordField> = smallvec![];

        while !self.at(SyntaxKind::RBrace) && !self.at_eof() {
            let docs = self.collect_doc_comment();
            fields.push(self.parse_record_field(docs));
            if !self.eat(SyntaxKind::Comma).is_some() {
                break;
            }
        }

        let end = if let Some(open) = lbrace {
            self.expect_closing(SyntaxKind::RBrace, "record", open.range)
        } else {
            self.expect(SyntaxKind::RBrace)
        }
        .map(|t| t.range.end())
        .unwrap_or_else(|| name.range.end());

        RecordDecl {
            gates,
            docs,
            name,
            fields,
            range: TextRange::new(start, end),
        }
    }

    fn parse_record_field(&mut self, docs: Option<DocComment>) -> RecordField {
        let start = self.current().range.start();
        let name = self
            .parse_ident()
            .unwrap_or_else(|| Ident::new("", TextRange::new(start, start)));
        self.expect(SyntaxKind::Colon);
        let ty = self.parse_type();

        RecordField {
            docs,
            name,
            ty: ty.clone(),
            range: TextRange::new(start, ty.range().end()),
        }
    }

    fn parse_variant(&mut self, gates: Gates, docs: Option<DocComment>) -> VariantDecl {
        let start = self.bump().range.start();
        let name = self
            .parse_ident()
            .unwrap_or_else(|| Ident::new("", TextRange::new(start, start)));

        let lbrace = self.expect(SyntaxKind::LBrace);
        let mut cases: SmallVec8<VariantCase> = smallvec![];

        while !self.at(SyntaxKind::RBrace) && !self.at_eof() {
            let docs = self.collect_doc_comment();
            cases.push(self.parse_variant_case(docs));
            if !self.eat(SyntaxKind::Comma).is_some() {
                break;
            }
        }

        let end = if let Some(open) = lbrace {
            self.expect_closing(SyntaxKind::RBrace, "variant", open.range)
        } else {
            self.expect(SyntaxKind::RBrace)
        }
        .map(|t| t.range.end())
        .unwrap_or_else(|| name.range.end());

        VariantDecl {
            gates,
            docs,
            name,
            cases,
            range: TextRange::new(start, end),
        }
    }

    fn parse_variant_case(&mut self, docs: Option<DocComment>) -> VariantCase {
        let start = self.current().range.start();
        let name = self
            .parse_ident()
            .unwrap_or_else(|| Ident::new("", TextRange::new(start, start)));

        let ty = if self.at(SyntaxKind::LParen) {
            self.bump();
            let t = self.parse_type();
            self.expect(SyntaxKind::RParen);
            Some(t)
        } else {
            None
        };

        let end = ty
            .as_ref()
            .map(|_| {
                self.tokens
                    .get(self.pos.saturating_sub(1))
                    .map(|t| t.range.end())
                    .unwrap_or(name.range.end())
            })
            .unwrap_or(name.range.end());

        VariantCase {
            docs,
            name,
            ty,
            range: TextRange::new(start, end),
        }
    }

    fn parse_enum(&mut self, gates: Gates, docs: Option<DocComment>) -> EnumDecl {
        let start = self.bump().range.start();
        let name = self
            .parse_ident()
            .unwrap_or_else(|| Ident::new("", TextRange::new(start, start)));

        let lbrace = self.expect(SyntaxKind::LBrace);
        let mut cases: SmallVec8<EnumCase> = smallvec![];

        while !self.at(SyntaxKind::RBrace) && !self.at_eof() {
            let docs = self.collect_doc_comment();
            let case_start = self.current().range.start();
            let case_name = self
                .parse_ident()
                .unwrap_or_else(|| Ident::new("", TextRange::new(case_start, case_start)));
            cases.push(EnumCase {
                docs,
                name: case_name.clone(),
                range: case_name.range,
            });
            if !self.eat(SyntaxKind::Comma).is_some() {
                break;
            }
        }

        let end = if let Some(open) = lbrace {
            self.expect_closing(SyntaxKind::RBrace, "enum", open.range)
        } else {
            self.expect(SyntaxKind::RBrace)
        }
        .map(|t| t.range.end())
        .unwrap_or_else(|| name.range.end());

        EnumDecl {
            gates,
            docs,
            name,
            cases,
            range: TextRange::new(start, end),
        }
    }

    fn parse_flags(&mut self, gates: Gates, docs: Option<DocComment>) -> FlagsDecl {
        let start = self.bump().range.start();
        let name = self
            .parse_ident()
            .unwrap_or_else(|| Ident::new("", TextRange::new(start, start)));

        let lbrace = self.expect(SyntaxKind::LBrace);
        let mut flags: SmallVec8<FlagCase> = smallvec![];

        while !self.at(SyntaxKind::RBrace) && !self.at_eof() {
            let docs = self.collect_doc_comment();
            let flag_start = self.current().range.start();
            let flag_name = self
                .parse_ident()
                .unwrap_or_else(|| Ident::new("", TextRange::new(flag_start, flag_start)));
            flags.push(FlagCase {
                docs,
                name: flag_name.clone(),
                range: flag_name.range,
            });
            if !self.eat(SyntaxKind::Comma).is_some() {
                break;
            }
        }

        let end = if let Some(open) = lbrace {
            self.expect_closing(SyntaxKind::RBrace, "flags", open.range)
        } else {
            self.expect(SyntaxKind::RBrace)
        }
        .map(|t| t.range.end())
        .unwrap_or_else(|| name.range.end());

        FlagsDecl {
            gates,
            docs,
            name,
            flags,
            range: TextRange::new(start, end),
        }
    }

    fn parse_resource(&mut self, gates: Gates, docs: Option<DocComment>) -> ResourceDecl {
        let start = self.bump().range.start();
        let name = self
            .parse_ident()
            .unwrap_or_else(|| Ident::new("", TextRange::new(start, start)));

        let mut items = Vec::new();

        if self.at(SyntaxKind::LBrace) {
            let open_tok = self.bump();

            while !self.at(SyntaxKind::RBrace) && !self.at_eof() {
                let item_gates = self.parse_gates();
                let docs = self.collect_doc_comment();
                if let Some(item) = self.parse_resource_item(item_gates, docs) {
                    items.push(item);
                } else if !self.at(SyntaxKind::RBrace) && !self.at_eof() {
                    self.error_expected("resource item (constructor, method, static)");
                    self.recover_to(&[SyntaxKind::RBrace, SyntaxKind::Semicolon]);
                    self.eat(SyntaxKind::Semicolon);
                }
            }

            self.expect_closing(SyntaxKind::RBrace, "resource", open_tok.range);
        } else {
            self.expect(SyntaxKind::Semicolon);
        }

        let end = self
            .tokens
            .get(self.pos.saturating_sub(1))
            .map(|t| t.range.end())
            .unwrap_or(name.range.end());

        ResourceDecl {
            gates,
            docs,
            name,
            items,
            range: TextRange::new(start, end),
        }
    }

    fn parse_resource_item(
        &mut self,
        gates: Gates,
        docs: Option<DocComment>,
    ) -> Option<ResourceItem> {
        match self.current().kind {
            SyntaxKind::ConstructorKw => Some(ResourceItem::Constructor(
                self.parse_constructor(gates, docs),
            )),
            SyntaxKind::Ident => {
                let name = self.parse_ident()?;
                let name_start = name.range.start();
                self.expect(SyntaxKind::Colon);

                if self.at(SyntaxKind::StaticKw) {
                    self.bump();
                    let sig = self.parse_func_signature();
                    let end = self
                        .expect(SyntaxKind::Semicolon)
                        .map(|t| t.range.end())
                        .unwrap_or_else(|| sig.range.end());
                    Some(ResourceItem::Static(StaticDecl {
                        gates,
                        docs,
                        name,
                        sig,
                        range: TextRange::new(name_start, end),
                    }))
                } else {
                    let sig = self.parse_func_signature();
                    let end = self
                        .expect(SyntaxKind::Semicolon)
                        .map(|t| t.range.end())
                        .unwrap_or_else(|| sig.range.end());
                    Some(ResourceItem::Method(MethodDecl {
                        gates,
                        docs,
                        name,
                        sig,
                        range: TextRange::new(name_start, end),
                    }))
                }
            }
            _ => None,
        }
    }

    fn parse_constructor(&mut self, gates: Gates, docs: Option<DocComment>) -> ConstructorDecl {
        let start = self.bump().range.start();
        self.expect(SyntaxKind::LParen);

        let mut params: SmallVec4<Param> = smallvec![];
        while !self.at(SyntaxKind::RParen) && !self.at_eof() {
            params.push(self.parse_param());
            if !self.eat(SyntaxKind::Comma).is_some() {
                break;
            }
        }

        self.expect(SyntaxKind::RParen);

        // Parse optional result type for fallible constructors: -> type
        let result = if self.eat(SyntaxKind::Arrow).is_some() {
            Some(self.parse_type())
        } else {
            None
        };

        let end = self
            .expect(SyntaxKind::Semicolon)
            .map(|t| t.range.end())
            .unwrap_or_else(|| self.current().range.start());

        ConstructorDecl {
            gates,
            docs,
            params,
            result,
            range: TextRange::new(start, end),
        }
    }

    fn parse_func_decl(&mut self, gates: Gates, docs: Option<DocComment>) -> FuncDecl {
        let start = self.current().range.start();
        let name = self
            .parse_ident()
            .unwrap_or_else(|| Ident::new("", TextRange::new(start, start)));
        self.expect(SyntaxKind::Colon);
        let sig = self.parse_func_signature();
        let end = self
            .expect(SyntaxKind::Semicolon)
            .map(|t| t.range.end())
            .unwrap_or_else(|| sig.range.end());

        FuncDecl {
            gates,
            docs,
            name,
            sig,
            range: TextRange::new(start, end),
        }
    }

    fn parse_func_signature(&mut self) -> FuncSignature {
        let start = self.current().range.start();

        let is_async = if self.at(SyntaxKind::AsyncKw) {
            self.bump();
            true
        } else {
            false
        };

        self.expect(SyntaxKind::FuncKw);
        self.expect(SyntaxKind::LParen);

        let mut params: SmallVec4<Param> = smallvec![];
        while !self.at(SyntaxKind::RParen) && !self.at_eof() {
            // Skip any error tokens before trying to parse a param
            while self.at(SyntaxKind::Error) {
                self.bump();
            }
            if self.at(SyntaxKind::RParen) || self.at_eof() {
                break;
            }
            // Break if we hit a keyword or brace that indicates we've gone too far
            if self.current().kind.is_keyword()
                || self.at(SyntaxKind::RBrace)
                || self.at(SyntaxKind::LBrace)
            {
                break;
            }
            params.push(self.parse_param());
            // After parsing a param, recover to , or ) or ; or } or keywords
            while !self.at_any(&[
                SyntaxKind::Comma,
                SyntaxKind::RParen,
                SyntaxKind::Semicolon,
                SyntaxKind::RBrace,
            ]) && !self.at_eof()
                && !self.current().kind.is_keyword()
            {
                self.bump();
            }
            if !self.eat(SyntaxKind::Comma).is_some() {
                break;
            }
        }

        self.expect(SyntaxKind::RParen);

        let results = if self.at(SyntaxKind::Arrow) {
            self.bump();
            self.parse_func_results()
        } else {
            FuncResults::None
        };

        let end = match &results {
            FuncResults::None => self
                .tokens
                .get(self.pos.saturating_sub(1))
                .map(|t| t.range.end())
                .unwrap_or(start),
            FuncResults::Anon(ty) => ty.range().end(),
            FuncResults::Named(params) => params.last().map(|p| p.range.end()).unwrap_or(start),
        };

        FuncSignature {
            is_async,
            params,
            results,
            range: TextRange::new(start, end),
        }
    }

    fn parse_param(&mut self) -> Param {
        let start = self.current().range.start();
        let name = self
            .parse_ident()
            .unwrap_or_else(|| Ident::new("", TextRange::new(start, start)));
        self.expect(SyntaxKind::Colon);
        let ty = self.parse_type();

        Param {
            name,
            ty: ty.clone(),
            range: TextRange::new(start, ty.range().end()),
        }
    }

    fn parse_func_results(&mut self) -> FuncResults {
        if self.at(SyntaxKind::LParen) {
            self.bump();
            let mut params: SmallVec4<Param> = smallvec![];
            while !self.at(SyntaxKind::RParen) && !self.at_eof() {
                params.push(self.parse_param());
                if !self.eat(SyntaxKind::Comma).is_some() {
                    break;
                }
            }
            self.expect(SyntaxKind::RParen);
            FuncResults::Named(params)
        } else {
            FuncResults::Anon(self.parse_type())
        }
    }

    fn parse_type(&mut self) -> Type {
        let start = self.current().range.start();

        match self.current().kind {
            SyntaxKind::BoolKw => self.parse_primitive(PrimitiveKind::Bool),
            SyntaxKind::U8Kw => self.parse_primitive(PrimitiveKind::U8),
            SyntaxKind::U16Kw => self.parse_primitive(PrimitiveKind::U16),
            SyntaxKind::U32Kw => self.parse_primitive(PrimitiveKind::U32),
            SyntaxKind::U64Kw => self.parse_primitive(PrimitiveKind::U64),
            SyntaxKind::S8Kw => self.parse_primitive(PrimitiveKind::S8),
            SyntaxKind::S16Kw => self.parse_primitive(PrimitiveKind::S16),
            SyntaxKind::S32Kw => self.parse_primitive(PrimitiveKind::S32),
            SyntaxKind::S64Kw => self.parse_primitive(PrimitiveKind::S64),
            SyntaxKind::F32Kw => self.parse_primitive(PrimitiveKind::F32),
            SyntaxKind::F64Kw => self.parse_primitive(PrimitiveKind::F64),
            SyntaxKind::CharKw => self.parse_primitive(PrimitiveKind::Char),
            SyntaxKind::StringKw => self.parse_primitive(PrimitiveKind::String),

            SyntaxKind::ListKw => self.parse_list_type(),
            SyntaxKind::OptionKw => self.parse_option_type(),
            SyntaxKind::ResultKw => self.parse_result_type(),
            SyntaxKind::TupleKw => self.parse_tuple_type(),
            SyntaxKind::BorrowKw => self.parse_borrow_type(),
            SyntaxKind::OwnKw => self.parse_own_type(),
            SyntaxKind::FutureKw => self.parse_future_type(),
            SyntaxKind::StreamKw => self.parse_stream_type(),

            SyntaxKind::Ident => {
                let name = self
                    .parse_ident()
                    .unwrap_or_else(|| Ident::new("", TextRange::new(start, start)));
                Type::Named(NamedType {
                    range: name.range,
                    name,
                })
            }

            _ => {
                self.error_expected("type");
                self.bump(); // consume the error token to avoid infinite loops
                Type::Named(NamedType {
                    name: Ident::new("", TextRange::new(start, start)),
                    range: TextRange::new(start, start),
                })
            }
        }
    }

    fn parse_primitive(&mut self, kind: PrimitiveKind) -> Type {
        let tok = self.bump();
        Type::Primitive(PrimitiveType {
            kind,
            range: tok.range,
        })
    }

    fn parse_list_type(&mut self) -> Type {
        let start = self.bump().range.start();
        let open = self.expect(SyntaxKind::LAngle);
        let element = self.parse_type();
        let end = if let Some(open_tok) = open {
            self.expect_closing(SyntaxKind::RAngle, "list<>", open_tok.range)
        } else {
            self.expect(SyntaxKind::RAngle)
        }
        .map(|t| t.range.end())
        .unwrap_or_else(|| element.range().end());

        Type::List(Box::new(ListType {
            element,
            range: TextRange::new(start, end),
        }))
    }

    fn parse_option_type(&mut self) -> Type {
        let start = self.bump().range.start();
        let open = self.expect(SyntaxKind::LAngle);
        let inner = self.parse_type();
        let end = if let Some(open_tok) = open {
            self.expect_closing(SyntaxKind::RAngle, "option<>", open_tok.range)
        } else {
            self.expect(SyntaxKind::RAngle)
        }
        .map(|t| t.range.end())
        .unwrap_or_else(|| inner.range().end());

        Type::Option(Box::new(OptionType {
            inner,
            range: TextRange::new(start, end),
        }))
    }

    fn parse_result_type(&mut self) -> Type {
        let start = self.bump().range.start();

        if !self.at(SyntaxKind::LAngle) {
            return Type::Result(Box::new(ResultType {
                ok: None,
                err: None,
                range: TextRange::new(
                    start,
                    self.tokens
                        .get(self.pos.saturating_sub(1))
                        .map(|t| t.range.end())
                        .unwrap_or(start),
                ),
            }));
        }

        let open_tok = self.bump();

        let ok = if self.at(SyntaxKind::Underscore) {
            self.bump();
            None
        } else {
            Some(self.parse_type())
        };

        let err = if self.at(SyntaxKind::Comma) {
            self.bump();
            Some(self.parse_type())
        } else {
            None
        };

        let end = self
            .expect_closing(SyntaxKind::RAngle, "result<>", open_tok.range)
            .map(|t| t.range.end())
            .unwrap_or_else(|| {
                err.as_ref()
                    .map(|e| e.range().end())
                    .or_else(|| ok.as_ref().map(|o| o.range().end()))
                    .unwrap_or(start)
            });

        Type::Result(Box::new(ResultType {
            ok,
            err,
            range: TextRange::new(start, end),
        }))
    }

    fn parse_tuple_type(&mut self) -> Type {
        let start = self.bump().range.start();
        let open = self.expect(SyntaxKind::LAngle);

        let mut elements: SmallVec4<Type> = smallvec![];
        while !self.at(SyntaxKind::RAngle) && !self.at_eof() {
            elements.push(self.parse_type());
            if !self.eat(SyntaxKind::Comma).is_some() {
                break;
            }
        }

        let end = if let Some(open_tok) = open {
            self.expect_closing(SyntaxKind::RAngle, "tuple<>", open_tok.range)
        } else {
            self.expect(SyntaxKind::RAngle)
        }
        .map(|t| t.range.end())
        .unwrap_or_else(|| elements.last().map(|e| e.range().end()).unwrap_or(start));

        Type::Tuple(Box::new(TupleType {
            elements,
            range: TextRange::new(start, end),
        }))
    }

    fn parse_borrow_type(&mut self) -> Type {
        let start = self.bump().range.start();
        let open = self.expect(SyntaxKind::LAngle);
        let resource = self
            .parse_ident()
            .unwrap_or_else(|| Ident::new("", TextRange::new(start, start)));
        let end = if let Some(open_tok) = open {
            self.expect_closing(SyntaxKind::RAngle, "borrow<>", open_tok.range)
        } else {
            self.expect(SyntaxKind::RAngle)
        }
        .map(|t| t.range.end())
        .unwrap_or_else(|| resource.range.end());

        Type::Borrow(Box::new(HandleType {
            resource,
            range: TextRange::new(start, end),
        }))
    }

    fn parse_own_type(&mut self) -> Type {
        let start = self.bump().range.start();
        let open = self.expect(SyntaxKind::LAngle);
        let resource = self
            .parse_ident()
            .unwrap_or_else(|| Ident::new("", TextRange::new(start, start)));
        let end = if let Some(open_tok) = open {
            self.expect_closing(SyntaxKind::RAngle, "own<>", open_tok.range)
        } else {
            self.expect(SyntaxKind::RAngle)
        }
        .map(|t| t.range.end())
        .unwrap_or_else(|| resource.range.end());

        Type::Own(Box::new(HandleType {
            resource,
            range: TextRange::new(start, end),
        }))
    }

    fn parse_future_type(&mut self) -> Type {
        let start = self.bump().range.start();

        if !self.at(SyntaxKind::LAngle) {
            return Type::Future(Box::new(FutureType {
                inner: None,
                range: TextRange::new(
                    start,
                    self.tokens
                        .get(self.pos.saturating_sub(1))
                        .map(|t| t.range.end())
                        .unwrap_or(start),
                ),
            }));
        }

        let open_tok = self.bump();
        let inner = self.parse_type();
        let end = self
            .expect_closing(SyntaxKind::RAngle, "future<>", open_tok.range)
            .map(|t| t.range.end())
            .unwrap_or_else(|| inner.range().end());

        Type::Future(Box::new(FutureType {
            inner: Some(Box::new(inner)),
            range: TextRange::new(start, end),
        }))
    }

    fn parse_stream_type(&mut self) -> Type {
        let start = self.bump().range.start();

        if !self.at(SyntaxKind::LAngle) {
            return Type::Stream(Box::new(StreamType {
                inner: None,
                range: TextRange::new(
                    start,
                    self.tokens
                        .get(self.pos.saturating_sub(1))
                        .map(|t| t.range.end())
                        .unwrap_or(start),
                ),
            }));
        }

        let open_tok = self.bump();
        let inner = self.parse_type();
        let end = self
            .expect_closing(SyntaxKind::RAngle, "stream<>", open_tok.range)
            .map(|t| t.range.end())
            .unwrap_or_else(|| inner.range().end());

        Type::Stream(Box::new(StreamType {
            inner: Some(Box::new(inner)),
            range: TextRange::new(start, end),
        }))
    }

    /// Parse zero or more feature gates: @since(version = x.y.z) @unstable(feature = foo)
    fn parse_gates(&mut self) -> Gates {
        let mut gates: SmallVec2<Gate> = smallvec![];

        while self.at(SyntaxKind::At) {
            if let Some(gate) = self.parse_gate() {
                gates.push(gate);
            } else {
                break;
            }
        }

        Gates { gates }
    }

    fn parse_gate(&mut self) -> Option<Gate> {
        let start = self.current().range.start();

        if !self.at(SyntaxKind::At) {
            return None;
        }
        self.bump();

        let keyword = self.parse_ident()?;

        if !self.at(SyntaxKind::LParen) {
            self.error_expected("'(' after gate keyword");
            return None;
        }
        self.bump();

        let gate = match &*keyword.name {
            "since" => {
                // Parse: version = x.y.z
                self.expect_keyword_ident("version");
                self.expect(SyntaxKind::Eq);
                let version = self.parse_semver();
                let end = self
                    .expect(SyntaxKind::RParen)
                    .map(|t| t.range.end())
                    .unwrap_or(version.range.end());
                Gate::Since(SinceGate {
                    version,
                    range: TextRange::new(start, end),
                })
            }
            "unstable" => {
                // Parse: feature = name
                self.expect_keyword_ident("feature");
                self.expect(SyntaxKind::Eq);
                let feature = self
                    .parse_ident()
                    .unwrap_or_else(|| Ident::new("", TextRange::new(start, start)));
                let end = self
                    .expect(SyntaxKind::RParen)
                    .map(|t| t.range.end())
                    .unwrap_or(feature.range.end());
                Gate::Unstable(UnstableGate {
                    feature,
                    range: TextRange::new(start, end),
                })
            }
            "deprecated" => {
                // Parse: version = x.y.z
                self.expect_keyword_ident("version");
                self.expect(SyntaxKind::Eq);
                let version = self.parse_semver();
                let end = self
                    .expect(SyntaxKind::RParen)
                    .map(|t| t.range.end())
                    .unwrap_or(version.range.end());
                Gate::Deprecated(DeprecatedGate {
                    version,
                    range: TextRange::new(start, end),
                })
            }
            _ => {
                self.error_expected("gate keyword (since, unstable, deprecated)");
                // Skip to closing paren
                self.recover_to(&[SyntaxKind::RParen]);
                self.eat(SyntaxKind::RParen);
                return None;
            }
        };

        Some(gate)
    }

    /// Expect an identifier with a specific name
    fn expect_keyword_ident(&mut self, expected: &str) {
        if self.at(SyntaxKind::Ident) {
            let tok = self.current();
            let name = self.text(tok.range);
            if name == expected {
                self.bump();
                return;
            }
        }
        self.error_expected(&format!("identifier `{}`", expected));
    }

    /// Parse semver: x.y.z
    fn parse_semver(&mut self) -> Version {
        let start = self.current().range.start();

        let major = self.parse_integer().unwrap_or(0);
        self.eat(SyntaxKind::Dot);
        let minor = self.parse_integer().unwrap_or(0);
        self.eat(SyntaxKind::Dot);
        let patch = self.parse_integer().unwrap_or(0);

        let end = self
            .tokens
            .get(self.pos.saturating_sub(1))
            .map(|t| t.range.end())
            .unwrap_or(start);

        Version {
            major,
            minor,
            patch,
            range: TextRange::new(start, end),
        }
    }

    fn parse_ident(&mut self) -> Option<Ident> {
        if self.at(SyntaxKind::Ident) {
            let tok = self.bump();
            Some(Ident::new(self.text(tok.range), tok.range))
        } else {
            let cur = self.current();
            self.errors.push(ParseError::expected_ident(cur.range));
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_empty() {
        let result = parse("");
        assert!(result.is_ok());
    }

    #[test]
    fn parse_package() {
        let result = parse("package example:types@1.0.0;");
        assert!(result.is_ok());
        let pkg = result.root.package.unwrap();
        assert_eq!(pkg.namespace.len(), 1);
        assert_eq!(pkg.namespace[0].name.as_ref(), "example");
        assert_eq!(pkg.name.name.as_ref(), "types");
    }

    #[test]
    fn parse_interface() {
        let result = parse("interface foo {}");
        assert!(result.is_ok());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_interface_with_func() {
        let result = parse("interface api { get: func(id: u32) -> string; }");
        assert!(result.is_ok());
    }

    #[test]
    fn parse_interface_with_error_token() {
        let result = parse("interface api { test: func(x: §); }");
        assert!(result.has_errors());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_multiple_interfaces() {
        let content = r#"package example:api@0.1.0;

interface api {
    test: func(x: u32);
}

interface users {
    get-user: func(id: u64) -> string;
}
"#;
        let result = parse(content);
        assert!(result.is_ok());
        assert_eq!(result.root.items.len(), 2);
    }

    #[test]
    fn parse_empty_type_in_record() {
        // This should not hang - tests parser error recovery
        let content = r#"interface foo {
    record bar {
        name: ,
        email: string,
    }
}"#;
        let result = parse(content);
        assert!(result.has_errors());
        // Should still parse the interface
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_empty_type_in_func_param() {
        let content = r#"interface api {
    create-user: func(name: , email: string) -> string;
}"#;
        let result = parse(content);
        assert!(result.has_errors());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_actual_file_content() {
        let content = r#"package example:api@0.1.0;

interface types {
    record user {
        id: string,
        name: string,
        email: string,
    }

    record post {
        id: u64,
        author: user,
        title: string,
        content: string,
    }

    enum status {
        active,
        inactive,
        pending,
    }

    variant response {
        success(user),
        error(string),
    }
}

interface api {
    use types.{user, post, status, response};

    get-user: func(id: u64) -> user;
    create-user: func(name: , email: string) -> response;
    list-posts: func(author: user) -> list<post>;
    get-status: func(id: u64) -> status;
}
"#;
        let result = parse(content);
        assert!(result.has_errors());
    }

    #[test]
    fn parse_record_with_empty_first_field() {
        let content = r#"interface types {
    record user {
        id: ,
        name: string,
    }
}"#;
        let result = parse(content);
        assert!(result.has_errors());
    }

    #[test]
    fn parse_record_with_empty_field_and_more() {
        let content = r#"interface types {
    record user {
        id: ,
        name: string,
    }

    record post {
        id: u64,
    }
}"#;
        let result = parse(content);
        assert!(result.has_errors());
    }

    #[test]
    fn parse_record_three_fields_empty_first() {
        let content = r#"interface types {
    record user {
        id: ,
        name: string,
        email: string,
    }
}"#;
        let result = parse(content);
        assert!(result.has_errors());
    }

    #[test]
    fn parse_with_package_and_empty_field() {
        let content = r#"package example:api@0.1.0;

interface types {
    record user {
        id: ,
        name: string,
        email: string,
    }
}"#;
        let result = parse(content);
        assert!(result.has_errors());
    }

    #[test]
    fn parse_with_two_interfaces_and_empty_field() {
        let content = r#"package example:api@0.1.0;

interface types {
    record user {
        id: ,
        name: string,
        email: string,
    }
}

interface api {
    use types.{user};
}"#;
        let result = parse(content);
        assert!(result.has_errors());
    }

    #[test]
    fn parse_with_two_records_and_empty_field() {
        let content = r#"interface types {
    record user {
        id: ,
        name: string,
        email: string,
    }

    record post {
        id: u64,
        author: user,
    }
}"#;
        let result = parse(content);
        assert!(result.has_errors());
    }

    #[test]
    fn parse_with_records_and_enum() {
        let content = r#"interface types {
    record user {
        id: ,
        name: string,
        email: string,
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
        let result = parse(content);
        assert!(result.has_errors());
    }

    #[test]
    fn parse_with_all_type_definitions() {
        let content = r#"interface types {
    record user {
        id: ,
        name: string,
        email: string,
    }

    record post {
        id: u64,
        author: user,
    }

    enum status {
        active,
        inactive,
    }

    variant response {
        success(user),
        error(string),
    }
}"#;
        let result = parse(content);
        assert!(result.has_errors());
    }

    #[test]
    fn parse_variant_only() {
        let content = r#"interface types {
    variant response {
        success(user),
        error(string),
    }
}"#;
        let result = parse(content);
        assert!(result.is_ok());
    }

    #[test]
    fn parse_broken_record_then_variant() {
        let content = r#"interface types {
    record user {
        id: ,
    }

    variant response {
        success(user),
    }
}"#;
        let result = parse(content);
        assert!(result.has_errors());
    }

    #[test]
    fn parse_broken_record_two_fields_then_variant() {
        let content = r#"interface types {
    record user {
        id: ,
        name: string,
    }

    variant response {
        success(user),
    }
}"#;
        let result = parse(content);
        assert!(result.has_errors());
    }

    #[test]
    fn parse_empty_enum() {
        let content = r#"interface types {
    enum status {
    }
}"#;
        let result = parse(content);
        assert!(result.is_ok());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_enum_with_stray_comma() {
        let content = r#"interface types {
    enum status {
        active,
        ,
        inactive,
    }
}"#;
        let result = parse(content);
        // Should parse with errors but not hang
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_enum_with_only_commas() {
        let content = r#"interface types {
    enum status {
        ,,,
    }
}"#;
        let result = parse(content);
        // Should not hang
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_empty_flags() {
        let content = r#"interface types {
    flags permissions {
    }
}"#;
        let result = parse(content);
        assert!(result.is_ok());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_flags_with_stray_comma() {
        let content = r#"interface types {
    flags permissions {
        read,
        ,
        write,
    }
}"#;
        let result = parse(content);
        // Should parse with errors but not hang
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_empty_use_list() {
        let content = r#"interface api {
    use types.{};
}"#;
        let result = parse(content);
        // Empty use list should parse (possibly with error)
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_use_with_stray_comma() {
        let content = r#"interface api {
    use types.{user, , post};
}"#;
        let result = parse(content);
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_func_consecutive_errors() {
        // Parser accepts `x: y` as valid param (type `y`), then recovery kicks in
        let content = r#"interface api {
    test: func(x:  y:  z: u32);
}"#;
        let result = parse(content);
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_func_missing_param_names() {
        let content = r#"interface api {
    test: func(x y z) -> string;
}"#;
        let result = parse(content);
        assert!(result.has_errors());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_func_empty_params() {
        let content = r#"interface api {
    test: func(, , ,) -> string;
}"#;
        let result = parse(content);
        assert!(result.has_errors());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_func_missing_rparen() {
        let content = r#"interface api {
    test: func(x: u32 -> string;
}"#;
        let result = parse(content);
        assert!(result.has_errors());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_result_without_angle_brackets() {
        let content = r#"interface api {
    test: func() -> result;
}"#;
        let result = parse(content);
        assert!(result.is_ok());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_result_empty_angle_brackets() {
        let content = r#"interface api {
    test: func() -> result<>;
}"#;
        let result = parse(content);
        // Should parse with errors
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_result_unclosed() {
        let content = r#"interface api {
    test: func() -> result<u32;
}"#;
        let result = parse(content);
        assert!(result.has_errors());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_result_missing_error_type() {
        let content = r#"interface api {
    test: func() -> result<u32,>;
}"#;
        let result = parse(content);
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_result_with_underscore() {
        let content = r#"interface api {
    test: func() -> result<_, string>;
}"#;
        let result = parse(content);
        assert!(result.is_ok());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_tuple_trailing_comma() {
        let content = r#"interface api {
    type coords = tuple<u32, u32,>;
}"#;
        let result = parse(content);
        // May have errors but should not hang
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_tuple_empty() {
        let content = r#"interface api {
    type empty = tuple<>;
}"#;
        let result = parse(content);
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_tuple_unclosed() {
        let content = r#"interface api {
    type bad = tuple<u32, u32;
}"#;
        let result = parse(content);
        assert!(result.has_errors());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_borrow_empty() {
        let content = r#"interface api {
    test: func(h: borrow<>) -> u32;
}"#;
        let result = parse(content);
        assert!(result.has_errors());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_own_empty() {
        let content = r#"interface api {
    test: func(h: own<>) -> u32;
}"#;
        let result = parse(content);
        assert!(result.has_errors());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_borrow_unclosed() {
        let content = r#"interface api {
    test: func(h: borrow<handle) -> u32;
}"#;
        let result = parse(content);
        assert!(result.has_errors());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_variant_empty_parens() {
        let content = r#"interface types {
    variant response {
        success(),
        error(string),
    }
}"#;
        let result = parse(content);
        // Empty parens should parse but may have errors
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_variant_missing_case_name() {
        let content = r#"interface types {
    variant response {
        (string),
        error(string),
    }
}"#;
        let result = parse(content);
        assert!(result.has_errors());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_variant_unclosed_paren() {
        let content = r#"interface types {
    variant response {
        success(string,
        error(string),
    }
}"#;
        let result = parse(content);
        assert!(result.has_errors());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_empty_variant() {
        let content = r#"interface types {
    variant response {
    }
}"#;
        let result = parse(content);
        assert!(result.is_ok());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_record_missing_field_name() {
        let content = r#"interface types {
    record user {
        : string,
        name: string,
    }
}"#;
        let result = parse(content);
        assert!(result.has_errors());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_record_missing_colon() {
        let content = r#"interface types {
    record user {
        id string,
        name: string,
    }
}"#;
        let result = parse(content);
        assert!(result.has_errors());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_empty_record() {
        let content = r#"interface types {
    record empty {
    }
}"#;
        let result = parse(content);
        assert!(result.is_ok());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_record_only_commas() {
        let content = r#"interface types {
    record bad {
        ,,,
    }
}"#;
        let result = parse(content);
        // Should not hang
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_interface_missing_name() {
        let content = r#"interface {
    test: func();
}"#;
        let result = parse(content);
        assert!(result.has_errors());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_interface_missing_brace() {
        let content = r#"interface api
    test: func();
}"#;
        let result = parse(content);
        assert!(result.has_errors());
    }

    #[test]
    fn parse_world_missing_name() {
        let content = r#"world {
    import api: interface {
        test: func();
    }
}"#;
        let result = parse(content);
        assert!(result.has_errors());
    }

    #[test]
    fn parse_world_empty() {
        let content = r#"world my-world {
}"#;
        let result = parse(content);
        assert!(result.is_ok());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_list_empty() {
        let content = r#"interface api {
    test: func() -> list<>;
}"#;
        let result = parse(content);
        assert!(result.has_errors());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_list_unclosed() {
        let content = r#"interface api {
    test: func() -> list<u32;
}"#;
        let result = parse(content);
        assert!(result.has_errors());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_option_empty() {
        let content = r#"interface api {
    test: func() -> option<>;
}"#;
        let result = parse(content);
        assert!(result.has_errors());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_resource_empty() {
        let content = r#"interface api {
    resource handle {
    }
}"#;
        let result = parse(content);
        assert!(result.is_ok());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_opaque_resource() {
        // Opaque resource declaration without body (like wasi:sockets/network)
        let content = r#"interface network {
    resource network;
    resource other-thing;
}"#;
        let result = parse(content);
        assert!(result.is_ok(), "Parse failed: {:?}", result.errors);

        if let Item::Interface(iface) = &result.root.items[0] {
            assert_eq!(iface.items.len(), 2);
            if let InterfaceItem::TypeDef(TypeDef::Resource(res)) = &iface.items[0] {
                assert_eq!(res.name.name.as_ref(), "network");
                assert!(res.items.is_empty(), "opaque resource should have no items");
            } else {
                panic!("expected resource");
            }
        } else {
            panic!("expected interface");
        }
    }

    #[test]
    fn parse_opaque_resource_with_gate() {
        let content = r#"interface network {
    @since(version = 0.2.0)
    resource network;
}"#;
        let result = parse(content);
        assert!(result.is_ok(), "Parse failed: {:?}", result.errors);

        if let Item::Interface(iface) = &result.root.items[0] {
            if let InterfaceItem::TypeDef(TypeDef::Resource(res)) = &iface.items[0] {
                assert!(!res.gates.is_empty());
            } else {
                panic!("expected resource");
            }
        } else {
            panic!("expected interface");
        }
    }

    #[test]
    fn parse_resource_missing_name() {
        let content = r#"interface api {
    resource {
        get: func() -> u32;
    }
}"#;
        let result = parse(content);
        assert!(result.has_errors());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_resource_malformed_constructor() {
        let content = r#"interface api {
    resource handle {
        constructor(;
        get: func() -> u32;
    }
}"#;
        let result = parse(content);
        assert!(result.has_errors());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_stray_rbrace_at_top_level() {
        let content = r#"interface api {
    test: func();
}
}
interface other {
    test2: func();
}"#;
        let result = parse(content);
        assert!(result.has_errors());
        // Should still parse both interfaces
        assert_eq!(result.root.items.len(), 2);
    }

    #[test]
    fn parse_multiple_stray_rbraces() {
        let content = r#"interface api {
    test: func();
}
}}}
interface other {
    test2: func();
}"#;
        let result = parse(content);
        assert!(result.has_errors());
        assert_eq!(result.root.items.len(), 2);
    }

    #[test]
    fn parse_stray_semicolons() {
        let content = r#";;;
interface api {
    test: func();
}
;;;"#;
        let result = parse(content);
        // Stray semicolons should be handled
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_deeply_nested_types() {
        let content = r#"interface api {
    test: func() -> list<option<result<tuple<u32, u32>, string>>>;
}"#;
        let result = parse(content);
        assert!(result.is_ok());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_nested_type_unclosed() {
        let content = r#"interface api {
    test: func() -> list<option<result<tuple<u32, u32;
}"#;
        let result = parse(content);
        assert!(result.has_errors());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_package_missing_version() {
        let content = r#"package example:api;

interface types {
    test: func();
}"#;
        let result = parse(content);
        // Package without version should be valid
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_package_malformed() {
        let content = r#"package ;

interface types {
    test: func();
}"#;
        let result = parse(content);
        assert!(result.has_errors());
    }

    #[test]
    fn parse_type_alias_missing_equals() {
        let content = r#"interface api {
    type user-id string;
}"#;
        let result = parse(content);
        assert!(result.has_errors());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_type_alias_missing_type() {
        let content = r#"interface api {
    type user-id = ;
}"#;
        let result = parse(content);
        assert!(result.has_errors());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_doc_comment_without_item() {
        let content = r#"interface api {
    /// This documents nothing
}"#;
        let result = parse(content);
        // Trailing doc comment should be handled
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_multiple_doc_comments() {
        let content = r#"interface api {
    /// First comment
    /// Second comment
    /// Third comment
    test: func();
}"#;
        let result = parse(content);
        assert!(result.is_ok());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_multiple_errors_in_one_interface() {
        let content = r#"interface types {
    record user {
        id: ,
        name: string,
    }

    enum status {
        ,
        active,
    }

    variant response {
        success(),
    }

    test: func(x: , y: ) -> result<>;
}"#;
        let result = parse(content);
        assert!(result.has_errors());
        // Parser may split interface due to error recovery - just verify it doesn't hang
        assert!(result.root.items.len() >= 1);
    }

    #[test]
    fn parse_error_recovery_preserves_valid_items() {
        let content = r#"interface types {
    record broken {
        id: ,
    }

    record valid {
        id: u64,
        name: string,
    }

    enum broken-enum {
        ,,,
    }

    enum valid-enum {
        a,
        b,
        c,
    }
}"#;
        let result = parse(content);
        assert!(result.has_errors());
        // All items should be parsed
        if let Some(Item::Interface(iface)) = result.root.items.first() {
            assert_eq!(iface.items.len(), 4);
        } else {
            panic!("Expected interface");
        }
    }

    #[test]
    fn parse_future_with_type() {
        let content = r#"interface api {
    start-task: func() -> future<string>;
}"#;
        let result = parse(content);
        assert!(result.is_ok());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_future_bare() {
        let content = r#"interface api {
    start-task: func() -> future;
}"#;
        let result = parse(content);
        assert!(result.is_ok());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_stream_with_type() {
        let content = r#"interface api {
    read-data: func() -> stream<u8>;
}"#;
        let result = parse(content);
        assert!(result.is_ok());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_stream_bare() {
        let content = r#"interface api {
    events: func() -> stream;
}"#;
        let result = parse(content);
        assert!(result.is_ok());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_future_nested() {
        let content = r#"interface api {
    complex: func() -> future<result<string, error>>;
}"#;
        let result = parse(content);
        assert!(result.is_ok());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_stream_of_futures() {
        let content = r#"interface api {
    task-stream: func() -> stream<future<string>>;
}"#;
        let result = parse(content);
        assert!(result.is_ok());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_future_unclosed() {
        let content = r#"interface api {
    task: func() -> future<string;
}"#;
        let result = parse(content);
        assert!(result.has_errors());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_stream_empty_angle() {
        let content = r#"interface api {
    data: func() -> stream<>;
}"#;
        let result = parse(content);
        assert!(result.has_errors());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_future_as_param() {
        let content = r#"interface api {
    await-task: func(task: future<string>) -> string;
}"#;
        let result = parse(content);
        assert!(result.is_ok());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_stream_as_param() {
        let content = r#"interface api {
    process: func(data: stream<u8>) -> u64;
}"#;
        let result = parse(content);
        assert!(result.is_ok());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_type_alias_future() {
        let content = r#"interface api {
    type async-result = future<result<string, error>>;
}"#;
        let result = parse(content);
        assert!(result.is_ok());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_type_alias_stream() {
        let content = r#"interface api {
    type byte-stream = stream<u8>;
}"#;
        let result = parse(content);
        assert!(result.is_ok());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_async_func() {
        let content = r#"interface api {
    fetch: async func(url: string) -> response;
}"#;
        let result = parse(content);
        assert!(result.is_ok());
        assert_eq!(result.root.items.len(), 1);

        if let Some(Item::Interface(iface)) = result.root.items.first() {
            if let Some(InterfaceItem::Func(f)) = iface.items.first() {
                assert!(f.sig.is_async);
            } else {
                panic!("Expected func");
            }
        } else {
            panic!("Expected interface");
        }
    }

    #[test]
    fn parse_sync_func_not_async() {
        let content = r#"interface api {
    get: func() -> string;
}"#;
        let result = parse(content);
        assert!(result.is_ok());

        if let Some(Item::Interface(iface)) = result.root.items.first() {
            if let Some(InterfaceItem::Func(f)) = iface.items.first() {
                assert!(!f.sig.is_async);
            } else {
                panic!("Expected func");
            }
        } else {
            panic!("Expected interface");
        }
    }

    #[test]
    fn parse_async_func_no_return() {
        let content = r#"interface api {
    process: async func(data: string);
}"#;
        let result = parse(content);
        assert!(result.is_ok());
        assert_eq!(result.root.items.len(), 1);

        if let Some(Item::Interface(iface)) = result.root.items.first() {
            if let Some(InterfaceItem::Func(f)) = iface.items.first() {
                assert!(f.sig.is_async);
            }
        }
    }

    #[test]
    fn parse_async_func_with_future_return() {
        let content = r#"interface api {
    start: async func() -> future<string>;
}"#;
        let result = parse(content);
        assert!(result.is_ok());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_multiple_async_funcs() {
        let content = r#"interface api {
    sync-op: func() -> u32;
    async-op: async func() -> string;
    another-sync: func(x: u32);
    another-async: async func(data: stream<u8>) -> result<u64, error>;
}"#;
        let result = parse(content);
        assert!(result.is_ok());

        if let Some(Item::Interface(iface)) = result.root.items.first() {
            assert_eq!(iface.items.len(), 4);

            // Check each function's async status
            let async_flags: Vec<bool> = iface
                .items
                .iter()
                .filter_map(|item| {
                    if let InterfaceItem::Func(f) = item {
                        Some(f.sig.is_async)
                    } else {
                        None
                    }
                })
                .collect();

            assert_eq!(async_flags, vec![false, true, false, true]);
        }
    }

    #[test]
    fn parse_async_in_world_export() {
        let content = r#"world my-world {
    export run: async func() -> result<string, error>;
}"#;
        let result = parse(content);
        assert!(result.is_ok());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_async_in_world_import() {
        let content = r#"world my-world {
    import fetch: async func(url: string) -> response;
}"#;
        let result = parse(content);
        assert!(result.is_ok());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_async_resource_method() {
        let content = r#"interface api {
    resource connection {
        read: async func(size: u32) -> stream<u8>;
        write: async func(data: stream<u8>) -> result<u64, error>;
    }
}"#;
        let result = parse(content);
        assert!(result.is_ok());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_async_static_method() {
        let content = r#"interface api {
    resource connection {
        connect: static async func(url: string) -> result<connection, error>;
    }
}"#;
        let result = parse(content);
        assert!(result.is_ok());
        assert_eq!(result.root.items.len(), 1);
    }

    #[test]
    fn parse_since_gate_on_interface() {
        let content = r#"@since(version = 1.0.0)
interface api {
    test: func();
}"#;
        let result = parse(content);
        assert!(result.is_ok());
        assert_eq!(result.root.items.len(), 1);

        if let Some(Item::Interface(iface)) = result.root.items.first() {
            assert_eq!(iface.gates.gates.len(), 1);
            assert!(matches!(&iface.gates.gates[0], Gate::Since(_)));
        } else {
            panic!("Expected interface");
        }
    }

    #[test]
    fn parse_unstable_gate_on_func() {
        let content = r#"interface api {
    @unstable(feature = experimental-api)
    test: func();
}"#;
        let result = parse(content);
        assert!(result.is_ok());

        if let Some(Item::Interface(iface)) = result.root.items.first() {
            if let Some(InterfaceItem::Func(f)) = iface.items.first() {
                assert_eq!(f.gates.gates.len(), 1);
                if let Gate::Unstable(g) = &f.gates.gates[0] {
                    assert_eq!(g.feature.name.as_ref(), "experimental-api");
                } else {
                    panic!("Expected unstable gate");
                }
            }
        }
    }

    #[test]
    fn parse_deprecated_gate_on_func() {
        let content = r#"interface api {
    @deprecated(version = 2.0.0)
    old-api: func();
}"#;
        let result = parse(content);
        assert!(result.is_ok());

        if let Some(Item::Interface(iface)) = result.root.items.first() {
            if let Some(InterfaceItem::Func(f)) = iface.items.first() {
                assert_eq!(f.gates.gates.len(), 1);
                if let Gate::Deprecated(g) = &f.gates.gates[0] {
                    assert_eq!(g.version.major, 2);
                    assert_eq!(g.version.minor, 0);
                    assert_eq!(g.version.patch, 0);
                } else {
                    panic!("Expected deprecated gate");
                }
            }
        }
    }

    #[test]
    fn parse_multiple_gates() {
        let content = r#"interface api {
    @since(version = 1.0.0)
    @deprecated(version = 2.0.0)
    legacy-func: func();
}"#;
        let result = parse(content);
        assert!(result.is_ok());

        if let Some(Item::Interface(iface)) = result.root.items.first() {
            if let Some(InterfaceItem::Func(f)) = iface.items.first() {
                assert_eq!(f.gates.gates.len(), 2);
                assert!(matches!(&f.gates.gates[0], Gate::Since(_)));
                assert!(matches!(&f.gates.gates[1], Gate::Deprecated(_)));
            }
        }
    }

    #[test]
    fn parse_gate_on_record() {
        let content = r#"interface api {
    @since(version = 0.2.0)
    record user {
        id: u64,
        name: string,
    }
}"#;
        let result = parse(content);
        assert!(result.is_ok());

        if let Some(Item::Interface(iface)) = result.root.items.first() {
            if let Some(InterfaceItem::TypeDef(TypeDef::Record(r))) = iface.items.first() {
                assert_eq!(r.gates.gates.len(), 1);
            }
        }
    }

    #[test]
    fn parse_gate_on_world() {
        let content = r#"@since(version = 0.2.0)
world my-world {
    export run: func();
}"#;
        let result = parse(content);
        assert!(result.is_ok());

        if let Some(Item::World(w)) = result.root.items.first() {
            assert_eq!(w.gates.gates.len(), 1);
        }
    }

    #[test]
    fn parse_gate_with_docs() {
        let content = r#"interface api {
    @since(version = 1.0.0)
    /// This is a documented function.
    test: func();
}"#;
        let result = parse(content);
        assert!(result.is_ok());

        if let Some(Item::Interface(iface)) = result.root.items.first() {
            if let Some(InterfaceItem::Func(f)) = iface.items.first() {
                assert_eq!(f.gates.gates.len(), 1);
                assert!(f.docs.is_some());
            }
        }
    }

    #[test]
    fn parse_no_gates() {
        let content = r#"interface api {
    test: func();
}"#;
        let result = parse(content);
        assert!(result.is_ok());

        if let Some(Item::Interface(iface)) = result.root.items.first() {
            assert!(iface.gates.is_empty());
            if let Some(InterfaceItem::Func(f)) = iface.items.first() {
                assert!(f.gates.is_empty());
            }
        }
    }

    #[test]
    fn parse_unstable_gate_wasi_style() {
        let content = r#"@unstable(feature = wasi-clocks)
interface clocks {
    @unstable(feature = wall-clock)
    now: func() -> datetime;
}"#;
        let result = parse(content);
        assert!(result.is_ok());

        if let Some(Item::Interface(iface)) = result.root.items.first() {
            assert_eq!(iface.gates.gates.len(), 1);
            if let Gate::Unstable(g) = &iface.gates.gates[0] {
                assert_eq!(g.feature.name.as_ref(), "wasi-clocks");
            }
        }
    }

    #[test]
    fn parse_include_basic() {
        let content = r#"world my-world {
    include other-world;
}"#;
        let result = parse(content);
        assert!(result.is_ok());

        if let Some(Item::World(world)) = result.root.items.first() {
            assert_eq!(world.items.len(), 1);
            if let WorldItem::Include(inc) = &world.items[0] {
                assert!(inc.with.is_empty());
            } else {
                panic!("expected include");
            }
        }
    }

    #[test]
    fn parse_include_with_single_rename() {
        let content = r#"world my-world {
    include other-world with { foo as bar };
}"#;
        let result = parse(content);
        assert!(result.is_ok());

        if let Some(Item::World(world)) = result.root.items.first() {
            if let WorldItem::Include(inc) = &world.items[0] {
                assert_eq!(inc.with.len(), 1);
                assert_eq!(inc.with[0].name.name.as_ref(), "foo");
                assert_eq!(inc.with[0].alias.name.as_ref(), "bar");
            } else {
                panic!("expected include");
            }
        }
    }

    #[test]
    fn parse_include_with_multiple_renames() {
        let content = r#"world my-world {
    include other-world with { foo as bar, baz as qux };
}"#;
        let result = parse(content);
        assert!(result.is_ok());

        if let Some(Item::World(world)) = result.root.items.first() {
            if let WorldItem::Include(inc) = &world.items[0] {
                assert_eq!(inc.with.len(), 2);
                assert_eq!(inc.with[0].name.name.as_ref(), "foo");
                assert_eq!(inc.with[0].alias.name.as_ref(), "bar");
                assert_eq!(inc.with[1].name.name.as_ref(), "baz");
                assert_eq!(inc.with[1].alias.name.as_ref(), "qux");
            } else {
                panic!("expected include");
            }
        }
    }

    #[test]
    fn parse_include_with_trailing_comma() {
        let content = r#"world my-world {
    include other-world with { foo as bar, };
}"#;
        let result = parse(content);
        assert!(result.is_ok());

        if let Some(Item::World(world)) = result.root.items.first() {
            if let WorldItem::Include(inc) = &world.items[0] {
                assert_eq!(inc.with.len(), 1);
            } else {
                panic!("expected include");
            }
        }
    }

    #[test]
    fn parse_constructor_basic() {
        let content = r#"interface test {
    resource my-resource {
        constructor();
    }
}"#;
        let result = parse(content);
        assert!(result.is_ok());

        if let Some(Item::Interface(iface)) = result.root.items.first() {
            if let Some(InterfaceItem::TypeDef(TypeDef::Resource(res))) = iface.items.first() {
                assert_eq!(res.items.len(), 1);
                if let ResourceItem::Constructor(c) = &res.items[0] {
                    assert!(c.result.is_none());
                    assert!(c.params.is_empty());
                }
            }
        }
    }

    #[test]
    fn parse_constructor_with_params() {
        let content = r#"interface test {
    resource my-resource {
        constructor(name: string, age: u32);
    }
}"#;
        let result = parse(content);
        assert!(result.is_ok());

        if let Some(Item::Interface(iface)) = result.root.items.first() {
            if let Some(InterfaceItem::TypeDef(TypeDef::Resource(res))) = iface.items.first() {
                if let ResourceItem::Constructor(c) = &res.items[0] {
                    assert!(c.result.is_none());
                    assert_eq!(c.params.len(), 2);
                }
            }
        }
    }

    #[test]
    fn parse_fallible_constructor_result() {
        let content = r#"interface test {
    resource my-resource {
        constructor(name: string) -> result<_, init-error>;
    }
}"#;
        let result = parse(content);
        assert!(result.is_ok());

        if let Some(Item::Interface(iface)) = result.root.items.first() {
            if let Some(InterfaceItem::TypeDef(TypeDef::Resource(res))) = iface.items.first() {
                if let ResourceItem::Constructor(c) = &res.items[0] {
                    assert!(c.result.is_some());
                    assert_eq!(c.params.len(), 1);
                }
            }
        }
    }

    #[test]
    fn parse_fallible_constructor_simple_error() {
        let content = r#"interface test {
    resource my-resource {
        constructor() -> result<_, string>;
    }
}"#;
        let result = parse(content);
        assert!(result.is_ok());

        if let Some(Item::Interface(iface)) = result.root.items.first() {
            if let Some(InterfaceItem::TypeDef(TypeDef::Resource(res))) = iface.items.first() {
                if let ResourceItem::Constructor(c) = &res.items[0] {
                    assert!(c.result.is_some());
                }
            }
        }
    }

    #[test]
    fn parse_fallible_constructor_result_error_only() {
        let content = r#"interface test {
    resource file {
        constructor(path: string) -> result<_, io-error>;
    }
}"#;
        let result = parse(content);
        assert!(result.is_ok());

        if let Some(Item::Interface(iface)) = result.root.items.first() {
            if let Some(InterfaceItem::TypeDef(TypeDef::Resource(res))) = iface.items.first() {
                if let ResourceItem::Constructor(c) = &res.items[0] {
                    assert!(c.result.is_some());
                    if let Type::Result(r) = c.result.as_ref().unwrap() {
                        // The ok type should be None (underscore)
                        assert!(r.ok.is_none());
                        // The err type should be present
                        assert!(r.err.is_some());
                    } else {
                        panic!("expected result type");
                    }
                }
            }
        }
    }

    #[test]
    fn parse_nested_package_single() {
        let content = r#"package local:foo {
    interface bar {}
}"#;
        let result = parse(content);
        assert!(result.is_ok());

        assert!(result.root.package.is_none());
        assert_eq!(result.root.nested_packages.len(), 1);

        let pkg = &result.root.nested_packages[0];
        assert_eq!(pkg.package.namespace.len(), 1);
        assert_eq!(pkg.package.namespace[0].name.as_ref(), "local");
        assert_eq!(pkg.package.name.name.as_ref(), "foo");
        assert_eq!(pkg.items.len(), 1);
    }

    #[test]
    fn parse_nested_package_multiple() {
        let content = r#"package local:a {
    interface foo {}
}

package local:b {
    interface bar {}
}"#;
        let result = parse(content);
        assert!(result.is_ok());

        assert!(result.root.package.is_none());
        assert_eq!(result.root.nested_packages.len(), 2);

        assert_eq!(
            result.root.nested_packages[0].package.name.name.as_ref(),
            "a"
        );
        assert_eq!(
            result.root.nested_packages[1].package.name.name.as_ref(),
            "b"
        );
    }

    #[test]
    fn parse_nested_package_with_version() {
        let content = r#"package wasi:http@1.0.0 {
    interface types {
        record request {
            url: string,
        }
    }
}"#;
        let result = parse(content);
        assert!(result.is_ok());

        let pkg = &result.root.nested_packages[0];
        assert_eq!(pkg.package.namespace.len(), 1);
        assert_eq!(pkg.package.namespace[0].name.as_ref(), "wasi");
        assert_eq!(pkg.package.name.name.as_ref(), "http");
        assert!(pkg.package.version.is_some());
        let version = pkg.package.version.as_ref().unwrap();
        assert_eq!(version.major, 1);
        assert_eq!(version.minor, 0);
        assert_eq!(version.patch, 0);
    }

    #[test]
    fn parse_nested_package_with_world() {
        let content = r#"package example:test {
    world my-world {
        import foo: func();
        export bar: func();
    }
}"#;
        let result = parse(content);
        assert!(result.is_ok());

        let pkg = &result.root.nested_packages[0];
        assert_eq!(pkg.items.len(), 1);
        if let Item::World(world) = &pkg.items[0] {
            assert_eq!(world.name.name.as_ref(), "my-world");
            assert_eq!(world.items.len(), 2);
        } else {
            panic!("expected world");
        }
    }

    #[test]
    fn parse_nested_package_with_uses() {
        let content = r#"package local:foo {
    use wasi:http/types@1.0.0;

    interface bar {
        use types.{request};
    }
}"#;
        let result = parse(content);
        assert!(result.is_ok());

        let pkg = &result.root.nested_packages[0];
        assert_eq!(pkg.uses.len(), 1);
        assert_eq!(pkg.items.len(), 1);
    }

    #[test]
    fn parse_standalone_then_nested() {
        let content = r#"package main:app@1.0.0;

package local:helper {
    interface utils {}
}"#;
        let result = parse(content);
        assert!(result.is_ok());

        // Should have both standalone and nested
        assert!(result.root.package.is_some());
        assert_eq!(
            result.root.package.as_ref().unwrap().name.name.as_ref(),
            "app"
        );

        assert_eq!(result.root.nested_packages.len(), 1);
        assert_eq!(
            result.root.nested_packages[0].package.name.name.as_ref(),
            "helper"
        );
    }

    #[test]
    fn parse_nested_package_empty() {
        let content = r#"package local:empty {
}"#;
        let result = parse(content);
        assert!(result.is_ok());

        assert_eq!(result.root.nested_packages.len(), 1);
        let pkg = &result.root.nested_packages[0];
        assert!(pkg.items.is_empty());
        assert!(pkg.uses.is_empty());
    }

    #[test]
    fn parse_mixed_toplevel_and_nested() {
        let content = r#"package main:app;

interface shared {}

package local:helper {
    interface utils {}
}

world main-world {
    export shared;
}"#;
        let result = parse(content);
        assert!(result.is_ok());

        assert!(result.root.package.is_some());
        assert_eq!(result.root.items.len(), 2); // shared interface + main-world
        assert_eq!(result.root.nested_packages.len(), 1);
    }

    #[test]
    fn parse_simple_namespace() {
        let content = "package wasi:http;";
        let result = parse(content);
        assert!(result.is_ok());

        let pkg = result.root.package.as_ref().unwrap();
        assert_eq!(pkg.namespace.len(), 1);
        assert_eq!(pkg.namespace[0].name.as_ref(), "wasi");
        assert_eq!(pkg.name.name.as_ref(), "http");
        assert!(pkg.nested.is_empty());
    }

    #[test]
    fn parse_nested_namespace_two_levels() {
        let content = "package foo:bar:baz;";
        let result = parse(content);
        assert!(result.is_ok());

        let pkg = result.root.package.as_ref().unwrap();
        assert_eq!(pkg.namespace.len(), 2);
        assert_eq!(pkg.namespace[0].name.as_ref(), "foo");
        assert_eq!(pkg.namespace[1].name.as_ref(), "bar");
        assert_eq!(pkg.name.name.as_ref(), "baz");
        assert!(pkg.nested.is_empty());
    }

    #[test]
    fn parse_nested_namespace_three_levels() {
        let content = "package a:b:c:d;";
        let result = parse(content);
        assert!(result.is_ok());

        let pkg = result.root.package.as_ref().unwrap();
        assert_eq!(pkg.namespace.len(), 3);
        assert_eq!(pkg.namespace[0].name.as_ref(), "a");
        assert_eq!(pkg.namespace[1].name.as_ref(), "b");
        assert_eq!(pkg.namespace[2].name.as_ref(), "c");
        assert_eq!(pkg.name.name.as_ref(), "d");
    }

    #[test]
    fn parse_nested_package_path() {
        let content = "package wasi:http/types;";
        let result = parse(content);
        assert!(result.is_ok());

        let pkg = result.root.package.as_ref().unwrap();
        assert_eq!(pkg.namespace.len(), 1);
        assert_eq!(pkg.namespace[0].name.as_ref(), "wasi");
        assert_eq!(pkg.name.name.as_ref(), "http");
        assert_eq!(pkg.nested.len(), 1);
        assert_eq!(pkg.nested[0].name.as_ref(), "types");
    }

    #[test]
    fn parse_nested_package_path_multiple() {
        let content = "package wasi:http/types/streams;";
        let result = parse(content);
        assert!(result.is_ok());

        let pkg = result.root.package.as_ref().unwrap();
        assert_eq!(pkg.namespace.len(), 1);
        assert_eq!(pkg.namespace[0].name.as_ref(), "wasi");
        assert_eq!(pkg.name.name.as_ref(), "http");
        assert_eq!(pkg.nested.len(), 2);
        assert_eq!(pkg.nested[0].name.as_ref(), "types");
        assert_eq!(pkg.nested[1].name.as_ref(), "streams");
    }

    #[test]
    fn parse_nested_namespace_and_package_combined() {
        // foo:bar:baz/quux - bar is nested namespace of foo, quux is nested package of baz
        let content = "package foo:bar:baz/quux;";
        let result = parse(content);
        assert!(result.is_ok());

        let pkg = result.root.package.as_ref().unwrap();
        assert_eq!(pkg.namespace.len(), 2);
        assert_eq!(pkg.namespace[0].name.as_ref(), "foo");
        assert_eq!(pkg.namespace[1].name.as_ref(), "bar");
        assert_eq!(pkg.name.name.as_ref(), "baz");
        assert_eq!(pkg.nested.len(), 1);
        assert_eq!(pkg.nested[0].name.as_ref(), "quux");
    }

    #[test]
    fn parse_nested_namespace_with_version() {
        let content = "package foo:bar:baz@1.2.3;";
        let result = parse(content);
        assert!(result.is_ok());

        let pkg = result.root.package.as_ref().unwrap();
        assert_eq!(pkg.namespace.len(), 2);
        assert_eq!(pkg.name.name.as_ref(), "baz");
        assert!(pkg.version.is_some());
        let v = pkg.version.as_ref().unwrap();
        assert_eq!(v.major, 1);
        assert_eq!(v.minor, 2);
        assert_eq!(v.patch, 3);
    }

    #[test]
    fn parse_full_nested_namespace_example() {
        // Full example: foo:bar:baz/quux/deep@1.0.0
        let content = "package foo:bar:baz/quux/deep@1.0.0;";
        let result = parse(content);
        assert!(result.is_ok());

        let pkg = result.root.package.as_ref().unwrap();
        assert_eq!(pkg.namespace.len(), 2);
        assert_eq!(pkg.namespace[0].name.as_ref(), "foo");
        assert_eq!(pkg.namespace[1].name.as_ref(), "bar");
        assert_eq!(pkg.name.name.as_ref(), "baz");
        assert_eq!(pkg.nested.len(), 2);
        assert_eq!(pkg.nested[0].name.as_ref(), "quux");
        assert_eq!(pkg.nested[1].name.as_ref(), "deep");
        assert!(pkg.version.is_some());
    }

    #[test]
    fn parse_nested_namespace_in_nested_package_def() {
        let content = r#"package foo:bar:baz {
    interface test {}
}"#;
        let result = parse(content);
        assert!(result.is_ok());

        assert_eq!(result.root.nested_packages.len(), 1);
        let pkg = &result.root.nested_packages[0].package;
        assert_eq!(pkg.namespace.len(), 2);
        assert_eq!(pkg.namespace[0].name.as_ref(), "foo");
        assert_eq!(pkg.namespace[1].name.as_ref(), "bar");
        assert_eq!(pkg.name.name.as_ref(), "baz");
    }

    #[test]
    fn parse_wasi_random_insecure() {
        // From https://github.com/WebAssembly/wasi-random
        let content = r#"package wasi:random@0.2.8;

/// The insecure interface for insecure pseudo-random numbers.
@since(version = 0.2.0)
interface insecure {
    /// Return `len` insecure pseudo-random bytes.
    @since(version = 0.2.0)
    get-insecure-random-bytes: func(len: u64) -> list<u8>;

    /// Return an insecure pseudo-random `u64` value.
    @since(version = 0.2.0)
    get-insecure-random-u64: func() -> u64;
}"#;
        let result = parse(content);
        assert!(result.is_ok(), "Parse failed: {:?}", result.errors);

        let pkg = result.root.package.as_ref().unwrap();
        assert_eq!(pkg.namespace[0].name.as_ref(), "wasi");
        assert_eq!(pkg.name.name.as_ref(), "random");
        assert!(pkg.version.is_some());

        assert_eq!(result.root.items.len(), 1);
        if let Item::Interface(iface) = &result.root.items[0] {
            assert_eq!(iface.name.name.as_ref(), "insecure");
            assert_eq!(iface.items.len(), 2);
            assert!(!iface.gates.is_empty());
        } else {
            panic!("expected interface");
        }
    }

    #[test]
    fn parse_wasi_clocks_wall_clock() {
        // From https://github.com/WebAssembly/wasi-clocks
        let content = r#"package wasi:clocks@0.2.8;

/// WASI Wall Clock is a clock API intended to let users query the current time.
@since(version = 0.2.0)
interface wall-clock {
    /// A time and date in seconds plus nanoseconds.
    @since(version = 0.2.0)
    record datetime {
        seconds: u64,
        nanoseconds: u32,
    }

    /// Read the current value of the clock.
    @since(version = 0.2.0)
    now: func() -> datetime;

    /// Query the resolution of the clock.
    @since(version = 0.2.0)
    resolution: func() -> datetime;
}"#;
        let result = parse(content);
        assert!(result.is_ok(), "Parse failed: {:?}", result.errors);

        if let Item::Interface(iface) = &result.root.items[0] {
            assert_eq!(iface.name.name.as_ref(), "wall-clock");
            // Should have record + 2 functions
            assert_eq!(iface.items.len(), 3);
        } else {
            panic!("expected interface");
        }
    }

    #[test]
    fn parse_wasi_cli_command() {
        // From https://github.com/WebAssembly/wasi-cli
        let content = r#"package wasi:cli@0.2.8;

@since(version = 0.2.0)
world command {
  @since(version = 0.2.0)
  include imports;

  @since(version = 0.2.0)
  export run;
}"#;
        let result = parse(content);
        assert!(result.is_ok(), "Parse failed: {:?}", result.errors);

        if let Item::World(world) = &result.root.items[0] {
            assert_eq!(world.name.name.as_ref(), "command");
            assert!(!world.gates.is_empty());
            // include + export
            assert_eq!(world.items.len(), 2);
        } else {
            panic!("expected world");
        }
    }

    #[test]
    fn parse_wasi_http_variant_with_many_cases() {
        // Simplified from wasi-http types.wit error-code variant
        let content = r#"package wasi:http@0.2.8;

interface types {
    @since(version = 0.2.0)
    variant error-code {
        DNS-timeout,
        DNS-error(DNS-error-payload),
        destination-not-found,
        destination-unavailable,
        connection-refused,
        connection-terminated,
        connection-timeout,
        TLS-protocol-error,
        TLS-certificate-error,
        HTTP-request-denied,
        HTTP-response-incomplete,
        loop-detected,
        configuration-error,
        internal-error(option<string>)
    }

    @since(version = 0.2.0)
    record DNS-error-payload {
        rcode: option<string>,
        info-code: option<u16>
    }
}"#;
        let result = parse(content);
        assert!(result.is_ok(), "Parse failed: {:?}", result.errors);

        if let Item::Interface(iface) = &result.root.items[0] {
            // variant + record
            assert_eq!(iface.items.len(), 2);
        } else {
            panic!("expected interface");
        }
    }

    #[test]
    fn parse_wasi_http_method_variant() {
        // From wasi-http types.wit
        let content = r#"package wasi:http@0.2.8;

interface types {
    @since(version = 0.2.0)
    variant method {
        get,
        head,
        post,
        put,
        delete,
        connect,
        options,
        trace,
        patch,
        other(string)
    }

    @since(version = 0.2.0)
    variant scheme {
        HTTP,
        HTTPS,
        other(string)
    }
}"#;
        let result = parse(content);
        assert!(result.is_ok(), "Parse failed: {:?}", result.errors);

        if let Item::Interface(iface) = &result.root.items[0] {
            assert_eq!(iface.items.len(), 2);
        } else {
            panic!("expected interface");
        }
    }

    #[test]
    fn parse_wasi_http_resource_with_methods() {
        // From wasi-http types.wit - fields resource
        let content = r#"package wasi:http@0.2.8;

interface types {
    type field-name = string;
    type field-value = list<u8>;

    @since(version = 0.2.0)
    variant header-error {
        invalid-syntax,
        forbidden,
        immutable,
    }

    @since(version = 0.2.0)
    resource fields {
        @since(version = 0.2.0)
        constructor();

        @since(version = 0.2.0)
        from-list: static func(
            entries: list<tuple<field-name, field-value>>
        ) -> result<fields, header-error>;

        @since(version = 0.2.0)
        get: func(name: field-name) -> list<field-value>;

        @since(version = 0.2.0)
        has: func(name: field-name) -> bool;

        @since(version = 0.2.0)
        set: func(name: field-name, value: list<field-value>) -> result<_, header-error>;

        @since(version = 0.2.0)
        delete: func(name: field-name) -> result<_, header-error>;

        @since(version = 0.2.0)
        append: func(name: field-name, value: field-value) -> result<_, header-error>;

        @since(version = 0.2.0)
        entries: func() -> list<tuple<field-name, field-value>>;

        @since(version = 0.2.0)
        clone: func() -> fields;
    }

    type headers = fields;
    type trailers = fields;
}"#;
        let result = parse(content);
        assert!(result.is_ok(), "Parse failed: {:?}", result.errors);

        if let Item::Interface(iface) = &result.root.items[0] {
            // 2 type aliases + variant + resource + 2 more type aliases
            assert_eq!(iface.items.len(), 6);
        } else {
            panic!("expected interface");
        }
    }

    #[test]
    fn parse_wasi_io_streams_with_borrow() {
        // From wasi-io streams.wit
        let content = r#"package wasi:io@0.2.8;

@since(version = 0.2.0)
interface streams {
    @since(version = 0.2.0)
    use error.{error};
    @since(version = 0.2.0)
    use poll.{pollable};

    @since(version = 0.2.0)
    variant stream-error {
        last-operation-failed(error),
        closed
    }

    @since(version = 0.2.0)
    resource input-stream {
        @since(version = 0.2.0)
        read: func(len: u64) -> result<list<u8>, stream-error>;

        @since(version = 0.2.0)
        blocking-read: func(len: u64) -> result<list<u8>, stream-error>;

        @since(version = 0.2.0)
        skip: func(len: u64) -> result<u64, stream-error>;

        @since(version = 0.2.0)
        subscribe: func() -> pollable;
    }

    @since(version = 0.2.0)
    resource output-stream {
        @since(version = 0.2.0)
        check-write: func() -> result<u64, stream-error>;

        @since(version = 0.2.0)
        write: func(contents: list<u8>) -> result<_, stream-error>;

        @since(version = 0.2.0)
        flush: func() -> result<_, stream-error>;

        @since(version = 0.2.0)
        subscribe: func() -> pollable;

        @since(version = 0.2.0)
        splice: func(src: borrow<input-stream>, len: u64) -> result<u64, stream-error>;

        @since(version = 0.2.0)
        blocking-splice: func(src: borrow<input-stream>, len: u64) -> result<u64, stream-error>;
    }
}"#;
        let result = parse(content);
        assert!(result.is_ok(), "Parse failed: {:?}", result.errors);

        if let Item::Interface(iface) = &result.root.items[0] {
            assert_eq!(iface.name.name.as_ref(), "streams");
            // 2 use + variant + 2 resources
            assert_eq!(iface.items.len(), 5);
        } else {
            panic!("expected interface");
        }
    }

    #[test]
    fn parse_wasi_sockets_tcp_enum() {
        // From wasi-sockets tcp.wit
        let content = r#"package wasi:sockets@0.2.8;

@since(version = 0.2.0)
interface tcp {
    @since(version = 0.2.0)
    use wasi:io/streams@0.2.8.{input-stream, output-stream};
    @since(version = 0.2.0)
    use wasi:io/poll@0.2.8.{pollable};
    @since(version = 0.2.0)
    use wasi:clocks/monotonic-clock@0.2.8.{duration};
    @since(version = 0.2.0)
    use network.{network, error-code, ip-socket-address, ip-address-family};

    @since(version = 0.2.0)
    enum shutdown-type {
        receive,
        send,
        both,
    }

    @since(version = 0.2.0)
    resource tcp-socket {
        @since(version = 0.2.0)
        start-bind: func(network: borrow<network>, local-address: ip-socket-address) -> result<_, error-code>;

        @since(version = 0.2.0)
        finish-bind: func() -> result<_, error-code>;

        @since(version = 0.2.0)
        start-connect: func(network: borrow<network>, remote-address: ip-socket-address) -> result<_, error-code>;

        @since(version = 0.2.0)
        finish-connect: func() -> result<tuple<input-stream, output-stream>, error-code>;

        @since(version = 0.2.0)
        accept: func() -> result<tuple<tcp-socket, input-stream, output-stream>, error-code>;

        @since(version = 0.2.0)
        local-address: func() -> result<ip-socket-address, error-code>;

        @since(version = 0.2.0)
        remote-address: func() -> result<ip-socket-address, error-code>;

        @since(version = 0.2.0)
        is-listening: func() -> bool;

        @since(version = 0.2.0)
        address-family: func() -> ip-address-family;

        @since(version = 0.2.0)
        subscribe: func() -> pollable;

        @since(version = 0.2.0)
        shutdown: func(shutdown-type: shutdown-type) -> result<_, error-code>;
    }
}"#;
        let result = parse(content);
        assert!(result.is_ok(), "Parse failed: {:?}", result.errors);

        if let Item::Interface(iface) = &result.root.items[0] {
            assert_eq!(iface.name.name.as_ref(), "tcp");
            // 4 use + enum + resource
            assert_eq!(iface.items.len(), 6);
        } else {
            panic!("expected interface");
        }
    }

    #[test]
    fn parse_wasi_http_incoming_request_resource() {
        // From wasi-http types.wit
        let content = r#"package wasi:http@0.2.8;

interface types {
    use wasi:io/streams@0.2.8.{input-stream};

    variant method {
        get,
        post,
        other(string)
    }

    variant scheme {
        HTTP,
        HTTPS,
        other(string)
    }

    type headers = u32;

    @since(version = 0.2.0)
    resource incoming-request {
        @since(version = 0.2.0)
        method: func() -> method;

        @since(version = 0.2.0)
        path-with-query: func() -> option<string>;

        @since(version = 0.2.0)
        scheme: func() -> option<scheme>;

        @since(version = 0.2.0)
        authority: func() -> option<string>;

        @since(version = 0.2.0)
        headers: func() -> headers;

        @since(version = 0.2.0)
        consume: func() -> result<incoming-body>;
    }

    resource incoming-body {
        @since(version = 0.2.0)
        %stream: func() -> result<input-stream>;

        @since(version = 0.2.0)
        finish: static func(this: incoming-body) -> future-trailers;
    }

    resource future-trailers {
        @since(version = 0.2.0)
        subscribe: func() -> pollable;

        @since(version = 0.2.0)
        get: func() -> option<result<result<option<trailers>, error-code>>>;
    }

    type pollable = u32;
    type trailers = u32;
    type error-code = u32;
}"#;
        let result = parse(content);
        assert!(result.is_ok(), "Parse failed: {:?}", result.errors);

        if let Item::Interface(iface) = &result.root.items[0] {
            assert_eq!(iface.name.name.as_ref(), "types");
        } else {
            panic!("expected interface");
        }
    }

    #[test]
    fn parse_wasi_http_outgoing_request_constructor() {
        // From wasi-http types.wit
        let content = r#"package wasi:http@0.2.8;

interface types {
    type headers = u32;
    type method = u32;

    @since(version = 0.2.0)
    resource outgoing-request {
        @since(version = 0.2.0)
        constructor(headers: headers);

        @since(version = 0.2.0)
        body: func() -> result<outgoing-body>;

        @since(version = 0.2.0)
        method: func() -> method;

        @since(version = 0.2.0)
        set-method: func(method: method) -> result;

        @since(version = 0.2.0)
        path-with-query: func() -> option<string>;

        @since(version = 0.2.0)
        set-path-with-query: func(path-with-query: option<string>) -> result;
    }

    resource outgoing-body {
        @since(version = 0.2.0)
        write: func() -> result<output-stream>;

        @since(version = 0.2.0)
        finish: static func(this: outgoing-body, trailers: option<trailers>) -> result<_, error-code>;
    }

    type output-stream = u32;
    type trailers = u32;
    type error-code = u32;
}"#;
        let result = parse(content);
        assert!(result.is_ok(), "Parse failed: {:?}", result.errors);

        if let Item::Interface(iface) = &result.root.items[0] {
            assert_eq!(iface.name.name.as_ref(), "types");
        } else {
            panic!("expected interface");
        }
    }

    #[test]
    fn parse_wasi_http_request_options() {
        // From wasi-http types.wit
        let content = r#"package wasi:http@0.2.8;

interface types {
    use wasi:clocks/monotonic-clock@0.2.8.{duration};

    @since(version = 0.2.0)
    resource request-options {
        @since(version = 0.2.0)
        constructor();

        @since(version = 0.2.0)
        connect-timeout: func() -> option<duration>;

        @since(version = 0.2.0)
        set-connect-timeout: func(duration: option<duration>) -> result;

        @since(version = 0.2.0)
        first-byte-timeout: func() -> option<duration>;

        @since(version = 0.2.0)
        set-first-byte-timeout: func(duration: option<duration>) -> result;

        @since(version = 0.2.0)
        between-bytes-timeout: func() -> option<duration>;

        @since(version = 0.2.0)
        set-between-bytes-timeout: func(duration: option<duration>) -> result;
    }
}"#;
        let result = parse(content);
        assert!(result.is_ok(), "Parse failed: {:?}", result.errors);

        if let Item::Interface(iface) = &result.root.items[0] {
            // use + resource
            assert_eq!(iface.items.len(), 2);
        } else {
            panic!("expected interface");
        }
    }

    #[test]
    fn parse_wasi_http_deprecated_type() {
        // From wasi-http types.wit - field-key is deprecated
        let content = r#"package wasi:http@0.2.8;

interface types {
    @since(version = 0.2.1)
    type field-name = field-key;

    /// This type has been deprecated in favor of the `field-name` type.
    @since(version = 0.2.0)
    @deprecated(version = 0.2.2)
    type field-key = string;

    type field-value = list<u8>;
}"#;
        let result = parse(content);
        assert!(result.is_ok(), "Parse failed: {:?}", result.errors);

        if let Item::Interface(iface) = &result.root.items[0] {
            assert_eq!(iface.items.len(), 3);
        } else {
            panic!("expected interface");
        }
    }

    #[test]
    fn parse_wasi_http_response_outparam_unstable() {
        // From wasi-http types.wit - has @unstable
        let content = r#"package wasi:http@0.2.8;

interface types {
    type headers = u32;
    type error-code = u32;

    @since(version = 0.2.0)
    resource response-outparam {
        @unstable(feature = informational-outbound-responses)
        send-informational: func(status: u16, headers: headers) -> result<_, error-code>;

        @since(version = 0.2.0)
        set: static func(
            param: response-outparam,
            response: result<outgoing-response, error-code>,
        );
    }

    type outgoing-response = u32;
}"#;
        let result = parse(content);
        assert!(result.is_ok(), "Parse failed: {:?}", result.errors);

        if let Item::Interface(iface) = &result.root.items[0] {
            assert_eq!(iface.name.name.as_ref(), "types");
        } else {
            panic!("expected interface");
        }
    }

    #[test]
    fn parse_wasi_poll_interface() {
        // From wasi-io poll.wit
        let content = r#"package wasi:io@0.2.8;

/// A poll API intended to let users wait for I/O events on multiple handles at once.
@since(version = 0.2.0)
interface poll {
    /// `pollable` represents a single I/O event which may be ready.
    @since(version = 0.2.0)
    resource pollable {
        /// Return the readiness of a pollable. This function never blocks.
        @since(version = 0.2.0)
        ready: func() -> bool;

        /// `block` returns immediately if the pollable is ready, and otherwise
        /// blocks until ready.
        @since(version = 0.2.0)
        block: func();
    }

    /// Poll for completion on a set of pollables.
    @since(version = 0.2.0)
    poll: func(in: list<borrow<pollable>>) -> list<u32>;
}"#;
        let result = parse(content);
        assert!(result.is_ok(), "Parse failed: {:?}", result.errors);

        if let Item::Interface(iface) = &result.root.items[0] {
            assert_eq!(iface.name.name.as_ref(), "poll");
            // resource + function
            assert_eq!(iface.items.len(), 2);
        } else {
            panic!("expected interface");
        }
    }

    #[test]
    fn parse_complex_result_types() {
        // Testing deeply nested result types from real WASI
        let content = r#"package wasi:http@0.2.8;

interface types {
    type trailers = u32;
    type error-code = u32;

    resource future-trailers {
        get: func() -> option<result<result<option<trailers>, error-code>>>;
    }

    resource future-incoming-response {
        get: func() -> option<result<result<incoming-response, error-code>>>;
    }

    type incoming-response = u32;
}"#;
        let result = parse(content);
        assert!(result.is_ok(), "Parse failed: {:?}", result.errors);

        if let Item::Interface(iface) = &result.root.items[0] {
            assert_eq!(iface.name.name.as_ref(), "types");
        } else {
            panic!("expected interface");
        }
    }

    #[test]
    fn parse_http_error_code_full() {
        // Full error-code variant from wasi-http
        let content = r#"package wasi:http@0.2.8;

interface types {
    @since(version = 0.2.0)
    variant error-code {
        DNS-timeout,
        DNS-error(DNS-error-payload),
        destination-not-found,
        destination-unavailable,
        destination-IP-prohibited,
        destination-IP-unroutable,
        connection-refused,
        connection-terminated,
        connection-timeout,
        connection-read-timeout,
        connection-write-timeout,
        connection-limit-reached,
        TLS-protocol-error,
        TLS-certificate-error,
        TLS-alert-received(TLS-alert-received-payload),
        HTTP-request-denied,
        HTTP-request-length-required,
        HTTP-request-body-size(option<u64>),
        HTTP-request-method-invalid,
        HTTP-request-URI-invalid,
        HTTP-request-URI-too-long,
        HTTP-request-header-section-size(option<u32>),
        HTTP-request-header-size(option<field-size-payload>),
        HTTP-request-trailer-section-size(option<u32>),
        HTTP-request-trailer-size(field-size-payload),
        HTTP-response-incomplete,
        HTTP-response-header-section-size(option<u32>),
        HTTP-response-header-size(field-size-payload),
        HTTP-response-body-size(option<u64>),
        HTTP-response-trailer-section-size(option<u32>),
        HTTP-response-trailer-size(field-size-payload),
        HTTP-response-transfer-coding(option<string>),
        HTTP-response-content-coding(option<string>),
        HTTP-response-timeout,
        HTTP-upgrade-failed,
        HTTP-protocol-error,
        loop-detected,
        configuration-error,
        internal-error(option<string>)
    }

    @since(version = 0.2.0)
    record DNS-error-payload {
        rcode: option<string>,
        info-code: option<u16>
    }

    @since(version = 0.2.0)
    record TLS-alert-received-payload {
        alert-id: option<u8>,
        alert-message: option<string>
    }

    @since(version = 0.2.0)
    record field-size-payload {
        field-name: option<string>,
        field-size: option<u32>
    }
}"#;
        let result = parse(content);
        assert!(result.is_ok(), "Parse failed: {:?}", result.errors);

        if let Item::Interface(iface) = &result.root.items[0] {
            // variant + 3 records
            assert_eq!(iface.items.len(), 4);
        } else {
            panic!("expected interface");
        }
    }
}
