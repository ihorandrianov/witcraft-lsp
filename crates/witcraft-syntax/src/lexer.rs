use crate::{SyntaxKind, TextRange};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token {
    pub kind: SyntaxKind,
    pub range: TextRange,
}

impl Token {
    pub fn text<'a>(&self, source: &'a str) -> &'a str {
        self.range.slice(source)
    }
}

pub struct Lexer<'a> {
    source: &'a [u8],
    pos: u32,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source: source.as_bytes(),
            pos: 0,
        }
    }

    fn current(&self) -> Option<u8> {
        self.source.get(self.pos as usize).copied()
    }

    fn peek(&self, offset: u32) -> Option<u8> {
        self.source.get((self.pos + offset) as usize).copied()
    }

    fn bump(&mut self) -> Option<u8> {
        let c = self.current()?;
        self.pos += 1;
        Some(c)
    }

    fn token(&self, kind: SyntaxKind, start: u32) -> Token {
        Token {
            kind,
            range: TextRange::new(start, self.pos),
        }
    }

    fn whitespace(&mut self) -> Token {
        let start = self.pos;
        while let Some(c) = self.current() {
            if is_whitespace(c) {
                self.bump();
            } else {
                break;
            }
        }
        self.token(SyntaxKind::Whitespace, start)
    }

    fn line_comment(&mut self) -> Token {
        let start = self.pos;
        self.bump(); // first /
        self.bump(); // second /

        let is_doc = self.current() == Some(b'/');

        while let Some(c) = self.current() {
            if c == b'\n' {
                break;
            }
            self.bump();
        }

        let kind = if is_doc {
            SyntaxKind::DocComment
        } else {
            SyntaxKind::LineComment
        };
        self.token(kind, start)
    }

    fn block_comment(&mut self) -> Token {
        let start = self.pos;
        self.bump(); // /
        self.bump(); // *

        // Track nesting depth for nested block comments
        let mut depth: u32 = 1;

        while depth > 0 {
            match self.current() {
                Some(b'*') if self.peek(1) == Some(b'/') => {
                    self.bump(); // *
                    self.bump(); // /
                    depth -= 1;
                }
                Some(b'/') if self.peek(1) == Some(b'*') => {
                    self.bump(); // /
                    self.bump(); // *
                    depth += 1;
                }
                Some(_) => {
                    self.bump();
                }
                None => break, // unterminated, but don't fail
            }
        }

        self.token(SyntaxKind::BlockComment, start)
    }

    fn ident_or_keyword(&mut self) -> Token {
        let start = self.pos;

        // Handle escaped identifiers (%keyword)
        if self.current() == Some(b'%') {
            self.bump();
        }

        while let Some(c) = self.current() {
            if is_ident_continue(c) {
                self.bump();
            } else {
                break;
            }
        }

        let text = &self.source[start as usize..self.pos as usize];
        let kind = keyword_or_ident(text);
        self.token(kind, start)
    }

    fn integer(&mut self) -> Token {
        let start = self.pos;
        while let Some(c) = self.current() {
            if c.is_ascii_digit() {
                self.bump();
            } else {
                break;
            }
        }
        self.token(SyntaxKind::Integer, start)
    }

    fn single(&mut self, kind: SyntaxKind) -> Token {
        let start = self.pos;
        self.bump();
        self.token(kind, start)
    }

    fn arrow_or_minus(&mut self) -> Token {
        let start = self.pos;
        self.bump(); // -
        if self.current() == Some(b'>') {
            self.bump();
            self.token(SyntaxKind::Arrow, start)
        } else {
            self.token(SyntaxKind::Minus, start)
        }
    }

    fn slash_or_comment(&mut self) -> Token {
        match self.peek(1) {
            Some(b'/') => self.line_comment(),
            Some(b'*') => self.block_comment(),
            _ => self.single(SyntaxKind::Slash),
        }
    }

    fn next_token(&mut self) -> Option<Token> {
        let c = self.current()?;

        let token = match c {
            _ if is_whitespace(c) => self.whitespace(),

            b'{' => self.single(SyntaxKind::LBrace),
            b'}' => self.single(SyntaxKind::RBrace),
            b'(' => self.single(SyntaxKind::LParen),
            b')' => self.single(SyntaxKind::RParen),
            b'<' => self.single(SyntaxKind::LAngle),
            b'>' => self.single(SyntaxKind::RAngle),
            b',' => self.single(SyntaxKind::Comma),
            b':' => self.single(SyntaxKind::Colon),
            b';' => self.single(SyntaxKind::Semicolon),
            b'=' => self.single(SyntaxKind::Eq),
            b'.' => self.single(SyntaxKind::Dot),
            b'@' => self.single(SyntaxKind::At),
            b'*' => self.single(SyntaxKind::Star),

            b'-' => self.arrow_or_minus(),
            b'/' => self.slash_or_comment(),

            b'%' | b'_' | b'a'..=b'z' | b'A'..=b'Z' => self.ident_or_keyword(),
            b'0'..=b'9' => self.integer(),

            // Unknown character - emit error token
            _ => {
                let start = self.pos;
                self.bump();
                self.token(SyntaxKind::Error, start)
            }
        };

        Some(token)
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

fn is_whitespace(c: u8) -> bool {
    matches!(c, b' ' | b'\t' | b'\n' | b'\r')
}

fn is_ident_continue(c: u8) -> bool {
    c.is_ascii_alphanumeric() || c == b'-' || c == b'_'
}

fn keyword_or_ident(text: &[u8]) -> SyntaxKind {
    match text {
        b"package" => SyntaxKind::PackageKw,
        b"world" => SyntaxKind::WorldKw,
        b"interface" => SyntaxKind::InterfaceKw,
        b"import" => SyntaxKind::ImportKw,
        b"export" => SyntaxKind::ExportKw,
        b"include" => SyntaxKind::IncludeKw,
        b"use" => SyntaxKind::UseKw,
        b"as" => SyntaxKind::AsKw,
        b"with" => SyntaxKind::WithKw,
        b"type" => SyntaxKind::TypeKw,
        b"resource" => SyntaxKind::ResourceKw,
        b"func" => SyntaxKind::FuncKw,
        b"record" => SyntaxKind::RecordKw,
        b"flags" => SyntaxKind::FlagsKw,
        b"variant" => SyntaxKind::VariantKw,
        b"enum" => SyntaxKind::EnumKw,
        b"static" => SyntaxKind::StaticKw,
        b"constructor" => SyntaxKind::ConstructorKw,
        b"borrow" => SyntaxKind::BorrowKw,
        b"own" => SyntaxKind::OwnKw,
        b"bool" => SyntaxKind::BoolKw,
        b"u8" => SyntaxKind::U8Kw,
        b"u16" => SyntaxKind::U16Kw,
        b"u32" => SyntaxKind::U32Kw,
        b"u64" => SyntaxKind::U64Kw,
        b"s8" => SyntaxKind::S8Kw,
        b"s16" => SyntaxKind::S16Kw,
        b"s32" => SyntaxKind::S32Kw,
        b"s64" => SyntaxKind::S64Kw,
        b"f32" => SyntaxKind::F32Kw,
        b"f64" => SyntaxKind::F64Kw,
        b"char" => SyntaxKind::CharKw,
        b"string" => SyntaxKind::StringKw,
        b"list" => SyntaxKind::ListKw,
        b"option" => SyntaxKind::OptionKw,
        b"result" => SyntaxKind::ResultKw,
        b"tuple" => SyntaxKind::TupleKw,
        b"future" => SyntaxKind::FutureKw,
        b"stream" => SyntaxKind::StreamKw,
        b"async" => SyntaxKind::AsyncKw,
        b"from" => SyntaxKind::FromKw,
        b"_" => SyntaxKind::Underscore,
        _ => SyntaxKind::Ident,
    }
}

/// Lex the entire source into a vector of tokens.
pub fn lex(source: &str) -> Vec<Token> {
    Lexer::new(source).collect()
}

/// Lex the source, returning only non-trivia tokens.
/// Useful for parsing where whitespace/comments are not needed.
pub fn lex_non_trivia(source: &str) -> Vec<Token> {
    Lexer::new(source).filter(|t| !t.kind.is_trivia()).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn token_kinds(source: &str) -> Vec<SyntaxKind> {
        lex(source).into_iter().map(|t| t.kind).collect()
    }

    #[test]
    fn lex_empty() {
        assert_eq!(token_kinds(""), vec![]);
    }

    #[test]
    fn lex_whitespace() {
        assert_eq!(token_kinds("   "), vec![SyntaxKind::Whitespace]);
        assert_eq!(token_kinds("\n\t"), vec![SyntaxKind::Whitespace]);
    }

    #[test]
    fn lex_keywords() {
        assert_eq!(
            token_kinds("package interface world"),
            vec![
                SyntaxKind::PackageKw,
                SyntaxKind::Whitespace,
                SyntaxKind::InterfaceKw,
                SyntaxKind::Whitespace,
                SyntaxKind::WorldKw,
            ]
        );
    }

    #[test]
    fn lex_interface() {
        assert_eq!(
            token_kinds("interface foo {}"),
            vec![
                SyntaxKind::InterfaceKw,
                SyntaxKind::Whitespace,
                SyntaxKind::Ident,
                SyntaxKind::Whitespace,
                SyntaxKind::LBrace,
                SyntaxKind::RBrace,
            ]
        );
    }

    #[test]
    fn lex_func_signature() {
        assert_eq!(
            token_kinds("get-user: func(id: u64) -> string"),
            vec![
                SyntaxKind::Ident,
                SyntaxKind::Colon,
                SyntaxKind::Whitespace,
                SyntaxKind::FuncKw,
                SyntaxKind::LParen,
                SyntaxKind::Ident,
                SyntaxKind::Colon,
                SyntaxKind::Whitespace,
                SyntaxKind::U64Kw,
                SyntaxKind::RParen,
                SyntaxKind::Whitespace,
                SyntaxKind::Arrow,
                SyntaxKind::Whitespace,
                SyntaxKind::StringKw,
            ]
        );
    }

    #[test]
    fn lex_package_decl() {
        assert_eq!(
            token_kinds("package example:types@0.1.0;"),
            vec![
                SyntaxKind::PackageKw,
                SyntaxKind::Whitespace,
                SyntaxKind::Ident,
                SyntaxKind::Colon,
                SyntaxKind::Ident,
                SyntaxKind::At,
                SyntaxKind::Integer,
                SyntaxKind::Dot,
                SyntaxKind::Integer,
                SyntaxKind::Dot,
                SyntaxKind::Integer,
                SyntaxKind::Semicolon,
            ]
        );
    }

    #[test]
    fn lex_doc_comment() {
        assert_eq!(
            token_kinds("/// This is a doc comment"),
            vec![SyntaxKind::DocComment]
        );
    }

    #[test]
    fn lex_line_comment() {
        assert_eq!(
            token_kinds("// This is a comment"),
            vec![SyntaxKind::LineComment]
        );
    }

    #[test]
    fn lex_block_comment() {
        assert_eq!(token_kinds("/* block */"), vec![SyntaxKind::BlockComment]);
    }

    #[test]
    fn lex_generic_types() {
        assert_eq!(
            token_kinds("list<u8>"),
            vec![
                SyntaxKind::ListKw,
                SyntaxKind::LAngle,
                SyntaxKind::U8Kw,
                SyntaxKind::RAngle,
            ]
        );

        assert_eq!(
            token_kinds("result<string, error>"),
            vec![
                SyntaxKind::ResultKw,
                SyntaxKind::LAngle,
                SyntaxKind::StringKw,
                SyntaxKind::Comma,
                SyntaxKind::Whitespace,
                SyntaxKind::Ident,
                SyntaxKind::RAngle,
            ]
        );
    }

    #[test]
    fn lex_resource() {
        assert_eq!(
            token_kinds("resource connection { constructor(url: string); }"),
            vec![
                SyntaxKind::ResourceKw,
                SyntaxKind::Whitespace,
                SyntaxKind::Ident,
                SyntaxKind::Whitespace,
                SyntaxKind::LBrace,
                SyntaxKind::Whitespace,
                SyntaxKind::ConstructorKw,
                SyntaxKind::LParen,
                SyntaxKind::Ident,
                SyntaxKind::Colon,
                SyntaxKind::Whitespace,
                SyntaxKind::StringKw,
                SyntaxKind::RParen,
                SyntaxKind::Semicolon,
                SyntaxKind::Whitespace,
                SyntaxKind::RBrace,
            ]
        );
    }

    #[test]
    fn lex_hyphenated_ident() {
        assert_eq!(token_kinds("get-user-by-id"), vec![SyntaxKind::Ident]);
    }

    #[test]
    fn lex_escaped_ident() {
        assert_eq!(token_kinds("%type"), vec![SyntaxKind::Ident]);
    }

    #[test]
    fn lex_handles() {
        assert_eq!(
            token_kinds("borrow<connection>"),
            vec![
                SyntaxKind::BorrowKw,
                SyntaxKind::LAngle,
                SyntaxKind::Ident,
                SyntaxKind::RAngle,
            ]
        );
    }

    #[test]
    fn lex_preserves_ranges() {
        let tokens = lex("interface foo");
        assert_eq!(tokens[0].range, TextRange::new(0, 9)); // "interface"
        assert_eq!(tokens[1].range, TextRange::new(9, 10)); // " "
        assert_eq!(tokens[2].range, TextRange::new(10, 13)); // "foo"
    }

    #[test]
    fn lex_underscore() {
        assert_eq!(
            token_kinds("result<_, error>"),
            vec![
                SyntaxKind::ResultKw,
                SyntaxKind::LAngle,
                SyntaxKind::Underscore,
                SyntaxKind::Comma,
                SyntaxKind::Whitespace,
                SyntaxKind::Ident,
                SyntaxKind::RAngle,
            ]
        );
    }

    #[test]
    fn lex_error_recovery() {
        let tokens = token_kinds("interface § foo");
        assert!(tokens.contains(&SyntaxKind::Error));
        assert!(tokens.contains(&SyntaxKind::InterfaceKw));
        assert!(tokens.contains(&SyntaxKind::Ident));
    }

    #[test]
    fn lex_real_wit_file() {
        let source = r#"package example:types@0.1.0;

/// Basic type definitions for the example package.
interface types {
    /// A simple record representing a user.
    record user {
        id: u64,
        name: string,
        email: option<string>,
    }

    type user-list = list<user>;
    type operation-result = result<user, error>;
}"#;
        let tokens = lex(source);
        let errors: Vec<_> = tokens
            .iter()
            .filter(|t| t.kind == SyntaxKind::Error)
            .collect();
        assert!(errors.is_empty(), "unexpected errors: {:?}", errors);

        let kinds = token_kinds(source);
        assert!(kinds.contains(&SyntaxKind::PackageKw));
        assert!(kinds.contains(&SyntaxKind::InterfaceKw));
        assert!(kinds.contains(&SyntaxKind::RecordKw));
        assert!(kinds.contains(&SyntaxKind::DocComment));
        assert!(kinds.contains(&SyntaxKind::ListKw));
        assert!(kinds.contains(&SyntaxKind::ResultKw));
    }

    #[test]
    fn lex_non_trivia_filters() {
        let source = "interface foo { }";
        let all = lex(source);
        let non_trivia = lex_non_trivia(source);

        assert!(all.len() > non_trivia.len());
        assert!(non_trivia.iter().all(|t| !t.kind.is_trivia()));
    }

    #[test]
    fn token_text_extraction() {
        let source = "interface foo";
        let tokens = lex(source);

        assert_eq!(tokens[0].text(source), "interface");
        assert_eq!(tokens[1].text(source), " ");
        assert_eq!(tokens[2].text(source), "foo");
    }

    #[test]
    fn lex_section_sign() {
        println!("Testing § (section sign, UTF-8: 0xC2 0xA7)");
        let tokens = lex("§");
        println!("Tokens: {:?}", tokens);
        assert!(!tokens.is_empty());
    }

    #[test]
    fn lex_nested_block_comment() {
        // Simple nested block comment
        let source = "/* outer /* inner */ still comment */";
        let tokens = lex(source);
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, SyntaxKind::BlockComment);
        assert_eq!(tokens[0].text(source), source);
    }

    #[test]
    fn lex_deeply_nested_block_comment() {
        // Three levels of nesting
        let source = "/* a /* b /* c */ b */ a */";
        let tokens = lex(source);
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, SyntaxKind::BlockComment);
        assert_eq!(tokens[0].text(source), source);
    }

    #[test]
    fn lex_nested_block_comment_in_code() {
        let source = "interface /* outer /* inner */ end */ foo";
        let kinds = token_kinds(source);
        assert_eq!(
            kinds,
            vec![
                SyntaxKind::InterfaceKw,
                SyntaxKind::Whitespace,
                SyntaxKind::BlockComment,
                SyntaxKind::Whitespace,
                SyntaxKind::Ident,
            ]
        );
    }

    #[test]
    fn lex_nested_block_comment_unterminated() {
        // Missing one closing */
        let source = "/* outer /* inner */ not closed";
        let tokens = lex(source);
        // Should still produce a token (unterminated but handled gracefully)
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, SyntaxKind::BlockComment);
    }

    #[test]
    fn lex_block_comment_simple_still_works() {
        // Ensure simple block comments still work
        let source = "/* simple comment */";
        let tokens = lex(source);
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, SyntaxKind::BlockComment);
        assert_eq!(tokens[0].text(source), source);
    }
}
