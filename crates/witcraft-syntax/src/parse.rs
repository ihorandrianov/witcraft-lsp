use crate::{SyntaxKind, TextRange, Token, ast::SourceFile};

/// Result of parsing a WIT file.
///
/// Always contains a (possibly incomplete) AST, even if there were errors.
/// This is critical for LSP - we always need something to work with.
#[derive(Debug, Clone)]
pub struct ParseResult {
    /// The parsed AST (may be partial if errors occurred).
    pub root: SourceFile,
    /// Parse errors encountered.
    pub errors: Vec<ParseError>,
    /// All tokens from lexing (including trivia).
    /// Useful for semantic tokens, formatting, etc.
    pub tokens: Vec<Token>,
}

impl ParseResult {
    /// Returns true if parsing completed without errors.
    pub fn is_ok(&self) -> bool {
        self.errors.is_empty()
    }

    /// Returns true if there were parse errors.
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }
}

/// A parse error with location and message.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    /// Human-readable error message.
    pub message: String,
    /// Location of the error in source.
    pub range: TextRange,
    /// Category of error for filtering/grouping.
    pub kind: ErrorKind,
}

impl ParseError {
    pub fn new(message: impl Into<String>, range: TextRange, kind: ErrorKind) -> Self {
        Self {
            message: message.into(),
            range,
            kind,
        }
    }

    pub fn unexpected_token(found: SyntaxKind, range: TextRange) -> Self {
        Self::new(
            format!("unexpected token: {:?}", found),
            range,
            ErrorKind::UnexpectedToken,
        )
    }

    pub fn expected_token(expected: SyntaxKind, range: TextRange) -> Self {
        Self::new(
            format!("expected {:?}", expected),
            range,
            ErrorKind::ExpectedToken(expected),
        )
    }

    pub fn expected_ident(range: TextRange) -> Self {
        Self::new("expected identifier", range, ErrorKind::ExpectedIdent)
    }

    pub fn expected_type(range: TextRange) -> Self {
        Self::new("expected type", range, ErrorKind::ExpectedType)
    }

    pub fn unclosed(what: &str, opened_at: TextRange, range: TextRange) -> Self {
        Self::new(
            format!("unclosed {} (opened at byte {})", what, opened_at.start()),
            range,
            ErrorKind::Unclosed { opened_at },
        )
    }

    pub fn expected_in_context(
        expected: &str,
        found: &str,
        context: &str,
        range: TextRange,
    ) -> Self {
        Self::new(
            format!("expected {} in {}, found {}", expected, context, found),
            range,
            ErrorKind::Other,
        )
    }
}

/// Categories of parse errors.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ErrorKind {
    /// Found a token that doesn't belong here.
    UnexpectedToken,
    /// Expected a specific token.
    ExpectedToken(SyntaxKind),
    /// Expected an identifier.
    ExpectedIdent,
    /// Expected a type.
    ExpectedType,
    /// Unclosed delimiter (brace, paren, angle bracket).
    Unclosed { opened_at: TextRange },
    /// Unexpected end of file.
    UnexpectedEof,
    /// Other error.
    Other,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn error_creation() {
        let err = ParseError::expected_token(SyntaxKind::LBrace, TextRange::new(10, 11));
        assert!(err.message.contains("LBrace"));
        assert_eq!(err.range, TextRange::new(10, 11));
    }
}
