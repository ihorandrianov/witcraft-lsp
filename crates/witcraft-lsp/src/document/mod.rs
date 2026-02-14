mod store;

pub use store::{DocumentStore, SharedDocumentStore};

use witcraft_syntax::{LineIndex, ParseResult, Position, TextRange};

#[derive(Debug)]
#[allow(dead_code)]
pub struct Document {
    pub uri: String,
    pub version: i32,
    pub content: String,
    pub line_index: LineIndex,
    parse: Option<ParseResult>,
}

#[allow(dead_code)]
impl Document {
    pub fn new(uri: String, version: i32, content: String) -> Self {
        let line_index = LineIndex::new(&content);
        Self {
            uri,
            version,
            content,
            line_index,
            parse: None,
        }
    }

    pub fn update(&mut self, version: i32, content: String) {
        self.version = version;
        self.content = content;
        self.line_index = LineIndex::new(&self.content);
        self.parse = None;
    }

    pub fn parse(&mut self) -> &ParseResult {
        self.parse
            .get_or_insert_with(|| witcraft_syntax::parse(&self.content))
    }

    pub fn reparse(&mut self) -> &ParseResult {
        self.parse = Some(witcraft_syntax::parse(&self.content));
        match self.parse.as_ref() {
            Some(parse) => parse,
            None => unreachable!("parse cache must be initialized after reparse"),
        }
    }

    pub fn offset_to_position(&self, offset: u32) -> Position {
        self.line_index.position(offset)
    }

    pub fn position_to_offset(&self, position: Position) -> Option<u32> {
        self.line_index.offset(position)
    }

    pub fn range_to_lsp(&self, range: TextRange) -> tower_lsp::lsp_types::Range {
        let start = self.line_index.position(range.start());
        let end = self.line_index.position(range.end());
        tower_lsp::lsp_types::Range {
            start: tower_lsp::lsp_types::Position {
                line: start.line,
                character: start.column,
            },
            end: tower_lsp::lsp_types::Position {
                line: end.line,
                character: end.column,
            },
        }
    }

    pub fn lsp_to_range(&self, range: tower_lsp::lsp_types::Range) -> Option<TextRange> {
        let start = self
            .line_index
            .offset(Position::new(range.start.line, range.start.character))?;
        let end = self
            .line_index
            .offset(Position::new(range.end.line, range.end.character))?;
        Some(TextRange::new(start, end))
    }

    pub fn text_at(&self, range: TextRange) -> &str {
        range.slice(&self.content)
    }
}
