/// A range in the source text, represented as byte offsets.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct TextRange {
    start: u32,
    end: u32,
}

impl TextRange {
    pub const fn new(start: u32, end: u32) -> Self {
        debug_assert!(start <= end);
        Self { start, end }
    }

    pub const fn empty(offset: u32) -> Self {
        Self {
            start: offset,
            end: offset,
        }
    }

    pub const fn start(self) -> u32 {
        self.start
    }

    pub const fn end(self) -> u32 {
        self.end
    }

    pub const fn len(self) -> u32 {
        self.end - self.start
    }

    pub const fn is_empty(self) -> bool {
        self.start == self.end
    }

    pub const fn contains(self, offset: u32) -> bool {
        self.start <= offset && offset < self.end
    }

    pub const fn contains_inclusive(self, offset: u32) -> bool {
        self.start <= offset && offset <= self.end
    }

    pub fn slice<'a>(&self, text: &'a str) -> &'a str {
        &text[self.start as usize..self.end as usize]
    }
}

/// A position in the source text as line and column (both 0-indexed).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Position {
    pub line: u32,
    pub column: u32,
}

impl Position {
    pub const fn new(line: u32, column: u32) -> Self {
        Self { line, column }
    }
}

/// Index for converting between byte offsets and line/column positions.
///
/// LSP uses line/column, but our lexer/parser uses byte offsets.
/// This provides O(log n) conversion in both directions.
#[derive(Debug, Clone)]
pub struct LineIndex {
    /// Byte offset where each line starts. First element is always 0.
    line_starts: Vec<u32>,
    /// Total length of the source in bytes.
    len: u32,
}

impl LineIndex {
    pub fn new(text: &str) -> Self {
        let mut line_starts = vec![0];

        for (i, c) in text.char_indices() {
            if c == '\n' {
                line_starts.push((i + 1) as u32);
            }
        }

        Self {
            line_starts,
            len: text.len() as u32,
        }
    }

    /// Convert a byte offset to a line/column position.
    pub fn position(&self, offset: u32) -> Position {
        let offset = offset.min(self.len);

        let line = self
            .line_starts
            .binary_search(&offset)
            .unwrap_or_else(|next_line| next_line - 1);

        let line_start = self.line_starts[line];
        let column = offset - line_start;

        Position::new(line as u32, column)
    }

    /// Convert a line/column position to a byte offset.
    pub fn offset(&self, position: Position) -> Option<u32> {
        let line_start = *self.line_starts.get(position.line as usize)?;
        let offset = line_start + position.column;

        if offset <= self.len {
            Some(offset)
        } else {
            None
        }
    }

    /// Get the byte range for a given line (0-indexed).
    pub fn line_range(&self, line: u32) -> Option<TextRange> {
        let start = *self.line_starts.get(line as usize)?;
        let end = self
            .line_starts
            .get(line as usize + 1)
            .copied()
            .unwrap_or(self.len);

        Some(TextRange::new(start, end))
    }

    /// Number of lines in the source.
    pub fn line_count(&self) -> u32 {
        self.line_starts.len() as u32
    }

    /// Total length of the source in bytes.
    pub fn len(&self) -> u32 {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// Convert a TextRange to start/end positions.
    pub fn range_positions(&self, range: TextRange) -> (Position, Position) {
        (self.position(range.start()), self.position(range.end()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn line_index_empty() {
        let idx = LineIndex::new("");
        assert_eq!(idx.line_count(), 1);
        assert_eq!(idx.position(0), Position::new(0, 0));
    }

    #[test]
    fn line_index_single_line() {
        let idx = LineIndex::new("hello");
        assert_eq!(idx.line_count(), 1);
        assert_eq!(idx.position(0), Position::new(0, 0));
        assert_eq!(idx.position(3), Position::new(0, 3));
        assert_eq!(idx.position(5), Position::new(0, 5));
    }

    #[test]
    fn line_index_multiple_lines() {
        let idx = LineIndex::new("hello\nworld\n");
        assert_eq!(idx.line_count(), 3);

        assert_eq!(idx.position(0), Position::new(0, 0));
        assert_eq!(idx.position(5), Position::new(0, 5)); // newline char
        assert_eq!(idx.position(6), Position::new(1, 0)); // start of "world"
        assert_eq!(idx.position(11), Position::new(1, 5)); // newline char
        assert_eq!(idx.position(12), Position::new(2, 0)); // after final newline
    }

    #[test]
    fn line_index_offset_roundtrip() {
        let text = "line one\nline two\nline three";
        let idx = LineIndex::new(text);

        for offset in 0..text.len() as u32 {
            let pos = idx.position(offset);
            let back = idx.offset(pos).unwrap();
            assert_eq!(offset, back, "roundtrip failed for offset {offset}");
        }
    }

    #[test]
    fn line_index_line_range() {
        let idx = LineIndex::new("aaa\nbbb\nccc");

        assert_eq!(idx.line_range(0), Some(TextRange::new(0, 4)));
        assert_eq!(idx.line_range(1), Some(TextRange::new(4, 8)));
        assert_eq!(idx.line_range(2), Some(TextRange::new(8, 11)));
        assert_eq!(idx.line_range(3), None);
    }

    #[test]
    fn text_range_slice() {
        let text = "hello world";
        let range = TextRange::new(6, 11);
        assert_eq!(range.slice(text), "world");
    }

    #[test]
    fn text_range_contains() {
        let range = TextRange::new(5, 10);
        assert!(!range.contains(4));
        assert!(range.contains(5));
        assert!(range.contains(9));
        assert!(!range.contains(10)); // exclusive end
        assert!(range.contains_inclusive(10)); // inclusive version
    }
}
