/// Average characters per line estimate for pre-allocation.
const ESTIMATED_CHARS_PER_LINE: usize = 60;

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
/// LSP uses line/UTF-16 code units, while our lexer/parser uses byte offsets.
/// This provides O(log n) line lookup with per-line conversion.
#[derive(Debug, Clone)]
pub struct LineIndex {
    /// Byte offset where each line starts. First element is always 0.
    line_starts: Vec<u32>,
    /// Total length of the source in bytes.
    len: u32,
    /// Source text for UTF-16 column conversion.
    text: String,
}

impl LineIndex {
    pub fn new(text: &str) -> Self {
        let estimated_lines = text.len() / ESTIMATED_CHARS_PER_LINE + 1;
        let mut line_starts = Vec::with_capacity(estimated_lines);
        line_starts.push(0);

        // SIMD-accelerated newline search
        let bytes = text.as_bytes();
        let mut pos = 0;
        while let Some(idx) = memchr::memchr(b'\n', &bytes[pos..]) {
            let absolute_pos = pos + idx + 1;
            line_starts.push(absolute_pos as u32);
            pos = absolute_pos;
        }

        Self {
            line_starts,
            len: text.len() as u32,
            text: text.to_string(),
        }
    }

    /// Convert a byte offset to a line/UTF-16-column position.
    pub fn position(&self, offset: u32) -> Position {
        let offset = offset.min(self.len);

        let line = self
            .line_starts
            .binary_search(&offset)
            .unwrap_or_else(|next_line| next_line - 1);

        let line_start = self.line_starts[line];
        let column = self.utf16_column(line_start, offset);

        Position::new(line as u32, column)
    }

    /// Convert a line/UTF-16-column position to a byte offset.
    pub fn offset(&self, position: Position) -> Option<u32> {
        let line = position.line as usize;
        let line_start = *self.line_starts.get(line)? as usize;
        let line_end = self
            .line_starts
            .get(line + 1)
            .copied()
            .unwrap_or(self.len) as usize;
        let line_text = self.text.get(line_start..line_end)?;

        let mut utf16_column = 0u32;
        for (byte_idx, ch) in line_text.char_indices() {
            if utf16_column == position.column {
                return Some((line_start + byte_idx) as u32);
            }
            utf16_column += ch.len_utf16() as u32;
            if utf16_column > position.column {
                // Position is in the middle of a UTF-16 surrogate pair.
                return None;
            }
        }

        if utf16_column == position.column {
            Some(line_end as u32)
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

    fn utf16_column(&self, line_start: u32, offset: u32) -> u32 {
        let start = line_start as usize;
        let end = offset.min(self.len) as usize;
        let mut consumed = start;
        let mut column = 0u32;
        let line = self.text.get(start..).unwrap_or("");

        for ch in line.chars() {
            let next = consumed + ch.len_utf8();
            if next > end {
                break;
            }
            consumed = next;
            column += ch.len_utf16() as u32;
            if consumed == end {
                break;
            }
        }

        column
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
    fn line_index_utf16_columns() {
        let text = "a😀b\n";
        let idx = LineIndex::new(text);

        // Byte offsets 0,1,5,6 map to UTF-16 columns 0,1,3,4.
        assert_eq!(idx.position(0), Position::new(0, 0));
        assert_eq!(idx.position(1), Position::new(0, 1));
        assert_eq!(idx.position(5), Position::new(0, 3));
        assert_eq!(idx.position(6), Position::new(0, 4));

        assert_eq!(idx.offset(Position::new(0, 0)), Some(0));
        assert_eq!(idx.offset(Position::new(0, 1)), Some(1));
        assert_eq!(idx.offset(Position::new(0, 3)), Some(5));
        assert_eq!(idx.offset(Position::new(0, 4)), Some(6));
        // Middle of surrogate pair is not a valid byte boundary.
        assert_eq!(idx.offset(Position::new(0, 2)), None);
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
