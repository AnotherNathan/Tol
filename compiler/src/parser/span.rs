use std::ops::Range;

#[derive(Debug, Clone, Eq, Hash)]
pub struct Span {
    pub file_pos: Range<usize>,
    pub line: Range<usize>,
    pub column: Range<usize>,
}

impl PartialEq for Span {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.file_pos == other.file_pos
    }
}
