use std::ops::Range;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Span {
    pub file_pos: Range<usize>,
    pub line: Range<usize>,
    pub column: Range<usize>,
}
