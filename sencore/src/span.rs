use std::ops::Range;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Span<T> {
    pub start: T,
    pub end: T,
}

impl<T> Span<T> {
    pub fn new(start: T, end: T) -> Self {
        Self { start, end }
    }
}

impl<T: Copy> Span<T> {
    pub fn range(&self) -> Range<T> {
        self.start..self.end
    }
}

impl<T> Into<Range<T>> for Span<T> {
    fn into(self) -> Range<T> {
        self.start..self.end
    }
}

impl<T> From<Range<T>> for Span<T> {
    fn from(value: Range<T>) -> Self {
        Self {
            start: value.start,
            end: value.end,
        }
    }
}

pub trait Stepable: Ord {
    fn strict_succ(&self) -> Self;
}

impl<T: Stepable> IntoIterator for Span<T> {
    type Item = T;
    type IntoIter = SpanIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        SpanIter {
            current: self.start,
            end: self.end,
        }
    }
}

#[derive(Debug, Clone)]
pub struct SpanIter<T> {
    current: T,
    end: T,
}

impl<T: Stepable> Iterator for SpanIter<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current < self.end {
            let next_current = self.current.strict_succ();
            let current = std::mem::replace(&mut self.current, next_current);
            return Some(current);
        }
        None
    }
}
