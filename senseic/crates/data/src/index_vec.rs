use std::marker::PhantomData;

use crate::{Span, index::X32};
use allocator_api2::{
    alloc::{Allocator, Global},
    vec::Vec,
};

pub struct IndexVec<IdxMarker, T, A: Allocator> {
    pub raw: Vec<T, A>,
    _index: PhantomData<IdxMarker>,
}

impl<I, T> IndexVec<I, T, Global> {
    pub fn new() -> Self {
        Self::from_raw(Vec::new())
    }
}

impl<I, T, A: Allocator> IndexVec<I, T, A> {
    pub fn with_capacity_in(capacity: usize, alloc: A) -> Self {
        Self::from_raw(Vec::with_capacity_in(capacity, alloc))
    }
    pub fn new_in(alloc: A) -> Self {
        Self::from_raw(Vec::new_in(alloc))
    }

    pub fn from_raw(raw: Vec<T, A>) -> Self {
        Self { raw, _index: PhantomData }
    }
}

impl<I, T: std::fmt::Debug, A: Allocator> std::fmt::Debug for IndexVec<I, T, A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.raw.fmt(f)
    }
}

impl<I, T: Clone, A: Allocator + Clone> Clone for IndexVec<I, T, A> {
    fn clone(&self) -> Self {
        Self { raw: self.raw.clone(), _index: PhantomData }
    }
}

impl<I, T, A: Allocator> std::ops::Deref for IndexVec<I, T, A> {
    type Target = Vec<T, A>;

    fn deref(&self) -> &Self::Target {
        &self.raw
    }
}

impl<I, T, A: Allocator> std::ops::DerefMut for IndexVec<I, T, A> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.raw
    }
}

impl<I, T, A: Allocator> std::ops::Index<X32<I>> for IndexVec<I, T, A> {
    type Output = T;

    fn index(&self, index: X32<I>) -> &Self::Output {
        &self.raw[index.idx()]
    }
}

impl<I, T, A: Allocator> std::ops::IndexMut<X32<I>> for IndexVec<I, T, A> {
    fn index_mut(&mut self, index: X32<I>) -> &mut Self::Output {
        &mut self.raw[index.idx()]
    }
}

impl<I, T, A: Allocator> std::ops::Index<Span<X32<I>>> for IndexVec<I, T, A> {
    type Output = [T];

    fn index(&self, span: Span<X32<I>>) -> &Self::Output {
        &self.raw[span.usize_range()]
    }
}

impl<I, T, A: Allocator> std::ops::IndexMut<Span<X32<I>>> for IndexVec<I, T, A> {
    fn index_mut(&mut self, span: Span<X32<I>>) -> &mut Self::Output {
        &mut self.raw[span.usize_range()]
    }
}
