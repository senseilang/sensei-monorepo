use std::{hash::Hash, marker::PhantomData, num::NonZero};
pub mod bigint;
use inturn::InternerSymbol;

/// Core crate assumption.
const _USIZE_AT_LEAST_U32: () = const {
    assert!(u32::BITS <= usize::BITS);
};

pub trait IncIterable: Eq + Ord {
    fn get_and_inc(&mut self) -> Self;
}

#[repr(transparent)]
pub struct X32<M> {
    idx: NonZero<u32>,
    _marker: PhantomData<M>,
}

#[cold]
#[inline(never)]
#[cfg_attr(debug_assertions, track_caller)]
const fn panic_x32_overflow() -> ! {
    panic!("Overflowed 32-bits for an X32");
}

impl<M> X32<M> {
    #[inline]
    pub const fn try_new(value: u32) -> Option<Self> {
        match NonZero::new(value.wrapping_add(1)) {
            Some(idx) => Some(Self { idx, _marker: PhantomData }),
            None => None,
        }
    }

    /// Creates a new index by wrapping the `value`.
    ///
    /// # Panics
    /// Panics if `value + 1` overflows 32 bits.
    #[inline]
    pub const fn new(value: u32) -> Self {
        match Self::try_new(value) {
            Some(x) => x,
            None => panic_x32_overflow(),
        }
    }

    /// # Safety
    /// `value + 1` must not overflow 32-bits.
    #[inline(always)]
    pub const unsafe fn new_unchecked(value: u32) -> Self {
        let idx = unsafe {
            let inner = value.unchecked_add(1);
            std::num::NonZero::new_unchecked(inner)
        };
        Self { idx, _marker: PhantomData }
    }

    #[inline(always)]
    pub fn idx(self) -> usize {
        self.get() as usize
    }

    /// Gets the underlying index value.
    #[inline(always)]
    pub fn get(self) -> u32 {
        // Safety: By definition `>= 1`.
        unsafe { self.idx.get().unchecked_sub(1) }
    }
}

impl<M> Hash for X32<M> {
    #[inline]
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.idx.hash(state);
    }
}

impl<M> std::fmt::Debug for X32<M> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}({})", std::any::type_name::<M>(), self.get())
    }
}

impl<M> InternerSymbol for X32<M> {
    fn try_from_usize(id: usize) -> Option<Self> {
        u32::try_from(id).ok().and_then(Self::try_new)
    }

    #[inline]
    fn to_usize(self) -> usize {
        usize::try_from(self.get()).unwrap()
    }
}

impl<M> Clone for X32<M> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<M> Copy for X32<M> {}

impl<M> PartialEq for X32<M> {
    fn eq(&self, other: &Self) -> bool {
        self.idx == other.idx
    }
}

impl<M> Eq for X32<M> {}

impl<M> PartialOrd for X32<M> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<M> Ord for X32<M> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.idx.cmp(&other.idx)
    }
}

impl<M> IncIterable for X32<M> {
    #[inline(always)]
    fn get_and_inc(&mut self) -> Self {
        let current = *self;
        *self = Self::new(self.get() + 1);
        current
    }
}

impl IncIterable for u32 {
    fn get_and_inc(&mut self) -> Self {
        let x = *self;
        *self += 1;
        x
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span<T> {
    pub start: T,
    pub end: T,
}

impl<T> Span<T> {
    pub fn new(start: T, end: T) -> Self {
        Self { start, end }
    }

    pub fn range(self) -> std::ops::Range<T> {
        self.start..self.end
    }
}

impl<T: IncIterable> Span<T> {
    pub fn iter(self) -> IncIterator<T> {
        let Self { start, end } = self;
        IncIterator { start, end }
    }
}

pub trait ToUsize {
    fn to_usize(self) -> usize;
}

impl<M> ToUsize for X32<M> {
    fn to_usize(self) -> usize {
        self.get() as usize
    }
}

impl ToUsize for usize {
    fn to_usize(self) -> usize {
        self
    }
}

impl ToUsize for u32 {
    fn to_usize(self) -> usize {
        self as usize
    }
}

impl ToUsize for u16 {
    fn to_usize(self) -> usize {
        self as usize
    }
}

impl ToUsize for u8 {
    fn to_usize(self) -> usize {
        self as usize
    }
}

impl<T: ToUsize> Span<T> {
    pub fn usize_range(self) -> std::ops::Range<usize> {
        self.start.to_usize()..self.end.to_usize()
    }
}

#[derive(Debug, Clone)]
pub struct IncIterator<T: IncIterable> {
    start: T,
    end: T,
}

impl<T: IncIterable> Iterator for IncIterator<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.start == self.end {
            return None;
        }
        Some(self.start.get_and_inc())
    }
}
