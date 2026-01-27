pub use index_vec::{
    Idx, IdxRangeBounds, IdxSliceIndex, IndexBox, IndexSlice, IndexVec, index_box, index_vec,
};

/// # Safety
/// Implementing this trait you guarantee that the result of `get` is always in the range
/// `0..u32::MAX`.
pub unsafe trait GudIndex:
    Copy
    + PartialEq
    + Eq
    + PartialOrd
    + Ord
    + core::hash::Hash
    + Idx
    + std::ops::Add<u32, Output = Self>
    + std::ops::Sub<Self, Output = u32>
{
    const ZERO: Self;

    fn get(self) -> u32;

    fn get_and_inc(&mut self) -> Self;

    fn iter_to(self, to: Self) -> impl Iterator<Item = Self>;
}

pub fn iter_idx<I: GudIndex>(r: std::ops::Range<I>) -> impl Iterator<Item = I> {
    r.start.iter_to(r.end)
}

/// Creates a new index to use with [`::index_vec`].
#[macro_export]
macro_rules! newtype_index {
    () => {};
    ($(#[$attr:meta])* $vis:vis struct $name:ident; $($rest:tt)*) => {
        newtype_index!($(#[$attr])* $vis struct $name $vis new; $($rest)*);
    };
    ($(#[$attr:meta])* $vis:vis struct $name:ident $new_vis:vis new; $($rest:tt)*) => {
        $(#[$attr])*
        #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        #[repr(transparent)]
        $vis struct $name(std::num::NonZero<u32>);

        impl std::fmt::Debug for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}({:?})", stringify!($name), self.get())
            }
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.get())
            }
        }

        impl $crate::index::Idx for $name {
            #[inline(always)]
            fn from_usize(value: usize) -> Self {
                let value = u32::try_from(value).expect("index overflowed");
                Self::new(value)
            }

            #[inline(always)]
            fn index(self) -> usize {
                self.get() as usize
            }
        }

        impl $name {
            /// Creates a new `$name` from the given `value`. Panics if `value` is equal to
            /// `u32::MAX`.
            #[inline(always)]
            $new_vis const fn new(value: u32) -> Self {
                assert!(value < u32::MAX, "value too large");
                unsafe {
                    Self::new_unchecked(value)
                }
            }

            /// Creates a new `$name` from the given `value`
            /// # Safety
            /// The passed `value` must be less than `u32::MAX`.
            #[inline(always)]
            $new_vis const unsafe fn new_unchecked(value: u32) -> Self {
                debug_assert!(value < u32::MAX, "safety assumption violated");
                unsafe {
                    let inner_repr = value.unchecked_add(1);
                    Self(std::num::NonZero::new_unchecked(inner_repr))
                }
            }

            /// Gets the underlying index value.
            #[inline(always)]
            $vis const fn get(self) -> u32 {
                self.0.get() - 1
            }

            #[inline(always)]
            $vis fn get_and_inc(&mut self) -> Self {
                let current = *self;
                *self = current + 1;
                current
            }
        }

        unsafe impl $crate::index::GudIndex for $name {
            const ZERO: Self = Self::new(0);

            fn get(self) -> u32 {
                self.get()
            }

            fn get_and_inc(&mut self) -> Self {
                self.get_and_inc()
            }

            fn iter_to(self, to: Self) -> impl Iterator<Item = Self> {
                let index = <Self as $crate::Idx>::index;
                (index(self)..index(to)).map($crate::Idx::from_usize)
            }
        }

        impl std::ops::Add<u32> for $name {
            type Output = Self;
            fn add(self, rhs: u32) -> Self::Output {
                Self::new(self.get().checked_add(rhs).expect("index addition overflowed"))
            }
        }

        impl std::ops::AddAssign<u32> for $name {
            fn add_assign(&mut self, rhs: u32) {
                *self = *self + rhs;
            }
        }

        impl std::ops::Sub<Self> for $name {
            type Output = u32;
            fn sub(self, rhs: Self) -> Self::Output {
                self.get() - rhs.get()
            }
        }

        $crate::newtype_index!($($rest)*);
    };
}

newtype_index! {
    pub struct CasesBasicBlocksIndex;
    pub struct FunctionId;
    pub struct BasicBlockId;
    pub struct OperationIndex;
    pub struct DataId;
    pub struct DataOffset;
    pub struct LocalId;
    pub struct LocalIndex;
    pub struct LargeConstId;
    pub struct CasesId;
    pub struct StaticAllocId;
}

#[derive(Debug, Clone)]
pub struct DenseIndexSet<I: GudIndex> {
    inner: Vec<usize>,
    _idx: std::marker::PhantomData<I>,
}

impl<I: GudIndex> DenseIndexSet<I> {
    pub fn new() -> Self {
        Self::with_capacity(0)
    }

    pub fn with_capacity_in_bits(bits: usize) -> Self {
        let words_capacity = bits.div_ceil(usize::BITS as usize);
        Self::with_capacity(words_capacity)
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self { inner: Vec::with_capacity(capacity), _idx: std::marker::PhantomData }
    }

    pub fn clear(&mut self) {
        self.inner.fill(0);
    }

    pub fn contains(&self, i: I) -> bool {
        let idx = i.get();
        let bit = idx % usize::BITS;
        let word = idx / usize::BITS;
        self.inner.get(word as usize).is_some_and(|word| word & (1 << bit) != 0)
    }

    /// Adds `i` to the set. Returns `true` if it was added or `false` if it was already in the
    /// set.
    pub fn add(&mut self, i: I) -> bool {
        let idx = i.get();
        let bit = idx % usize::BITS;
        let word = (idx / usize::BITS) as usize;

        if word >= self.inner.len() {
            let length_to_fit_word = word.checked_add(1).expect("overflow should be impossible");
            let additional = length_to_fit_word.saturating_sub(self.inner.len());
            self.inner.reserve(additional);
            self.inner.resize(self.inner.capacity(), 0);
        }

        // Safety: Just resized to ensure we have at least `word + 1` elements.
        let word = unsafe { self.inner.get_unchecked_mut(word) };
        let added = *word & (1 << bit) == 0;
        *word |= 1 << bit;
        debug_assert!(self.contains(i), "adding failed");
        added
    }

    /// Removes `i` from the set. Returns `true` if `i` was removed, `false` if it wasn't present
    /// in the set.
    pub fn remove(&mut self, i: I) -> bool {
        let idx = i.get();
        let bit = idx % usize::BITS;
        let word = (idx / usize::BITS) as usize;

        if word >= self.inner.len() {
            debug_assert!(!self.contains(i), "removing failed");
            return false;
        }
        //
        // Safety: Just checked whether `word` was within bounds.
        let word = unsafe { self.inner.get_unchecked_mut(word) };
        let removing = *word & (1 << bit) != 0;
        *word &= !(1 << bit);
        debug_assert!(!self.contains(i), "removing failed");
        removing
    }
}

impl<I: GudIndex> Default for DenseIndexSet<I> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    newtype_index!(
        struct MyIndex;
    );

    #[test]
    fn test_newtype_index() {
        assert_eq!(MyIndex::new(0).get(), 0);
        assert_eq!(MyIndex::new(1).get(), 1);
        assert_eq!(MyIndex::new(0xFFFF_FF00).get(), 0xFFFF_FF00);
    }

    #[test]
    fn test_index_size() {
        assert_eq!(std::mem::size_of::<MyIndex>(), 4);
        assert_eq!(std::mem::size_of::<Option<MyIndex>>(), 4);
        assert_eq!(std::mem::size_of::<BasicBlockId>(), 4);
        assert_eq!(std::mem::size_of::<Option<BasicBlockId>>(), 4);
        assert_eq!(std::mem::size_of::<OperationIndex>(), 4);
        assert_eq!(std::mem::size_of::<Option<OperationIndex>>(), 4);
        assert_eq!(std::mem::size_of::<DataOffset>(), 4);
        assert_eq!(std::mem::size_of::<Option<DataOffset>>(), 4);
        assert_eq!(std::mem::size_of::<LocalId>(), 4);
        assert_eq!(std::mem::size_of::<Option<LocalId>>(), 4);
        assert_eq!(std::mem::size_of::<LocalIndex>(), 4);
    }

    #[test]
    fn test_index_set() {
        let mut set = DenseIndexSet::new();
        assert!(!set.contains(MyIndex::new(0)));
        assert!(!set.contains(MyIndex::new(1)));
        assert!(!set.contains(MyIndex::new(2)));
        assert!(!set.contains(MyIndex::new(3)));

        assert!(set.add(MyIndex::new(2)));

        assert!(!set.contains(MyIndex::new(0)));
        assert!(!set.contains(MyIndex::new(1)));
        assert!(set.contains(MyIndex::new(2)));
        assert!(!set.contains(MyIndex::new(3)));

        assert!(!set.add(MyIndex::new(2)));

        assert!(!set.contains(MyIndex::new(0)));
        assert!(!set.contains(MyIndex::new(1)));
        assert!(set.contains(MyIndex::new(2)));
        assert!(!set.contains(MyIndex::new(3)));

        assert!(!set.contains(MyIndex::new(64)));
        assert!(!set.contains(MyIndex::new(65)));
        assert!(set.add(MyIndex::new(64)));
        assert!(set.add(MyIndex::new(65)));
        assert!(set.contains(MyIndex::new(64)));
        assert!(set.contains(MyIndex::new(65)));

        assert!(!set.contains(MyIndex::new(17)));
        assert!(!set.remove(MyIndex::new(17)));
        assert!(!set.contains(MyIndex::new(17)));

        assert!(set.remove(MyIndex::new(2)));
        assert!(!set.contains(MyIndex::new(2)));
        assert!(!set.remove(MyIndex::new(2)));
        assert!(!set.contains(MyIndex::new(2)));
    }
}
