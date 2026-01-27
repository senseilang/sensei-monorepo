use inturn::Interner;
use sensei_core::X32;

pub struct InternedString;
pub type IStr = X32<InternedString>;
pub type StringInterner = Interner<IStr>;
