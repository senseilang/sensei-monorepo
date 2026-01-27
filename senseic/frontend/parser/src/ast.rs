use inturn::Interner;
use neosen_data::X32;

pub struct InternedString;
pub type IStr = X32<InternedString>;
pub type StringInterner = Interner<IStr>;
