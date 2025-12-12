use crate::ast;
use std::collections::HashMap;

type TypeId = u32;
type AllocationId = u32;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct StructType {
    def_uuid: ast::StructDefUniqueId,
    fields: Vec<TypeId>,
    defs: Vec<Value>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Void,
    Num,
    Bool,
    MemoryPointer,
    Type,
    Function,
    Struct(StructType),
}

#[derive(Debug, Clone)]
pub struct Types {
    pub id_to_type: Vec<Type>,
    pub type_to_id: HashMap<Type, TypeId>,
}

impl Types {
    const VOID_ID: TypeId = 0;
    const NUM_ID: TypeId = 1;
    const BOOL_ID: TypeId = 2;
    const MEMORY_POINTER_ID: TypeId = 3;
    const TYPE_TYPE_ID: TypeId = 4;
    const FUNCTION_ID: TypeId = 5;

    pub fn new() -> Self {
        let id_to_type = Vec::with_capacity(32);
        let type_to_id = HashMap::with_capacity(32);
        let mut the_new = Self {
            id_to_type,
            type_to_id,
        };

        assert_eq!(the_new.intern_type(Type::Void), Self::VOID_ID);
        assert_eq!(the_new.intern_type(Type::Num), Self::NUM_ID);
        assert_eq!(the_new.intern_type(Type::Bool), Self::BOOL_ID);
        assert_eq!(
            the_new.intern_type(Type::MemoryPointer),
            Self::MEMORY_POINTER_ID
        );
        assert_eq!(the_new.intern_type(Type::Type), Self::TYPE_TYPE_ID);
        assert_eq!(the_new.intern_type(Type::Function), Self::FUNCTION_ID);

        the_new
    }

    pub fn intern_type(&mut self, r#type: Type) -> TypeId {
        *self.type_to_id.entry(r#type).or_insert_with_key(|r#type| {
            let id = self.id_to_type.len() as TypeId;
            self.id_to_type.push(r#type.clone());
            id
        })
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Default)]
pub struct Env {
    pub bindings: Vec<(Box<str>, Value)>,
}

impl Env {
    pub fn enter_new_scope(&self) -> usize {
        self.bindings.len()
    }

    pub fn get(&self, name: &str) -> Option<&Value> {
        self.bindings
            .iter()
            .rev()
            .find_map(|(bind_name, value)| (bind_name as &str == name).then_some(value))
    }

    pub fn push(&mut self, name: &str, value: Value) {
        self.bindings.push((name.into(), value));
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Closure {
    pub r#type: TypeId,
    pub binds: Box<str>,
    pub body: ast::Expr,
    pub captures: Env,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct StructValue {
    pub r#type: TypeId,
    pub field_values: Vec<Value>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct VirtualMemoryPointer {
    pub allocation: AllocationId,
    pub offset: i32,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum ValueKind {
    Void,
    Num(i32),
    Bool(bool),
    Struct(Box<StructValue>),
    MemoryPointer(VirtualMemoryPointer),
    Type(TypeId),
    Closure(Box<Closure>),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Value {
    pub kind: ValueKind,
}
