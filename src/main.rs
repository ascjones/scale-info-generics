use std::any;

mod meta_type {
    use super::*;
    use super::TypeInfo;

    pub enum MetaType {
        Parameter(MetaTypeParameter),
        Concrete(MetaTypeConcrete),
        Generic(MetaTypeGeneric),
    }

    impl MetaType
    {
        pub fn of<T>() -> Self
        where
            T: 'static + ?Sized + TypeInfo
        {
            MetaType::Concrete(MetaTypeConcrete {
                id: any::TypeId::of::<T>(),
                path: T::path(),
            })
        }

        pub fn parameter<T>(name: &'static str, parent: MetaType) -> Self
        where
            T: 'static + ?Sized + TypeInfo
        {
            todo!()
        }

        pub fn parameterized<T>(params: Vec<MetaType>) -> Self
        where
            T: 'static + ?Sized + TypeInfo
        {
            todo!()
        }
    }

    pub struct MetaTypeConcrete {
        id: any::TypeId,
        path: &'static str,
    }

    pub struct MetaTypeParameter {
        name: &'static str,
        parent: MetaTypeConcrete,
        instance_id: any::TypeId,
    }

    pub struct MetaTypeGeneric {

    }
}

mod form {
    use super::*;

    pub trait Form {
        type Type;
    }

    pub enum MetaForm {}

    impl Form for MetaForm {
        type Type = MetaType;
    }

    pub enum CompactForm {}

    impl Form for CompactForm {
        type Type = u32;
    }
}

mod registry {
    use std::collections::btree_map::{BTreeMap, Entry};
    use super::*;
    use super::form::*;
    use super::meta_type::*;
    use super::Type;

    pub trait IntoCompact {
        type Output;

        fn into_compact(self, registry: &mut Registry) -> Self::Output;
    }

    #[derive(Debug, PartialEq, Eq, Ord, PartialOrd, Clone)]
    pub enum TypeId {
        Any(any::TypeId),
        Path(&'static str),
        Parameter(TypeParameter),
    }

    pub enum RegistryType<F: Form = CompactForm> {
        Definition(Type<F>),
        Parameter(TypeParameter)
    }

    #[derive(Debug, PartialEq, Eq, Ord, PartialOrd, Clone)]
    pub struct TypeParameter {
        path: &'static str,
        parent: <CompactForm as Form>::Type,
    }

    #[derive(Default)]
    pub struct Registry {
        type_table: BTreeMap<TypeId, usize>,
        type_ids: Vec<TypeId>,
        types: BTreeMap<<CompactForm as Form>::Type, RegistryType<CompactForm>>,
    }

    impl Registry {
        fn intern_type<F, T>(&mut self, type_id: TypeId, f: F) -> <CompactForm as Form>::Type
            where
                F: FnOnce () -> T,
                T: Into<RegistryType>,
        {
            let next_id = self.type_ids.len();
            let (inserted, sym_id) = match self.type_table.entry(type_id.clone()) {
                Entry::Vacant(vacant) => {
                    vacant.insert(next_id);
                    self.type_ids.push(type_id);
                    (true, next_id)
                }
                Entry::Occupied(occupied) => (false, *occupied.get()),
            };
            let symbol = (sym_id + 1) as u32;
            if inserted {
                let registry_type = f().into();
                // let compact_type = registry_type.into_compact(self);
                self.types.insert(symbol.clone(), registry_type);
            }
            symbol
        }

        pub fn register_type(&mut self, ty: &MetaType) -> <CompactForm as Form>::Type {
            match ty {
                MetaType::Concrete(ty) => {
                    todo!()
                },
                MetaType::Parameter(p) => {
                    todo!()
                }
                MetaType::Generic(g) => {
                    todo!()
                }
            }
        }
    }
}

use meta_type::*;
use form::*;
use registry::*;

pub enum Type<F: Form = MetaForm> {
    Primitive(Primitive),
    Struct(Struct<F>),
}

impl IntoCompact for Type<MetaForm> {
    type Output = Type<CompactForm>;

    fn into_compact(self, registry: &mut Registry) -> Self::Output {
        todo!()
    }
}

pub struct Struct<F: Form = MetaForm> {
    fields: Vec<F::Type>,
}

pub enum Primitive {
    Bool,
    U32,
}

pub trait TypeInfo {
    fn path() -> &'static str;
    fn params() -> Vec<MetaType> {
        Vec::new()
    }
    fn type_info() -> Type;
}

impl TypeInfo for bool {
    fn path() -> &'static str {
        ""
    }

    fn type_info() -> Type {
        Type::Primitive(Primitive::Bool)
    }
}

#[allow(unused)]
struct A<T> {
    a: B<T, bool>,
    b: B<B<T, T>, bool>,
}

impl<T> TypeInfo for A<T> where T: TypeInfo + 'static
{
    fn path() -> &'static str {
        "A"
    }

    fn type_info() -> Type {
        Type::Struct (Struct {
            fields: vec! [
                MetaType::parameterized::<B<T, bool>>(
                    vec![
                        MetaType::parameter::<Self>("T", MetaType::of::<T>()),
                        MetaType::of::<bool>(),
                    ]
                ),
                MetaType::of::<B<B<T, T>, bool>>(),
            ]
        })
    }
}

#[allow(unused)]
struct B<T, U> {
    a: T,
    b: U,
}

impl<T, U> TypeInfo for B<T, U>
where
    T: TypeInfo + 'static,
    U: TypeInfo + 'static,
{
    fn path() -> &'static str {
        "B"
    }

    fn type_info() -> Type {
        Type::Struct (Struct {
            fields: vec! [
                MetaType::parameter::<Self>("T", MetaType::of::<T>()),
                MetaType::parameter::<Self>("U", MetaType::of::<U>()),
            ]
        })
    }
}

fn main() {
    let mut registry = Registry::default();
}
