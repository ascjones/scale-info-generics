use std::any;

mod meta_type {
    use super::TypeInfo;

    pub enum MetaType {
        Parameter(MetaTypeParameter),
        Concrete(MetaTypeConcrete),
        Generic(MetaTypeGeneric),
    }

    impl MetaType
        where
            T: 'static + ?Sized + TypeInfo
    {
        fn of<T>() -> Self {
            MetaType::Concrete(MetaTypeConcrete {
                id: any::TypeId::of::<T>(),
                path: T::path(),
            })
        }
    }

    struct MetaTypeConcrete {
        id: any::TypeId,
        path: &'static str,
    }

    struct MetaTypeParameter {
        name: &'static str,
        parent: MetaTypeConcrete,
        instance_id: any::TypeId,
    }

    struct MetaTypeGeneric {

    }
}

mod form {
    pub trait Form {
        type Type;
    }

    pub enum MetaForm {}

    impl Form for MetaFprm {
        type Type = MetaType;
    }

    pub enum CompactForm {}

    impl Form for CompactForm {
        type Type = u32;
    }
}

mod registry {
    use std::collections::{BTreeMap, HashMap};
    use super::form::*;
    use super::meta_type::*;
    use super::Type;

    pub trait IntoCompact {
        type Output;

        fn into_compact(self, registry: &mut Registry) -> Self::Output;
    }

    #[derive(Debug, PartialEq, Eq, Clone)]
    pub enum TypeId {
        Any(any::TypeId),
        Path(&'static str),
        Parameter(TypeParameter),
    }

    pub enum RegistryType<F: Form = MetaForm> {
        Definition(Type<F>),
        Parameter(TypeParameter)
    }

    #[derive(Debug, PartialEq, Eq, Clone)]
    pub struct TypeParameter {
        path: &'static str,
        parent: <CompactForm as Form>::Type,
    }

    pub struct Registry {
        type_table: BTreeMap<TypeId, <CompactForm as Form>::Type>,
        type_ids: Vec<TypeId>,
        types: BTreeMap<<CompactForm as Form>::Type, Type<CompactForm>>,
    }

    impl Registry {
        fn intern_type<F, T>(&mut self, ty: &MetaType) -> <CompactForm as Form>::Type
            where
                F: FnOnce () -> T,
                T: Into<RegistryType>,
        {
            let next_id = self.type_ids.len();
            let (inserted, sym_id) = match self.type_table.entry(s.clone()) {
                Entry::Vacant(vacant) => {
                    vacant.insert(next_id);
                    self.type_ids.push(s);
                    (true, next_id)
                }
                Entry::Occupied(occupied) => (false, *occupied.get()),
            };
            let symbol = (sym_id + 1) as u32;
            if inserted {
                let registry_type = f().into();
                let compact_type = registry_type.into_compact(self);
                self.types.insert(symbol.clone(), compact_type);
            }
            symbol
        }

        pub fn register_type(&mut self, ty: &MetaType) -> <CompactForm as Form>::Type {
            match ty {
                MetaType::Concrete(ty) => {

                },
                MetaType::Parameter(p) => {

                }
                MetaType::Generic(g) => {

                }
            }
        }
    }
}

enum Type<F: Form = MetaForm> {
    Primitive(Primitive<F>),
    Struct(Struct<F>),
}

struct Struct<F: Form = MetaForm> {
    fields: Vec<F::Type>,
}

enum Primitive<F: Form = Metaform> {
    Bool,
    U32,
}

trait TypeInfo {
    fn path() -> &'static str;
    fn params() -> Vec<MetaTypeParameter> {
        Vec::new()
    }
    fn type_info() -> Type;
}

struct A<T> {
    a: B<T, bool>,
    b: B<B<T, T>, bool>,
}

impl<T> TypeInfo for A<T> where T: 'static + ?Sized
{
    fn path() -> &'static str {
        "A"
    }

    fn type_info() -> Type {
        Type::Struct (Struct {
            fields: vec! [
                MetaType::parameterized::B<T, bool>>(
                    vec![
                        MetaType::parameter::<Self>("T", MetaType::new::<T>()),
                        MetaType::new::<bool>(),
                    ]
                ),
                MetaType::of::<B<B<T, T>, bool>>(),
            ]
        })
    }
}

struct B<T, U> {
    a: T,
    b: U,
}

fn main() {

}
