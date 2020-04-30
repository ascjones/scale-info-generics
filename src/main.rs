use std::any;

mod meta_type {
    use super::*;
    use super::TypeInfo;
    use std::cmp::Ordering;

    #[derive(Clone, Eq, PartialEq, PartialOrd, Ord, Debug)]
    pub enum MetaType {
        Parameter(MetaTypeParameter),
        Concrete(MetaTypeConcrete),
        Parameterized(MetaTypeParameterized),
        Generic(MetaTypeGeneric),
    }

    impl MetaType
    {
        pub fn of<T>() -> Self
        where
            T: 'static + ?Sized + TypeInfo
        {
            MetaType::Concrete(MetaTypeConcrete::new::<T>())
        }

        pub fn parameter<T, P>(name: &'static str) -> Self
        where
            T: 'static + ?Sized + TypeInfo,
            P: 'static + ?Sized + TypeInfo,
        {
            MetaType::Parameter(MetaTypeParameter {
                name,
                parent: MetaTypeGeneric::new::<T>(),
                instance_id: any::TypeId::of::<P>(),
            })
        }

        pub fn parameterized<T>(params: Vec<MetaTypeParameterValue>) -> Self
        where
            T: 'static + ?Sized + TypeInfo
        {
            MetaType::Parameterized(MetaTypeParameterized {
                concrete: MetaTypeConcrete::new::<T>(),
                params,
            })
        }

        // pub fn concrete_type_id(&self) -> any::TypeId {
        //     match self {
        //         MetaType::Parameter(p) => {
        //             TypeId::Parameter()
        //         }
        //     }
        // }
    }

    #[derive(Clone, Debug)]
    pub struct MetaTypeConcrete {
        pub type_id: any::TypeId,
        pub fn_type_info: fn() -> Type,
        pub path: &'static str,
        pub params: Vec<MetaTypeConcrete>,
    }

    impl PartialEq for MetaTypeConcrete {
        fn eq(&self, other: &Self) -> bool {
            self.type_id == other.type_id
        }
    }

    impl Eq for MetaTypeConcrete {}

    impl PartialOrd for MetaTypeConcrete {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            self.type_id.partial_cmp(&other.type_id)
        }
    }

    impl Ord for MetaTypeConcrete {
        fn cmp(&self, other: &Self) -> Ordering {
            self.type_id.cmp(&other.type_id)
        }
    }

    impl MetaTypeConcrete {
        pub fn new<T>() -> Self
        where
            T: 'static + ?Sized + TypeInfo
        {
            Self {
                type_id: any::TypeId::of::<T>(),
                fn_type_info: T::type_info,
                path: T::path(),
                params: T::params(),
            }
        }
    }

    #[derive(Clone, Eq, PartialEq, PartialOrd, Ord, Debug)]
    pub struct MetaTypeParameter {
        pub name: &'static str,
        pub parent: MetaTypeGeneric,
        pub instance_id: any::TypeId,
    }

    #[derive(Clone, Eq, PartialEq, PartialOrd, Ord, Debug)]
    pub struct MetaTypeParameterized {
        pub concrete: MetaTypeConcrete,
        pub params: Vec<MetaTypeParameterValue>,
    }

    #[derive(Clone, Eq, PartialEq, PartialOrd, Ord, Debug)]
    pub enum MetaTypeParameterValue {
        Concrete(MetaTypeConcrete),
        Parameter(MetaTypeParameter),
    }

    impl MetaTypeParameterValue {
        pub fn parameter<T, P>(name: &'static str) -> Self
        where
            T: 'static + ?Sized + TypeInfo,
            P: 'static + ?Sized + TypeInfo,
        {
            MetaTypeParameterValue::Parameter(MetaTypeParameter {
                name,
                parent: MetaTypeGeneric::new::<T>(),
                instance_id: any::TypeId::of::<P>(),
            })
        }

        pub fn concrete<T>() -> Self
            where
                T: 'static + ?Sized + TypeInfo
        {
            MetaTypeParameterValue::Concrete(MetaTypeConcrete::new::<T>())
        }

        fn type_id(&self) -> any::TypeId {
            match self {
                MetaTypeParameterValue::Concrete(c) => c.type_id,
                MetaTypeParameterValue::Parameter(p) => p.instance_id,
            }
        }
    }

    impl From<MetaTypeParameterValue> for MetaType {
        fn from(p: MetaTypeParameterValue) -> Self {
            match p {
                MetaTypeParameterValue::Concrete(c) => MetaType::Concrete(c),
                MetaTypeParameterValue::Parameter(p) => MetaType::Parameter(p),
            }
        }
    }

    #[derive(Clone, Eq, PartialEq, PartialOrd, Ord, Debug)]
    pub struct MetaTypeGeneric {
        pub fn_type_info: fn() -> Type,
        pub path: &'static str,
    }

    impl MetaTypeGeneric {
        fn new<T>() -> Self
            where
                T: 'static + ?Sized + TypeInfo
        {
            Self {
                fn_type_info: T::type_info,
                path: T::path(),
            }
        }
    }
}

mod form {
    use super::*;

    pub trait Form {
        type Type: Clone + Eq + PartialEq + Ord + PartialOrd + std::fmt::Debug;
    }

    #[derive(Clone, Eq, PartialEq, PartialOrd, Ord, Debug)]
    pub enum MetaForm {}

    impl Form for MetaForm {
        type Type = MetaType;
    }

    #[derive(Clone, Eq, PartialEq, PartialOrd, Ord, Debug)]
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
    use std::fmt::{Debug, Formatter, Result};
    use std::any::Any;

    pub trait IntoCompact {
        type Output;

        fn into_compact(self, registry: &mut Registry) -> Self::Output;
    }

    // impl<T> IntoCompact for T where T: IntoCompact<Output = T<CompactForm>> {
    //     type Output = T<CompactForm>;
    //
    //     fn into_compact(self, registry: &mut Registry) -> Self::Output {
    //         self
    //     }
    // }

    #[derive(Debug, PartialEq, Eq, Ord, PartialOrd, Clone)]
    pub enum TypeId {
        Any(any::TypeId),
        Path(&'static str),
        Parameter(TypeParameter<CompactForm>),
        Generic(RegistryTypeGeneric<CompactForm>),
    }

    #[derive(Debug)]
    pub enum RegistryType<F: Form = MetaForm> {
        Definition(Type<F>),
        Parameter(TypeParameter<F>),
        Generic(RegistryTypeGeneric<F>),
    }

    impl IntoCompact for RegistryType<MetaForm> {
        type Output = RegistryType<CompactForm>;

        fn into_compact(self, registry: &mut Registry) -> Self::Output {
            match self {
                RegistryType::Definition(ty) => RegistryType::Definition(ty.into_compact(registry)),
                RegistryType::Parameter(tp) => RegistryType::Parameter(tp.into_compact(registry)),
                RegistryType::Generic(g) => RegistryType::Generic(g.into_compact(registry)),
            }
        }
    }

    #[derive(Debug, PartialEq, Eq, Ord, PartialOrd, Clone)]
    pub struct TypeParameter<F: Form = MetaForm> {
        path: &'static str,
        parent: F::Type,
    }

    impl IntoCompact for TypeParameter<MetaForm> {
        type Output = TypeParameter<CompactForm>;

        fn into_compact(self, registry: &mut Registry) -> Self::Output {
            TypeParameter {
                path: self.path,
                parent: registry.register_type(&self.parent),
            }
        }
    }

    #[derive(Debug, PartialEq, Eq, Ord, PartialOrd, Clone)]
    pub struct RegistryTypeGeneric<F: Form = MetaForm> {
        ty: F::Type,
        params: Vec<F::Type>,
    }

    impl IntoCompact for RegistryTypeGeneric<MetaForm> {
        type Output = RegistryTypeGeneric<CompactForm>;

        fn into_compact(self, registry: &mut Registry) -> Self::Output {
            RegistryTypeGeneric {
                ty: registry.register_type(&self.ty),
                params: self.params.iter().map(|p| registry.register_type(p)).collect(),
            }
        }
    }

    #[derive(Default)]
    pub struct Registry {
        type_table: BTreeMap<TypeId, usize>,
        type_ids: Vec<TypeId>,
        param_stack: Vec<MetaTypeParameterValue>,
        types: BTreeMap<<CompactForm as Form>::Type, RegistryType<CompactForm>>,
    }

    impl Debug for Registry {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result {
            for (id, ty) in self.types.iter() {
                writeln!(f, "{:?} {:?}", id, ty)?;
            }
            Ok(())
        }
    }

    impl Registry {
        fn intern_type<F, T>(&mut self, type_id: TypeId, f: F) -> <CompactForm as Form>::Type
        where
            F: FnOnce () -> T,
            T: IntoCompact<Output = RegistryType<CompactForm>>
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
                let registry_type = f();
                let compact_type = registry_type.into_compact(self);
                self.types.insert(symbol.clone(), compact_type);
            }
            symbol
        }

        pub fn register_type(&mut self, ty: &MetaType) -> <CompactForm as Form>::Type {
            match ty {
                MetaType::Concrete(concrete) => {
                    if concrete.params.len() > 0 {
                        let parameterized = MetaType::Parameterized(MetaTypeParameterized {
                            concrete: concrete.clone(),
                            params: concrete.params.iter().map(|concrete| MetaTypeParameterValue::Concrete(concrete.clone())).collect(),
                        });
                        self.register_type(&parameterized)
                    } else {
                        let type_id = TypeId::Any(concrete.type_id);
                        self.intern_type(type_id, || {
                            let type_info = (concrete.fn_type_info)();
                            RegistryType::Definition(type_info)
                        })
                    }
                }
                MetaType::Generic(ty) => {
                    let type_id = TypeId::Path(ty.path);
                    self.intern_type(type_id, || {
                        let type_info = (ty.fn_type_info)();
                        RegistryType::Definition(type_info)
                    })
                }
                MetaType::Parameter(p) => {
                    let generic_meta_type = MetaType::Generic(p.parent.clone());
                    let type_parameter = TypeParameter {
                        parent: generic_meta_type,
                        path: p.name,
                    };
                    let param_type_id = TypeId::Parameter(type_parameter.clone().into_compact(self));
                    self.intern_type(param_type_id, || {
                        RegistryType::Parameter(type_parameter)
                    })
                }
                MetaType::Parameterized(g) => {
                    let generic_meta_type = MetaType::Generic(MetaTypeGeneric {
                        fn_type_info: g.concrete.fn_type_info.clone(),
                        path: g.concrete.path,
                    });

                    self.param_stack.extend_from_slice(&g.params);

                    let params = g.concrete.params.iter().map(|p| {
                        if let Some(param) = self.param_stack.pop() {
                            if param.type_id() == p.type_id {
                                param.into()
                            } else {
                                self.param_stack.push(param);
                                MetaType::Concrete(p.clone())
                            }
                        } else {
                            MetaType::Concrete(p.clone())
                        }
                    }).collect::<Vec<_>>();

                    let generic = RegistryTypeGeneric {
                        ty: generic_meta_type,
                        params
                    };

                    let type_id = TypeId::Generic(generic.clone().into_compact(self));

                    self.intern_type(type_id, || RegistryType::Generic(generic))
                }
            }
        }
    }
}

use meta_type::*;
use form::*;
use registry::*;

#[derive(Debug)]
pub enum Type<F: Form = MetaForm> {
    Primitive(Primitive),
    Struct(Struct<F>),
}

impl IntoCompact for Type<MetaForm> {
    type Output = Type<CompactForm>;

    fn into_compact(self, registry: &mut Registry) -> Self::Output {
        match self {
            Type::Primitive(primitive) => Type::Primitive(primitive),
            Type::Struct(s) => Type::Struct(s.into_compact(registry)),
        }
    }
}

#[derive(Debug)]
pub struct Struct<F: Form = MetaForm> {
    fields: Vec<F::Type>,
}

impl IntoCompact for Struct<MetaForm> {
    type Output = Struct<CompactForm>;

    fn into_compact(self, registry: &mut Registry) -> Self::Output {
        Struct {
            fields: self.fields.iter().map(|f| registry.register_type(f)).collect()
        }
    }
}

#[derive(Debug)]
pub enum Primitive {
    Bool,
    U32,
}

pub trait TypeInfo {
    fn path() -> &'static str;
    fn params() -> Vec<MetaTypeConcrete> {
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

impl TypeInfo for u32 {
    fn path() -> &'static str {
        ""
    }

    fn type_info() -> Type {
        Type::Primitive(Primitive::U32)
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
                        MetaTypeParameterValue::parameter::<Self, T>("T"),
                        MetaTypeParameterValue::concrete::<bool>(),
                    ]
                ),
                // MetaType::of::<B<B<T, T>, bool>>(),
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

    fn params() -> Vec<MetaTypeConcrete> {
        vec![MetaTypeConcrete::new::<T>(), MetaTypeConcrete::new::<U>()]
    }

    fn type_info() -> Type {
        Type::Struct (Struct {
            fields: vec! [
                MetaType::parameter::<Self, T>("T"),
                MetaType::parameter::<Self, U>("U"),
            ]
        })
    }
}

fn main() {
    let mut registry = Registry::default();
    registry.register_type(&MetaType::of::<B<bool, u32>>());
    println!("{:?}", registry);
}
