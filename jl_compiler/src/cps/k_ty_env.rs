use super::{KEnumOutline, KMetaTy, KMetaTyData, KMut, KStructOutline, KTy};
use crate::token::Location;
use std::cell::RefCell;

#[derive(Clone, Debug, Default)]
pub(crate) struct KTyEnv {
    meta_tys: Vec<KMetaTyData>,
}

impl KTyEnv {
    pub(crate) fn is_empty(&self) -> bool {
        self.meta_tys.is_empty()
    }

    pub(crate) fn meta_ty_new(&mut self, location: Location) -> KMetaTy {
        let id = self.meta_tys.len();
        self.meta_tys
            .push(KMetaTyData::new(RefCell::default(), location));
        KMetaTy::new(id)
    }

    pub(crate) fn meta_ty_get(&self, meta_ty: KMetaTy) -> &KMetaTyData {
        &self.meta_tys[meta_ty.id()]
    }

    pub(crate) fn is_unbound(&self, ty: &KTy) -> bool {
        match ty {
            KTy::Meta(meta_ty) => match meta_ty.try_unwrap(self) {
                Some(ty) => self.is_unbound(&*ty.borrow()),
                None => true,
            },
            _ => false,
        }
    }

    pub(crate) fn is_unit_or_never(&self, ty: &KTy) -> bool {
        match ty {
            KTy::Unit | KTy::Never => true,
            KTy::Meta(meta_ty) => match meta_ty.try_unwrap(self) {
                Some(ty) => self.is_unit_or_never(&*ty.borrow()),
                None => false,
            },
            _ => false,
        }
    }

    pub(crate) fn is_primitive(&self, ty: &KTy) -> bool {
        match ty {
            KTy::Meta(meta_ty) => match meta_ty.try_unwrap(self) {
                Some(ty) => self.is_primitive(&*ty.borrow()),
                None => false,
            },
            ty => ty.is_primitive(),
        }
    }

    pub(crate) fn is_ptr(&self, ty: &KTy) -> bool {
        self.as_ptr(ty).is_some()
    }

    pub(crate) fn as_ptr(&self, ty: &KTy) -> Option<(KMut, KTy)> {
        match ty {
            KTy::Ptr { k_mut, ty } => Some((*k_mut, ty.as_ref().clone())),
            KTy::Meta(meta_ty) => {
                let ty = meta_ty.try_unwrap(self)?;
                self.as_ptr(&ty.borrow().clone())
            }
            _ => None,
        }
    }

    pub(crate) fn is_struct_or_enum(&self, ty: &KTy) -> bool {
        match ty {
            KTy::Enum(_) | KTy::Struct(_) => true,
            KTy::Meta(meta_ty) => match meta_ty.try_unwrap(self) {
                Some(ty) => self.is_struct_or_enum(&ty.borrow().clone()),
                None => false,
            },
            _ => false,
        }
    }

    pub(crate) fn display(
        &self,
        ty: &KTy,
        enums: &[KEnumOutline],
        structs: &[KStructOutline],
    ) -> String {
        match ty {
            KTy::Meta(meta_ty) => {
                let ty = match meta_ty.try_unwrap(self) {
                    Some(ty) => ty,
                    None => return "{unknown}".to_string(),
                };
                self.display(&ty.borrow().clone(), enums, structs)
            }
            KTy::Unresolved => "{unresolved}".to_string(),
            KTy::Never
            | KTy::Unit
            | KTy::I32
            | KTy::I64
            | KTy::Usize
            | KTy::F64
            | KTy::C8
            | KTy::Bool => format!("{:?}", ty),
            KTy::Ptr { k_mut, ty } => format!(
                "*{}{}",
                match k_mut {
                    KMut::Const => "",
                    KMut::Mut => "mut ",
                },
                self.display(ty, enums, structs)
            ),
            KTy::Fn {
                param_tys,
                result_ty,
            } => format!(
                "|{}| -> {}",
                param_tys
                    .iter()
                    .map(|ty| self.display(ty, enums, structs))
                    .collect::<Vec<_>>()
                    .join(", "),
                self.display(result_ty, enums, structs)
            ),
            KTy::Enum(k_enum) => k_enum.name(enums).to_string(),
            KTy::Struct(k_struct) => k_struct.name(structs).to_string(),
        }
    }
}
