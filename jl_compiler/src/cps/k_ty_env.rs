use super::{
    k_enum::KEnumArena, k_meta_ty::KMetaTys, KMetaTy, KMetaTyData, KMut, KStructArena, KTy,
};
use crate::token::Location;
use std::cell::RefCell;

#[derive(Clone, Debug, Default)]
pub(crate) struct KTyEnv {
    meta_tys: KMetaTys,
}

impl KTyEnv {
    pub(crate) const EMPTY: &'static KTyEnv = &KTyEnv {
        meta_tys: KMetaTys::new(),
    };

    pub(crate) fn is_empty(&self) -> bool {
        self.meta_tys.is_empty()
    }

    pub(crate) fn meta_ty_new(&mut self, location: Location) -> KMetaTy {
        self.meta_tys
            .alloc(KMetaTyData::new(RefCell::default(), location))
    }

    pub(crate) fn meta_ty_get(&self, meta_ty: KMetaTy) -> &KMetaTyData {
        &self.meta_tys[meta_ty]
    }

    pub(crate) fn display(&self, ty: &KTy, enums: &KEnumArena, structs: &KStructArena) -> String {
        match ty {
            KTy::Meta(meta_ty) => {
                let ty = match meta_ty.try_unwrap(self) {
                    Some(ty) => ty,
                    None => return "{unknown}".to_string(),
                };
                self.display(&ty.borrow().clone().to_ty1(), enums, structs)
            }
            KTy::Unresolved => "{unresolved}".to_string(),
            KTy::Never
            | KTy::Unit
            | KTy::I8
            | KTy::I16
            | KTy::I32
            | KTy::I64
            | KTy::Isize
            | KTy::U8
            | KTy::U16
            | KTy::U32
            | KTy::U64
            | KTy::Usize
            | KTy::F32
            | KTy::F64
            | KTy::C8
            | KTy::C16
            | KTy::C32
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
            KTy::Alias(_) => {
                // FIXME: 実装. aliases を引数にとる？
                "{alias}".to_string()
            }
            KTy::Enum(k_enum) => k_enum.name(enums).to_string(),
            KTy::Struct(k_struct) => k_struct.name(structs).to_string(),
        }
    }
}
