use super::*;
use crate::{source::Loc, utils::DebugWithContext};
use std::fmt::{self, Debug, Formatter};

/// CPS 原子項
#[derive(Clone)]
pub(crate) enum KTerm {
    Unit { loc: Loc },
    Int { text: String, ty: KTy2, loc: Loc },
    Float { text: String, ty: KTy2, loc: Loc },
    Char { text: String, ty: KTy2, loc: Loc },
    Str { text: String, loc: Loc },
    True { loc: Loc },
    False { loc: Loc },
    Name(KSymbol),
    Alias { alias: KAlias, loc: Loc },
    Const { k_const: KConst, loc: Loc },
    StaticVar { static_var: KStaticVar, loc: Loc },
    Fn { k_fn: KFn, loc: Loc },
    Label { label: KLabel, loc: Loc },
    Return { k_fn: KFn, loc: Loc },
    ExternFn { extern_fn: KExternFn, loc: Loc },
    RecordTag { k_struct: KStruct, loc: Loc },
    FieldTag(KFieldTag),
}

impl KTerm {
    pub(crate) fn ty<'a>(
        &self,
        k_mod: KMod,
        mod_outline: &KModOutline,
        labels: &KLabelSigArena,
        locals: &KLocalArena,
    ) -> KTy2 {
        match self {
            KTerm::Unit { .. } => KTy2::Unit,
            KTerm::Int { ty, .. } => ty.clone(),
            KTerm::Float { ty, .. } => ty.clone(),
            KTerm::Char { ty, .. } => ty.clone(),
            KTerm::Str { .. } => KTy2::C8.into_ptr(KMut::Const),
            KTerm::True { .. } | KTerm::False { .. } => KTy2::BOOL,
            KTerm::Name(symbol) => symbol.local.ty(&locals),
            KTerm::Alias { .. } => {
                // FIXME: 実装. mod_outlines を受け取る必要がある
                KTy2::Unresolved
            }
            KTerm::Const { k_const, .. } => k_const.ty(&mod_outline.consts).to_ty2(k_mod),
            KTerm::StaticVar { static_var, .. } => {
                static_var.ty(&mod_outline.static_vars).to_ty2(k_mod)
            }
            KTerm::Fn { k_fn, .. } => k_fn.ty(&mod_outline.fns).to_ty2(k_mod),
            KTerm::Label { label, .. } => label.ty(labels),
            KTerm::Return { k_fn, .. } => k_fn.return_ty(&mod_outline.fns).to_ty2(k_mod),
            KTerm::ExternFn { extern_fn, .. } => {
                extern_fn.ty(&mod_outline.extern_fns).to_ty2(k_mod)
            }
            KTerm::RecordTag { k_struct, .. } => k_struct
                .tag_ty(&mod_outline.structs, &mod_outline.enum_reprs)
                .to_ty2(k_mod),
            KTerm::FieldTag(field_tag) => {
                error!("don't obtain type of field tag {:?}", field_tag);
                KTy2::Unresolved
            }
        }
    }

    pub(crate) fn loc(&self) -> Loc {
        match self {
            KTerm::Unit { loc }
            | KTerm::Int { loc, .. }
            | KTerm::Float { loc, .. }
            | KTerm::Char { loc, .. }
            | KTerm::Str { loc, .. }
            | KTerm::True { loc }
            | KTerm::False { loc }
            | KTerm::Name(KSymbol { loc, .. })
            | KTerm::Alias { loc, .. }
            | KTerm::Const { loc, .. }
            | KTerm::StaticVar { loc, .. }
            | KTerm::Fn { loc, .. }
            | KTerm::Label { loc, .. }
            | KTerm::Return { loc, .. }
            | KTerm::ExternFn { loc, .. }
            | KTerm::RecordTag { loc, .. }
            | KTerm::FieldTag(KFieldTag { loc, .. }) => *loc,
        }
    }
}

impl Debug for KTerm {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        // FIXME: 実装
        write!(f, "<KTerm>")
    }
}

impl<'a> DebugWithContext<(&'a KModOutline, Option<(&'a KLocalArena, &'a KLabelArena)>)> for KTerm {
    fn fmt(
        &self,
        context: &(&'a KModOutline, Option<(&'a KLocalArena, &'a KLabelArena)>),
        f: &mut Formatter<'_>,
    ) -> fmt::Result {
        let (mod_outline, local_opt) = context;

        match self {
            KTerm::Unit { .. } => write!(f, "()"),
            KTerm::Int { text, .. } => write!(f, "{}", text),
            KTerm::Float { text, .. } => write!(f, "{}", text),
            KTerm::Char { text, .. } => write!(f, "{:?}", text),
            KTerm::Str { text, .. } => write!(f, "{:?}", text),
            KTerm::True { .. } => write!(f, "true"),
            KTerm::False { .. } => write!(f, "false"),
            KTerm::Name(symbol) => match local_opt {
                Some((locals, _)) => write!(f, "{}", symbol.local.of(locals).name),
                None => write!(f, "symbol({:?})", symbol.loc),
            },
            KTerm::Alias { alias, .. } => write!(f, "{}", alias.of(&mod_outline.aliases).name()),
            KTerm::Const { k_const, .. } => write!(f, "{}", k_const.of(&mod_outline.consts).name),
            KTerm::StaticVar { static_var, .. } => {
                write!(f, "{}", static_var.name(&mod_outline.static_vars))
            }
            KTerm::Fn { k_fn, .. } => write!(f, "{}", k_fn.name(&mod_outline.fns)),
            KTerm::Label { label, .. } => match local_opt {
                Some((_, labels)) => write!(f, "{}", label.of(labels).name),
                None => write!(f, "label#{}", label.to_index()),
            },
            KTerm::Return { k_fn, .. } => write!(f, "return({})", k_fn.name(&mod_outline.fns)),
            KTerm::ExternFn { extern_fn, .. } => {
                write!(f, "{}", extern_fn.name(&mod_outline.extern_fns))
            }
            KTerm::RecordTag { k_struct, .. } => {
                write!(f, "record_tag::[{}]", k_struct.name(&mod_outline.structs))
            }
            KTerm::FieldTag(KFieldTag { name, .. }) => write!(f, "{}", name),
        }
    }
}
