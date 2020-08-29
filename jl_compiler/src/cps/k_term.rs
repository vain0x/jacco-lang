use super::*;
use crate::{
    parse::{PLoc, PToken},
    source::{Doc, Loc},
    utils::DebugWithContext,
};
use k_ty::KTyCause;
use std::fmt::{self, Debug, Formatter};

#[derive(Copy, Clone)]
pub(crate) enum KTermCause {
    Token(Doc, PToken),
}

impl KTermCause {
    pub(crate) fn loc(self) -> Loc {
        match self {
            KTermCause::Token(doc, token) => Loc::new(doc, PLoc::Token(token)),
        }
    }
}

/// CPS 原子項
#[derive(Clone)]
pub(crate) enum KTerm {
    Unit {
        loc: Loc,
    },
    True {
        loc: Loc,
    },
    False {
        loc: Loc,
    },
    Int {
        text: String,
        ty: KTy2,
        cause: KTermCause,
    },
    Float {
        text: String,
        ty: KTy2,
        loc: Loc,
    },
    Char {
        text: String,
        ty: KTy2,
        loc: Loc,
    },
    Str {
        text: String,
        loc: Loc,
    },
    Name(KSymbol),
    Alias {
        alias: KAlias,
        loc: Loc,
    },
    Const {
        k_mod: KMod,
        k_const: KConst,
        loc: Loc,
    },
    StaticVar {
        static_var: KStaticVar,
        loc: Loc,
    },
    Fn {
        k_fn: KFn,
        ty: KTy2,
        loc: Loc,
    },
    Label {
        label: KLabel,
        loc: Loc,
    },
    Return {
        k_fn: KFn,
        loc: Loc,
    },
    ExternFn {
        extern_fn: KExternFn,
        loc: Loc,
    },
    RecordTag {
        k_mod: KMod,
        k_struct: KStruct,
        loc: Loc,
    },
    FieldTag(KFieldTag),
}

impl KTerm {
    // いまのところ CPS 変換からしか呼ばれていないので多相型をインスンタンス化する必要はなくて、型環境は受け取っていない
    pub(crate) fn ty<'a>(
        &self,
        k_mod: KMod,
        mod_outline: &KModOutline,
        labels: &KLabelSigArena,
        local_vars: &KLocalVarArena,
        mod_outlines: &KModOutlines,
    ) -> KTy2 {
        let ty = match self {
            KTerm::Unit { .. } => KTy2::Unit,
            KTerm::Int { ty, .. } => ty.clone(),
            KTerm::Float { ty, .. } => ty.clone(),
            KTerm::Char { ty, .. } => ty.clone(),
            KTerm::Str { .. } => KTy2::C8.into_ptr(KMut::Const),
            KTerm::True { .. } | KTerm::False { .. } => KTy2::BOOL,
            KTerm::Name(symbol) => symbol.local_var.ty(&local_vars),
            KTerm::Alias { alias, .. } => KTy2::Alias(k_mod, *alias),
            KTerm::Const { k_mod, k_const, .. } => {
                k_const.ty(&k_mod.of(mod_outlines).consts).erasure(*k_mod)
            }
            KTerm::StaticVar { static_var, .. } => {
                static_var.ty(&mod_outline.static_vars).erasure(k_mod)
            }
            KTerm::Fn { ty, .. } => ty.clone(),
            KTerm::Label { label, .. } => label.ty(labels),
            KTerm::Return { k_fn, .. } => k_fn.return_ty(&mod_outline.fns).erasure(k_mod),
            KTerm::ExternFn { extern_fn, .. } => {
                extern_fn.ty(&mod_outline.extern_fns).erasure(k_mod)
            }
            KTerm::RecordTag {
                k_mod, k_struct, ..
            } => {
                let mod_outline = k_mod.of(mod_outlines);
                k_struct
                    .tag_ty(&mod_outline.structs, &mod_outline.struct_enums)
                    .erasure(*k_mod)
            }
            KTerm::FieldTag(field_tag) => {
                error!("don't obtain type of field tag {:?}", field_tag);
                KTy2::Unresolved {
                    cause: KTyCause::FieldTag,
                }
            }
        };

        match ty {
            KTy2::Alias(k_mod, alias) => alias
                .of(&k_mod.of(mod_outlines).aliases)
                .referent_as_ty()
                .unwrap_or(ty),
            _ => ty,
        }
    }

    pub(crate) fn loc(&self) -> Loc {
        match self {
            KTerm::Int { cause, .. } => cause.loc(),
            KTerm::Name(KSymbol { cause, .. }) => cause.loc(),
            KTerm::Unit { loc }
            | KTerm::Float { loc, .. }
            | KTerm::Char { loc, .. }
            | KTerm::Str { loc, .. }
            | KTerm::True { loc }
            | KTerm::False { loc }
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

impl<'a>
    DebugWithContext<(
        &'a KModOutline,
        Option<(&'a KLocalVarArena, &'a KLabelArena)>,
    )> for KTerm
{
    fn fmt(
        &self,
        context: &(
            &'a KModOutline,
            Option<(&'a KLocalVarArena, &'a KLabelArena)>,
        ),
        f: &mut Formatter<'_>,
    ) -> fmt::Result {
        let (mod_outline, local_var_opt) = context;

        match self {
            KTerm::Unit { .. } => write!(f, "()"),
            KTerm::Int { text, .. } => write!(f, "{}", text),
            KTerm::Float { text, .. } => write!(f, "{}", text),
            KTerm::Char { text, .. } => write!(f, "{:?}", text),
            KTerm::Str { text, .. } => write!(f, "{:?}", text),
            KTerm::True { .. } => write!(f, "true"),
            KTerm::False { .. } => write!(f, "false"),
            KTerm::Name(symbol) => match local_var_opt {
                Some((local_vars, _)) => write!(f, "{}", symbol.local_var.of(local_vars).name),
                None => write!(f, "symbol({:?})", symbol.cause),
            },
            KTerm::Alias { alias, .. } => write!(f, "{}", alias.of(&mod_outline.aliases).name()),
            KTerm::Const {
                k_mod: _, k_const, ..
            } => write!(f, "const#{:?}", k_const),
            KTerm::StaticVar { static_var, .. } => {
                write!(f, "{}", static_var.name(&mod_outline.static_vars))
            }
            KTerm::Fn { k_fn, .. } => write!(f, "{}", k_fn.name(&mod_outline.fns)),
            KTerm::Label { label, .. } => match local_var_opt {
                Some((_, labels)) => write!(f, "{}", label.of(labels).name),
                None => write!(f, "label#{}", label.to_index()),
            },
            KTerm::Return { k_fn, .. } => write!(f, "return({})", k_fn.name(&mod_outline.fns)),
            KTerm::ExternFn { extern_fn, .. } => {
                write!(f, "{}", extern_fn.name(&mod_outline.extern_fns))
            }
            KTerm::RecordTag { .. } => {
                // write!(f, "record_tag::[{}]", k_struct.name(&mod_outline.structs))
                Ok(())
            }
            KTerm::FieldTag(KFieldTag { name, .. }) => write!(f, "{}", name),
        }
    }
}
