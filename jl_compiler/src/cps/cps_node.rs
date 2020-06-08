//! CPS 中間表現や、CPS 中間表現のもとになる命令列の定義

use super::*;
use std::cell::RefCell;
use std::fmt;
use std::mem::replace;
use std::rc::Rc;

#[derive(Debug)]
pub(crate) enum KCommand {
    Node {
        prim: KPrim,
        tys: Vec<KTy>,
        args: Vec<KTerm>,
        result_opt: Option<KSymbol>,
        cont_count: usize,
    },
    Label {
        label: KSymbol,
        params: Vec<KSymbol>,
    },
}

#[derive(Clone)]
pub(crate) enum KTy {
    Unresolved,
    Meta(KMetaTy),
    Never,
    Unit,
    I32,
    Ptr {
        ty: Box<KTy>,
    },
    Fn {
        param_tys: Vec<KTy>,
        result_ty: Box<KTy>,
    },
    Struct(KStruct),
}

impl KTy {
    pub(crate) fn is_unresolved(&self) -> bool {
        match self {
            KTy::Unresolved => true,
            _ => false,
        }
    }

    pub(crate) fn is_struct(&self) -> bool {
        match self {
            KTy::Struct { .. } => true,
            _ => false,
        }
    }

    pub(crate) fn as_ptr(self) -> Option<KTy> {
        match self {
            KTy::Ptr { ty } => Some(*ty),
            _ => None,
        }
    }

    pub(crate) fn into_ptr(self) -> KTy {
        KTy::Ptr { ty: Box::new(self) }
    }

    pub(crate) fn as_struct(self) -> Option<KStruct> {
        match self {
            KTy::Struct(k_struct) => Some(k_struct),
            _ => None,
        }
    }
}

impl Default for KTy {
    fn default() -> Self {
        KTy::Unresolved
    }
}

impl fmt::Debug for KTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            KTy::Unresolved => write!(f, "???"),
            KTy::Meta(meta) => fmt::Debug::fmt(meta, f),
            KTy::Never => write!(f, "never"),
            KTy::Unit => write!(f, "()"),
            KTy::I32 => write!(f, "i32"),
            KTy::Ptr { ty } => {
                write!(f, "*")?;
                fmt::Debug::fmt(&ty, f)
            }
            KTy::Fn {
                param_tys,
                result_ty,
            } => {
                write!(f, "fn(")?;
                for (i, _ty) in param_tys.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    // NOTE: this cause stack overflow
                    // fmt::Debug::fmt(ty, f)?;
                    write!(f, "_")?;
                }
                write!(f, ") -> ")?;
                fmt::Debug::fmt(result_ty, f)
            }
            KTy::Struct(k_struct) => {
                // FIXME: print name
                write!(f, "struct {}", k_struct.id())
            }
        }
    }
}

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
        self.meta_tys.push(KMetaTyData {
            ty: RefCell::default(),
            location,
        });
        KMetaTy { id }
    }

    pub(crate) fn meta_ty_get(&self, meta_ty: KMetaTy) -> &KMetaTyData {
        &self.meta_tys[meta_ty.id]
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct KMetaTy {
    id: usize,
}

impl KMetaTy {
    pub(crate) fn try_unwrap(self, ty_env: &KTyEnv) -> Option<&RefCell<KTy>> {
        if ty_env.meta_ty_get(self).ty.borrow().is_unresolved() {
            return None;
        }

        Some(&ty_env.meta_ty_get(self).ty)
    }

    pub(crate) fn bind(&mut self, new_ty: KTy, ty_env: &KTyEnv) {
        debug_assert!(self.try_unwrap(ty_env).is_none());
        debug_assert!(!new_ty.is_unresolved());

        let data = ty_env.meta_ty_get(*self);
        let old = replace(&mut *data.ty.borrow_mut(), new_ty);

        debug_assert!(old.is_unresolved());
    }
}

#[derive(Clone)]
pub(crate) struct KMetaTyData {
    ty: RefCell<KTy>,
    location: Location,
}

impl fmt::Debug for KMetaTyData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // let p = self.ty.as_ptr() as usize;
        // match &*self.ty.borrow() {
        //     KTy::Unresolved => write!(f, "?<{}>", p),
        //     ty => fmt::Debug::fmt(ty, f),
        // }
        // FIXME: need env
        fmt::Debug::fmt(&self.ty, f)
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub(crate) struct KLocal {
    id: usize,
}

impl KLocal {
    pub(crate) fn new(id: usize) -> Self {
        Self { id }
    }

    pub(crate) fn id(self) -> usize {
        self.id
    }
}

#[derive(Clone, Debug, Default)]
pub(crate) struct KLocalData {
    pub(crate) name: String,
    pub(crate) ty: KTy,
}

#[derive(Clone, Debug, Default)]
pub(crate) struct KVarData {
    pub(crate) local: KLocal,
    pub(crate) name: String,
    pub(crate) ty: RefCell<KTy>,
}

impl KVarData {
    pub(crate) fn new_with_ty(local: KLocal, name: String, ty: KTy) -> Self {
        KVarData {
            local,
            name,
            ty: RefCell::new(ty),
        }
    }
}

#[derive(Clone, Default)]
pub(crate) struct KSymbol {
    pub(crate) location: Location,
    pub(crate) def: Rc<KVarData>,
}

impl KSymbol {
    pub(crate) fn raw_name(&self) -> &str {
        &self.def.name
    }

    pub(crate) fn ty(&self) -> KTy {
        self.def.ty.borrow().clone()
    }
}

impl fmt::Debug for KSymbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}_{:X?}: {:?}",
            self.raw_name(),
            self.def.as_ref() as *const _,
            self.ty()
        )
    }
}

impl HaveLocation for KSymbol {
    fn location(&self) -> Location {
        self.location.clone()
    }
}

#[derive(Clone, Debug)]
pub(crate) enum KSymbolExt {
    Symbol(KSymbol),
    Fn(KFn),
    ExternFn(KExternFn),
}

impl KSymbolExt {
    pub(crate) fn expect_symbol(self) -> KSymbol {
        match self {
            KSymbolExt::Symbol(symbol) => symbol,
            _ => unreachable!("{:?}", self),
        }
    }
}

#[derive(Clone)]
pub(crate) struct KNode {
    pub(crate) prim: KPrim,
    pub(crate) tys: Vec<KTy>,
    pub(crate) args: Vec<KTerm>,
    pub(crate) results: Vec<KSymbol>,
    pub(crate) conts: Vec<KNode>,
}

impl Default for KNode {
    fn default() -> Self {
        KNode {
            prim: KPrim::Stuck,
            tys: vec![],
            args: vec![],
            results: vec![],
            conts: vec![],
        }
    }
}

impl fmt::Debug for KNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.tys.is_empty() {
            write!(f, "({:?}", self.prim)?;
        } else {
            write!(f, "({:?}::", self.prim)?;

            let mut list = f.debug_list();
            for ty in &self.tys {
                list.entry(ty);
            }
            list.finish()?;
        }

        write!(f, " ")?;

        {
            let mut list = f.debug_list();
            for arg in &self.args {
                list.entry(arg);
            }
            list.finish()?;
        }

        write!(f, " ")?;

        {
            let mut list = f.debug_list();
            for result in &self.results {
                list.entry(result);
            }
            list.finish()?;
        }

        if self.conts.len() == 1 {
            write!(f, ") => ")?;

            for cont in &self.conts {
                fmt::Debug::fmt(cont, f)?;
            }

            Ok(())
        } else {
            write!(f, " ")?;

            let mut list = f.debug_list();
            for cont in &self.conts {
                list.entry(cont);
            }
            list.finish()?;

            write!(f, ")")
        }
    }
}

#[derive(Clone, Debug, Default)]
pub(crate) struct KFnData {
    pub(crate) params: Vec<KSymbol>,
    pub(crate) body: KNode,
    pub(crate) labels: Vec<KLabelData>,
    pub(crate) label_sigs: Vec<KLabelSig>,
    pub(crate) ty_env: KTyEnv,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) struct KLabel {
    id: usize,
}

impl KLabel {
    #[allow(dead_code)]
    pub(crate) fn new(id: usize) -> Self {
        Self { id }
    }

    #[allow(dead_code)]
    pub(crate) fn id(self) -> usize {
        self.id
    }
}

#[derive(Clone, Debug)]
pub(crate) struct KLabelSig {
    name: String,
    param_tys: Vec<KTy>,
}

impl KLabelSig {
    pub(crate) fn new(name: String, param_tys: Vec<KTy>) -> Self {
        Self { name, param_tys }
    }

    #[allow(dead_code)]
    pub(crate) fn ty(&self) -> KTy {
        let param_tys = self.param_tys.iter().cloned().collect();
        KTy::Fn {
            param_tys,
            result_ty: Box::new(KTy::Never),
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct KLabelData {
    pub(crate) name: KSymbol,
    pub(crate) params: Vec<KSymbol>,
    pub(crate) body: KNode,
}

#[derive(Clone, Debug)]
pub(crate) struct KExternFnData {
    pub(crate) params: Vec<KSymbol>,
}

#[derive(Clone, Debug)]
pub(crate) struct KFieldTag {
    pub(crate) name: String,
    pub(crate) location: Location,
}

#[derive(Clone, Debug)]
pub(crate) struct KRoot {
    pub(crate) fns: Vec<KFnData>,
    pub(crate) extern_fns: Vec<KExternFnData>,
}
