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

    pub(crate) fn resolve(mut self) -> KTy {
        self.make_resolved();
        self
    }

    fn make_resolved(&mut self) {
        fn detect_infinite_loop(hint: &str) {
            let tick = {
                static mut TICK: usize = 0;
                unsafe {
                    TICK += 1;
                    TICK
                }
            };
            assert!(tick < 10_000_000, "Infinite loop? ({})", hint);
        }
        detect_infinite_loop("ty resolve");

        match self {
            KTy::Unresolved | KTy::Never | KTy::Unit | KTy::I32 | KTy::Struct { .. } => {}
            KTy::Meta(ref meta) => match meta.try_resolve() {
                None => {}
                Some(ty) => *self = ty,
            },
            KTy::Ptr { ty } => ty.make_resolved(),
            KTy::Fn {
                param_tys,
                result_ty,
            } => {
                for param_ty in param_tys {
                    param_ty.make_resolved();
                }
                result_ty.make_resolved();
            }
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

pub(crate) type KMetaTy = Rc<KMetaTyData>;

#[derive(Clone)]
pub(crate) struct KMetaTyData {
    ty: RefCell<KTy>,
    location: Location,
}

impl KMetaTyData {
    pub(crate) fn new(location: Location) -> Self {
        KMetaTyData {
            ty: RefCell::default(),
            location,
        }
    }

    pub(crate) fn try_resolve(&self) -> Option<KTy> {
        {
            let ty = &mut *self.ty.borrow_mut();
            ty.make_resolved();
            if ty.is_unresolved() {
                return None;
            }
        }

        Some(self.ty.borrow().clone())
    }

    pub(crate) fn bind(&self, ty: KTy) {
        debug_assert!(!ty.is_unresolved());

        let ty = ty.resolve();
        let old = replace(&mut *self.ty.borrow_mut(), ty);

        debug_assert!(old.is_unresolved());
    }
}

impl fmt::Debug for KMetaTyData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let p = self.ty.as_ptr() as usize;
        match &*self.ty.borrow() {
            KTy::Unresolved => write!(f, "?<{}>", p),
            ty => fmt::Debug::fmt(ty, f),
        }
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

#[derive(Clone, Debug)]
pub(crate) struct KFnData {
    pub(crate) name: String,
    pub(crate) params: Vec<KSymbol>,
    pub(crate) return_label: KSymbol,
    pub(crate) body: KNode,
    pub(crate) labels: Vec<KLabelData>,
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
