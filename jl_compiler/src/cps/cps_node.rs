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
    Struct {
        struct_ref: KStruct,
    },
}

impl KTy {
    pub(crate) fn is_struct(&self) -> bool {
        matches!(self, KTy::Struct{..})
    }

    pub(crate) fn into_ptr(self) -> KTy {
        KTy::Ptr { ty: Box::new(self) }
    }

    pub(crate) fn resolve(mut self) -> KTy {
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

        loop {
            match self {
                KTy::Meta(ref meta) => match meta.try_resolve() {
                    None => return self,
                    Some(ty) => {
                        detect_infinite_loop("resolve meta ty");
                        self = ty;
                        continue;
                    }
                },
                ty => return ty,
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
            KTy::Struct { struct_ref } => write!(f, "struct {}", struct_ref.raw_name()),
        }
    }
}

pub(crate) type KMetaTy = Rc<KMetaTyData>;

#[derive(Clone)]
pub(crate) struct KMetaTyData {
    ty_opt: RefCell<Option<KTy>>,
    location: Location,
}

impl KMetaTyData {
    pub(crate) fn new(location: Location) -> Self {
        KMetaTyData {
            ty_opt: RefCell::default(),
            location,
        }
    }

    pub(crate) fn try_resolve(&self) -> Option<KTy> {
        let slot = self.ty_opt.borrow();
        let ty = slot.as_ref()?;
        Some(ty.clone())
    }

    pub(crate) fn bind(&self, ty: KTy) {
        let ty = ty.resolve();
        let old = replace(&mut *self.ty_opt.borrow_mut(), Some(ty));
        debug_assert!(old.is_none());
    }
}

impl fmt::Debug for KMetaTyData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let p = self.ty_opt.as_ptr() as usize;
        match self.ty_opt.borrow().as_ref() {
            None => write!(f, "?<{}>", p),
            Some(ty) => fmt::Debug::fmt(ty, f),
        }
    }
}

#[derive(Clone, Debug, Default)]
pub(crate) struct KVarData {
    pub(crate) name: String,
    pub(crate) ty: RefCell<KTy>,
    pub(crate) id_opt: RefCell<Option<usize>>,
}

impl KVarData {
    pub(crate) fn new_with_ty(name: String, ty: KTy) -> Self {
        KVarData {
            name,
            ty: RefCell::new(ty),
            ..Default::default()
        }
    }
}

#[derive(Clone, Default)]
pub(crate) struct KSymbol {
    pub(crate) ty: RefCell<KTy>,
    pub(crate) location: Location,
    pub(crate) def: Rc<KVarData>,
}

impl KSymbol {
    pub(crate) fn raw_name(&self) -> &str {
        &self.def.name
    }

    pub(crate) fn ty(&self) -> KTy {
        self.ty.borrow().clone()
    }

    pub(crate) fn def_ty_slot(&self) -> &RefCell<KTy> {
        &self.def.ty
    }
}

impl fmt::Debug for KSymbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}_{:X?}: {:?}",
            self.raw_name(),
            self.def.as_ref() as *const _,
            self.ty
        )
    }
}

impl HaveLocation for KSymbol {
    fn location(&self) -> Location {
        self.location.clone()
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
    pub(crate) name: KSymbol,
    pub(crate) params: Vec<KSymbol>,
    pub(crate) body: KNode,
    pub(crate) labels: Vec<KFnData>,
}

#[derive(Clone, Debug)]
pub(crate) struct KExternFnData {
    pub(crate) name: KSymbol,
    pub(crate) params: Vec<KSymbol>,
    pub(crate) result_ty: KTy,
}

#[derive(Clone, Debug)]
pub(crate) struct KStructData {
    pub(crate) name: String,
    pub(crate) def_site_ty: RefCell<KTy>,
    pub(crate) location: Location,
    pub(crate) fields: Vec<KFieldData>,
    pub(crate) symbol: KSymbol,
    pub(crate) id_opt: RefCell<Option<usize>>,
}

impl KStructData {
    pub(crate) fn raw_name(&self) -> &str {
        &self.name
    }
}

#[derive(Clone, Debug)]
pub(crate) struct KFieldData {
    pub(crate) name: KSymbol,
}

#[derive(Clone, Debug)]
pub(crate) struct KFieldTag {
    pub(crate) name: String,
    pub(crate) location: Location,
}

#[derive(Clone, Debug)]
pub(crate) struct KStruct {
    pub(crate) def: Rc<KStructData>,
}

impl KStruct {
    pub(crate) fn raw_name(&self) -> &str {
        self.def.raw_name()
    }

    pub(crate) fn is_same(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.def, &other.def)
    }
}

impl From<Rc<KStructData>> for KStruct {
    fn from(data: Rc<KStructData>) -> Self {
        KStruct { def: data }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct KRoot {
    pub(crate) extern_fns: Vec<KExternFnData>,
    pub(crate) fns: Vec<KFnData>,
    pub(crate) structs: Vec<KStruct>,
}
