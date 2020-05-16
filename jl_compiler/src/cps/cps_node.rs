//! CPS 中間表現や、CPS 中間表現のもとになる命令列の定義

use super::*;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

#[derive(Debug)]
pub(crate) enum XCommand {
    Prim {
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
    Unresolved(Option<KMetaTy>),
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
    Symbol {
        def: Rc<RefCell<KStructDef>>,
    },
}

impl KTy {
    pub(crate) fn new_unresolved() -> KTy {
        KTy::default()
    }

    pub(crate) fn is_symbol(&self) -> bool {
        matches!(self, KTy::Symbol {.. })
    }

    pub(crate) fn into_ptr(self) -> KTy {
        KTy::Ptr { ty: Box::new(self) }
    }

    pub(crate) fn resolve(mut self) -> KTy {
        loop {
            match self {
                KTy::Unresolved(Some(ref meta)) if meta.is_bound() => {
                    self = meta.content_ty().unwrap();
                    continue;
                }
                ty => return ty,
            }
        }
    }
}

impl Default for KTy {
    fn default() -> Self {
        KTy::Unresolved(None)
    }
}

impl fmt::Debug for KTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            KTy::Unresolved(None) => write!(f, "???"),
            KTy::Unresolved(Some(meta)) => fmt::Debug::fmt(meta, f),
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
            KTy::Symbol { def } => {
                let def = def.borrow();
                write!(f, "struct {}", def.name.text)
            }
        }
    }
}

#[derive(Clone)]
pub(crate) struct KMetaTy {
    slot: Rc<RefCell<Option<KTy>>>,
    location: Location,
}

impl KMetaTy {
    pub(crate) fn new(location: Location) -> Self {
        KMetaTy {
            slot: Rc::default(),
            location,
        }
    }

    pub(crate) fn ptr_eq(&self, other: &KMetaTy) -> bool {
        Rc::ptr_eq(&self.slot, &other.slot)
    }

    pub(crate) fn content_ty(&self) -> Option<KTy> {
        self.slot.borrow().clone()
    }

    pub(crate) fn is_bound(&self) -> bool {
        self.slot.borrow().is_some()
    }

    pub(crate) fn bind(&mut self, ty: KTy) {
        let ty = ty.resolve();
        let old = std::mem::replace(&mut *self.slot.borrow_mut(), Some(ty));
        debug_assert!(old.is_none());
    }
}

impl fmt::Debug for KMetaTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let p = self.slot.as_ptr() as usize;
        match self.slot.borrow().as_ref() {
            None => write!(f, "?<{}>", p),
            Some(ty) => fmt::Debug::fmt(ty, f),
        }
    }
}

#[derive(Clone, Default)]
pub(crate) struct KSymbol {
    pub(crate) text: String,
    pub(crate) ty: KTy,
    pub(crate) location: Location,
    pub(crate) id_slot: Rc<RefCell<Option<usize>>>,
    pub(crate) def_ty_slot: Rc<RefCell<KTy>>,
}

impl KSymbol {
    fn init_id(&self, mut get_id: impl FnMut() -> usize) {
        let mut slot = self.id_slot.borrow_mut();
        if slot.is_none() {
            *slot = Some(get_id());
        }
    }

    pub(crate) fn unique_name(&self, ids: &mut IdProvider) -> String {
        self.init_id(|| ids.next());

        match *self.id_slot.borrow() {
            Some(id) => format!("{}_{}", self.text, id),
            None => format!("{}<{:X?}>", self.text, self.def_ty_slot.as_ptr()),
        }
    }

    pub(crate) fn def_ty_slot(&self) -> &RefCell<KTy> {
        &self.def_ty_slot
    }
}

impl fmt::Debug for KSymbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}_{:X?}: {:?}",
            self.text,
            self.def_ty_slot.as_ptr(),
            self.ty
        )
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
pub(crate) struct KFn {
    pub(crate) name: KSymbol,
    pub(crate) params: Vec<KSymbol>,
    pub(crate) body: KNode,
    pub(crate) labels: Vec<KFn>,
}

#[derive(Clone, Debug)]
pub(crate) struct KExternFn {
    pub(crate) name: KSymbol,
    pub(crate) params: Vec<KSymbol>,
    pub(crate) result: KTy,
}

#[derive(Clone, Debug)]
pub(crate) struct KStructDef {
    pub(crate) name: KSymbol,
}

#[derive(Clone, Debug)]
pub(crate) struct KStruct {
    pub(crate) def: Rc<RefCell<KStructDef>>,
}

#[derive(Clone, Debug)]
pub(crate) struct KRoot {
    pub(crate) extern_fns: Vec<KExternFn>,
    pub(crate) fns: Vec<KFn>,
    pub(crate) structs: Vec<KStruct>,
}
