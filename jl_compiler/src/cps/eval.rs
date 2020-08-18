// コンパイル時実行

use super::*;
use crate::{
    source::Loc,
    token::{eval_number, LitErr},
};

#[derive(Debug)]
enum EvalError {
    Unsupported,
    Lit(LitErr),
}

impl From<LitErr> for EvalError {
    fn from(err: LitErr) -> Self {
        EvalError::Lit(err)
    }
}

struct Xx<'a> {
    #[allow(unused)]
    mod_outline: &'a KModOutline,
    mod_outlines: &'a KModOutlines,
}

fn eval_term(term: &KTerm, xx: &Xx) -> Result<Option<KConstValue>, (EvalError, Loc)> {
    let value = match term {
        KTerm::Unit { .. } => return Ok(None),
        KTerm::True { .. } => KConstValue::Bool(true),
        KTerm::False { .. } => KConstValue::Bool(false),
        KTerm::Int { text, cause, .. } => {
            let pair = eval_number(&text).map_err(|err| (EvalError::Lit(err), cause.loc()))?;
            KConstValue::from(pair)
        }
        KTerm::Float { text, loc, .. } => {
            let pair = eval_number(&text).map_err(|err| (EvalError::Lit(err), *loc))?;
            KConstValue::from(pair)
        }
        KTerm::Const { k_mod, k_const, .. } => {
            match &k_const.of(&k_mod.of(xx.mod_outlines).consts).value_opt {
                Some(value) => value.clone(),
                None => return Ok(None),
            }
        }
        _ => return Err((EvalError::Unsupported, term.loc())),
    };
    Ok(Some(value))
}

fn do_eval(
    term: &KTerm,
    k_mod: KMod,
    mod_outlines: &KModOutlines,
    logger: &Logger,
) -> Option<KConstValue> {
    let result = {
        let xx = Xx {
            mod_outline: k_mod.of(&mod_outlines),
            mod_outlines,
        };
        eval_term(term, &xx)
    };
    match result {
        Ok(it) => it,
        Err((err, loc)) => {
            logger.error(loc, format!("{:?}", err));
            None
        }
    }
}

pub(crate) fn eval_cps(mod_outlines: &mut KModOutlines, mods: &mut KModArena, logger: &Logger) {
    for k_mod in mod_outlines.keys().collect::<Vec<_>>() {
        for k_const in k_mod.of(&mod_outlines).consts.keys().collect::<Vec<_>>() {
            let (_node, term) = match &k_const.of(&k_mod.of(&mods).consts).init_opt {
                Some(it) => it,
                None => continue,
            };
            let value = match do_eval(term, k_mod, mod_outlines, logger) {
                Some(it) => it,
                None => continue,
            };

            k_const
                .of_mut(&mut k_mod.of_mut(mod_outlines).consts)
                .value_opt = Some(value);
        }

        for static_var in k_mod
            .of(&mod_outlines)
            .static_vars
            .keys()
            .collect::<Vec<_>>()
        {
            let (_node, term) = match &static_var.of(&k_mod.of(&mods).static_vars).init_opt {
                Some(it) => it,
                None => continue,
            };
            let value = match do_eval(term, k_mod, mod_outlines, logger) {
                Some(it) => it,
                None => continue,
            };

            static_var
                .of_mut(&mut k_mod.of_mut(mod_outlines).static_vars)
                .value_opt = Some(value);
        }
    }
}
