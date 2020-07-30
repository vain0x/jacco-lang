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
    mod_outline: &'a KModOutline,
}

fn eval_term(term: &KTerm, xx: &Xx) -> Result<KConstValue, (EvalError, Loc)> {
    let value = match term {
        KTerm::Int { text, loc, .. } | KTerm::Float { text, loc, .. } => {
            let pair = eval_number(&text).map_err(|err| (EvalError::Lit(err), *loc))?;
            KConstValue::from(pair)
        }
        KTerm::True { .. } => KConstValue::Bool(true),
        KTerm::False { .. } => KConstValue::Bool(false),
        KTerm::Const { k_const, loc } => match &k_const.of(&xx.mod_outline.consts).value_opt {
            Some(value) => value.clone(),
            None => return Err((EvalError::Unsupported, *loc)),
        },
        _ => return Err((EvalError::Unsupported, term.loc())),
    };
    Ok(value)
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
        };
        eval_term(term, &xx)
    };
    match result {
        Ok(it) => Some(it),
        Err((err, loc)) => {
            logger.error(loc, format!("{:?}", err));
            None
        }
    }
}

pub(crate) fn eval_cps(mod_outlines: &mut KModOutlines, mods: &mut KModArena, logger: &Logger) {
    for k_mod in mod_outlines.keys().collect::<Vec<_>>() {
        for k_const in k_mod.of(&mod_outlines).consts.keys().collect::<Vec<_>>() {
            let init = k_const.of(&k_mod.of(&mods).consts);
            let value = match do_eval(&init.term, k_mod, mod_outlines, logger) {
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
            let init = static_var.of(&k_mod.of(&mods).static_vars);
            let value = match do_eval(&init.term, k_mod, mod_outlines, logger) {
                Some(it) => it,
                None => continue,
            };

            static_var
                .of_mut(&mut k_mod.of_mut(mod_outlines).static_vars)
                .value_opt = Some(value);
        }
    }
}
