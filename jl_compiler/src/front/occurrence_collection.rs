//! シンボルの出現箇所を収集する。

use super::*;
use name_resolution::{DefOrUse, NAbsName};
use std::{collections::HashMap, rc::Rc};

#[derive(Default)]
pub(crate) struct Occurrences {
    pub(crate) def_sites: HashMap<NAbsName, Vec<Location>>,
    pub(crate) use_sites: HashMap<NAbsName, Vec<Location>>,
    // pub(crate) field_uses: Vec<(String, Location)>,
}

pub(crate) fn collect_occurrences(p_root: &PRoot, res: Rc<NameResolution>) -> Occurrences {
    // FIXME: フィールド参照や名前のパスの一部などの出現も含める。
    let mut occurrences = Occurrences::default();

    for (p_name, &(abs_name, def_or_use)) in res.occurrences.enumerate() {
        let location = p_name.of(&p_root.names).token.of(&p_root.tokens).location();

        match def_or_use {
            DefOrUse::Def => {
                occurrences
                    .def_sites
                    .entry(abs_name)
                    .or_insert(vec![])
                    .push(location);
            }
            DefOrUse::Use => {
                occurrences
                    .use_sites
                    .entry(abs_name)
                    .or_insert(vec![])
                    .push(location);
            }
        }
    }

    occurrences
}
