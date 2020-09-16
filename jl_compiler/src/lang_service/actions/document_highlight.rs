use super::{collect_def_sites, collect_use_sites, hit_test, Doc, LangService, TPos16, TRange};
use crate::{
    cps::*,
    lang_service::{
        doc_analysis::DocContentAnalysisMut,
        lang_service::{loc_to_range, to_range16},
    },
    parse::{AExpr, AFieldExpr, PToken},
    source::Loc,
};

struct FieldOccurrenceInFnCollector<'a> {
    k_mod: KMod,
    mod_outline: &'a KModOutline,
    mod_outlines: &'a KModOutlines,
    fn_data: &'a KFnData,
    occurrences: &'a mut Vec<(KMod, KField, Loc)>,
}

impl FieldOccurrenceInFnCollector<'_> {
    fn do_on_node(&mut self, node: &KNode) {
        let (record, field_name, loc) = match node.prim {
            KPrim::GetField | KPrim::GetFieldMut => match node.args.as_slice() {
                [record, KTerm::FieldTag(KFieldTag { name, loc, .. })] => {
                    (record, name.as_str(), loc.clone())
                }
                _ => return,
            },
            _ => return,
        };

        let ty = record.ty(
            self.mod_outline,
            &self.fn_data.label_sigs,
            &self.fn_data.local_vars,
        );

        let k_struct = match ty.as_struct_by_deref(&self.fn_data.ty_env) {
            Some(it) => it,
            None => return,
        };

        let field_opt = k_struct
            .of(&self.mod_outline.structs)
            .fields
            .iter()
            .copied()
            .find(|field| field.name(&self.mod_outline.fields) == field_name);
        let field = match field_opt {
            Some(it) => it,
            _ => return,
        };

        self.occurrences.push((MOD, field, loc));
    }

    fn on_node(&mut self, node: &KNode) {
        self.do_on_node(node);

        for cont in &node.conts {
            self.on_node(cont);
        }
    }
}

fn collect_field_occurrences(
    only_doc: Option<Doc>,
    ls: &mut LangService,
    occurrences: &mut Vec<(KMod, KField, Loc)>,
) {
    ls.request_types();

    for mod_data in ls.mods.iter() {
        for fn_data in mod_data.fns.iter() {
            let mut collector = FieldOccurrenceInFnCollector {
                k_mod: MOD,
                mod_outline: &ls.mod_outlines[MOD],
                // FIXME: 正しい mod_outlines を渡す
                mod_outlines: &ls.mod_outlines,
                fn_data,
                occurrences,
            };

            for label_data in fn_data.labels.iter() {
                collector.on_node(&label_data.body);
            }
        }
    }
}

fn hit_test_on_field(doc: Doc, pos: TPos16, ls: &mut LangService) -> Option<PToken> {
    let syntax = ls.request_syntax(doc)?;

    for expr in syntax.tree.ast.exprs().iter() {
        match expr {
            AExpr::Field(AFieldExpr {
                field_opt: Some(field),
                ..
            }) => {
                let range = to_range16(field.range(&syntax.tree.tokens));
                if !range.contains_inclusive(pos) {
                    continue;
                }

                return Some(*field);
            }
            // AExpr::Record(..) => {}
            _ => continue,
        }
    }

    None
}

fn document_highlight_of_fields(
    doc: Doc,
    token: PToken,
    ls: &mut LangService,
) -> Option<(Vec<TRange>, Vec<TRange>)> {
    let mut occurrences = vec![];
    collect_field_occurrences(Some(doc), ls, &mut occurrences);

    let syntax = ls.request_syntax(doc)?;
    let range = token.range(&syntax.tree.tokens);

    let pair = occurrences.iter().find_map(|&(the_mod, k_field, loc)| {
        let (the_doc, loc) = loc.inner().ok()?;
        if the_mod != MOD || the_doc != doc {
            return None;
        }

        let the_range = loc.range(&syntax.tree).ok()?;
        if the_range != range {
            return None;
        }

        Some((MOD, k_field))
    })?;

    let locations = occurrences
        .iter()
        .filter(|&&(the_mod, the_field, _)| (the_mod, the_field) == pair)
        .map(|&(_, _, loc)| loc)
        .filter_map(|loc| loc_to_range(loc, &syntax.tree))
        .collect();

    // FIXME: 定義箇所もハイライトする
    Some((vec![], locations))
}

pub(crate) fn document_highlight(
    doc: Doc,
    pos: TPos16,
    ls: &mut LangService,
) -> Option<(Vec<TRange>, Vec<TRange>)> {
    // if let Some(token) = hit_test_on_field(doc, pos, ls) {
    //     return document_highlight_of_fields(doc, token, ls);
    // }

    // let DocContentAnalysisMut {
    //     syntax,
    //     symbols,
    //     cps,
    // } = ls.request_cps(doc)?;

    // let (name, _) = hit_test(doc, pos, syntax, symbols, cps)?;
    // let mut locations = vec![];

    // collect_def_sites(doc, name, syntax, symbols, cps, &mut locations);
    // let def_sites = locations
    //     .drain(..)
    //     .map(|location| location.range())
    //     .collect();

    // collect_use_sites(doc, name, syntax, symbols, cps, &mut locations);
    // let use_sites = locations
    //     .drain(..)
    //     .map(|location| location.range())
    //     .collect();

    // Some((def_sites, use_sites))
    None
}
