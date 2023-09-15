use crate::{
    lang::{Lang, Node, Token},
    lsp_types::{SemanticToken, SemanticTokenType},
    model::{spanned, Spanned},
    parent::ParentingSystem,
};
use ropey::Rope;

struct T {
    start: usize,
    length: usize,
    ty: usize,
}

pub fn semantic_tokens<L: Lang>(
    lang: &L,
    system: &ParentingSystem<Spanned<L::Node>>,
    rope: &Rope,
) -> Vec<SemanticToken> {
    let mut tokens: Vec<Option<SemanticTokenType>> = Vec::with_capacity(rope.len_chars());
    tokens.resize(rope.len_chars(), None);

    lang.semantic_tokens(system, |token, ty| {
        token.for_each(|i| tokens[i] = Some(ty.clone()));
    });

    system
        .iter()
        .flat_map(|(_, x)| x.leaf().map(|t| spanned(t, x.span().clone())))
        .flat_map(|s| s.value().token().map(|t| (s.span().clone(), t)))
        .for_each(|(range, ty)| {
            if range.clone().all(|i| tokens[i].is_none()) {
                range.for_each(|i| tokens[i] = Some(ty.clone()));
            }
        });

    let mut last = None;
    let mut start = 0;
    let mut out_tokens = Vec::new();
    for (i, ty) in tokens.into_iter().enumerate() {
        if last != ty {
            if let Some(t) = last {
                log::error!("sem tokens {} - {} {:?}", start, i, t);
                out_tokens.push(T {
                    start,
                    length: i - start,
                    ty: L::LEGEND_TYPES.iter().position(|x| x == &t).unwrap_or(0),
                });
            }

            last = ty;
            start = i;
        }
    }

    if let Some(t) = last {
        log::error!("sem tokens {} - end {:?}", start, t);
        out_tokens.push(T {
            start,
            length: rope.len_chars() - start,
            ty: L::LEGEND_TYPES.iter().position(|x| x == &t).unwrap_or(0),
        });
    }

    let mut pre_line = 0;
    let mut pre_start = 0;

    out_tokens
        .into_iter()
        .flat_map(|token| {
            let line = rope.try_byte_to_line(token.start as usize).ok()? as u32;
            let first = rope.try_line_to_char(line as usize).ok()? as u32;
            let start = rope.try_byte_to_char(token.start as usize).ok()? as u32 - first;
            let delta_line = line - pre_line;
            let delta_start = if delta_line == 0 {
                start - pre_start
            } else {
                start
            };
            let ret = Some(SemanticToken {
                delta_line,
                delta_start,
                length: token.length as u32,
                token_type: token.ty as u32,
                token_modifiers_bitset: 0,
            });
            pre_line = line;
            pre_start = start;
            ret
        })
        .collect()
}
