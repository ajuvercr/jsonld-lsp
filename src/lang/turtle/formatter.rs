use lsp_types::FormattingOptions;

use super::token::Token;

pub fn format(tokens: &[&Token], options: FormattingOptions) -> String {
    let mut indent_str = String::new();
    for _ in 0..options.tab_size {
        indent_str += " ";
    }

    let mut indent = 0;
    let mut document = String::new();
    let mut line = String::new();
    let mut wants_newline = 0;
    let mut needs_new_line = false;

    let mut listings = vec![false];
    let mut first = true;

    let mut last_open_bnode = false;

    for token in tokens {
        if last_open_bnode && token.is_b_node_end() {
            wants_newline = 0;
        }

        let space = match token {
            Token::Stop | Token::ObjectSplit | Token::PredicateSplit => false,
            Token::DataTypeDelim | Token::LangTag(_) => false,
            _ => true,
        };

        if wants_newline == 0 && space && !first {
            line += " ";
        }

        if needs_new_line || (wants_newline > 0 && !token.is_comment()) {
            document += "\n";
            document += &line;
            line = String::new();

            for _ in 1..wants_newline {
                line += "\n";
            }

            for _ in 0..indent {
                line += &indent_str;
            }

            if listings.last().copied().unwrap_or_default() {
                line += &indent_str;
            }

            wants_newline = 0;
            needs_new_line = false;
        }

        last_open_bnode = token.is_b_node_start();

        match token {
            Token::PrefixTag => line += "@prefix",
            Token::BaseTag => line += "@base",
            Token::SparqlPrefix => line += "PREFIX",
            Token::SparqlBase => line += "BASE",
            Token::PredType => line += "a",
            Token::BNodeStart => {
                line += "[";
                indent += 1;
                wants_newline = 1;
                listings.push(false);
            }
            Token::BNodeEnd => {
                line += "]";
                indent -= 1;
                listings.pop();
            }
            Token::ColStart => line += "(",
            Token::ColEnd => line += ")",
            Token::DataTypeDelim => {
                line += "^^";
            }
            Token::Stop => {
                line += ".";
                wants_newline = 1;
                if let Some(l) = listings.last_mut() {
                    if *l {
                        wants_newline = 2;
                    }
                    *l = false;
                }
            }
            Token::PredicateSplit => {
                line += ";";
                wants_newline = 1;
                if let Some(l) = listings.last_mut() {
                    *l = true;
                }
            }
            Token::ObjectSplit => line += ",",
            Token::True => line += "true",
            Token::False => line += "false",
            Token::IRIRef(x) => {
                line += "<";
                line += x.as_str();
                line += ">";
            }
            Token::PNameNS(x) => {
                if let Some(x) = x {
                    line += x.as_str();
                }
                line += ":";
            }
            Token::PNameLN(x, y) => {
                if let Some(x) = x {
                    line += x.as_str();
                }
                line += ":";
                line += y.as_str();
            }
            Token::BlankNodeLabel(x) => {
                line += "_:";
                line += x.as_str();
            }
            Token::LangTag(x) => {
                line += "@";
                line += x.as_str();
            }
            Token::Number(x) => line += x,
            Token::Str(x, y) => {
                line += y.quote();
                line += x;
                line += y.quote();
            }
            Token::ANON => line += "[]",
            Token::Comment(x) => {
                line += "#";
                line += x;
                needs_new_line = true;
            }
        }

        first = false;
    }

    document += "\n";
    document += &line;
    document + "\n"
}
