use std::io::{self, Cursor, Write};

use lsp_types::FormattingOptions;

use super::{token::Token, Base, BlankNode, Prefix, Term, Triple, Turtle, PO};

#[allow(unused)]
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

type Buf = Cursor<Vec<u8>>;
struct FormatState {
    indent_level: usize,
    indent: String,
    buf: Buf,
    line_start: u64,
}
impl FormatState {
    fn new(options: FormattingOptions, buf: Buf) -> Self {
        let mut indent = String::new();
        for _ in 0..options.tab_size {
            indent.push(' ');
        }
        Self {
            line_start: 0,
            indent_level: 0,
            indent,
            buf,
        }
    }
    fn current_line_length(&self) -> u64 {
        self.buf.position() - self.line_start
    }
    fn new_line(&mut self) -> io::Result<()> {
        write!(self.buf, "\n")?;
        self.line_start = self.buf.position();
        for _ in 0..self.indent_level {
            write!(self.buf, "{}", &self.indent)?;
        }
        Ok(())
    }
    fn inc(&mut self) {
        self.indent_level += 1;
    }
    fn decr(&mut self) {
        self.indent_level -= 1;
    }
}

impl FormatState {
    fn write_turtle(&mut self, turtle: &Turtle) -> io::Result<()> {
        if let Some(ref b) = turtle.base {
            self.write_base(b)?;
            self.new_line()?;
        }
        for p in &turtle.prefixes {
            self.write_prefix(p)?;
            self.new_line()?;
        }

        let mut request_newline = turtle.base.is_some() || !turtle.prefixes.is_empty();

        for t in &turtle.triples {
            if request_newline {
                self.new_line()?;
            }
            self.write_triple(&t)?;
            self.new_line()?;
            request_newline = t.0.po.len() > 1;
        }
        self.new_line()?;

        Ok(())
    }
    fn write_prefix(&mut self, prefix: &Prefix) -> io::Result<()> {
        write!(self.buf, "@prefix {}: {}.", prefix.prefix.0, prefix.value.0)
    }
    fn write_base(&mut self, base: &Base) -> io::Result<()> {
        write!(self.buf, "@base {}.", base.1 .0)
    }
    fn write_bnode(&mut self, bnode: &BlankNode) -> io::Result<()> {
        match bnode {
            BlankNode::Named(x) => write!(self.buf, "_:{}", x)?,
            BlankNode::Unnamed(pos) => {
                if pos.len() == 0 {
                    return write!(self.buf, "[ ]");
                }
                if pos.len() == 1 {
                    write!(self.buf, "[ ")?;
                    self.write_po(&pos[0])?;
                    return write!(self.buf, " ]");
                }
                let is_first_of_line = self.current_line_length() == 0;
                self.inc();
                write!(self.buf, "[")?;
                if is_first_of_line {
                    write!(self.buf, " ")?;
                    self.write_po(&pos[0])?;
                    write!(self.buf, ";")?;
                } else {
                    self.new_line()?;
                    self.write_po(&pos[0])?;
                    write!(self.buf, ";")?;
                }
                for po in pos.iter().skip(1) {
                    self.new_line()?;
                    self.write_po(&po)?;
                    write!(self.buf, ";")?;
                }
                self.decr();
                self.new_line()?;
                write!(self.buf, "]")?;
            }
            BlankNode::Invalid => return Err(io::Error::new(io::ErrorKind::Other, "")),
        }
        Ok(())
    }

    fn write_term(&mut self, term: &Term) -> io::Result<()> {
        match term {
            Term::Literal(s) => write!(self.buf, "{}", s)?,
            Term::BlankNode(b) => self.write_bnode(b)?,
            Term::NamedNode(n) => write!(self.buf, "{}", n)?,
            Term::Invalid => return Err(io::Error::new(io::ErrorKind::Other, "")),
        }
        Ok(())
    }

    fn write_po(&mut self, po: &PO) -> io::Result<()> {
        write!(self.buf, "{} ", po.predicate.0)?;
        self.write_term(&po.object[0])?;
        let mut should_indent = false;

        let start = self.buf.position();
        for i in 1..po.object.len() {
            write!(self.buf, ", ")?;
            self.write_term(&po.object[i])?;

            println!("current line length {}", self.current_line_length());
            if self.current_line_length() > 80 {
                should_indent = true;
                break;
            }
        }
        self.buf.set_position(start);
        if should_indent {
            self.inc();
            for i in 1..po.object.len() {
                write!(self.buf, ",")?;
                self.new_line()?;
                self.write_term(&po.object[i])?;
            }
            self.decr();
        }

        Ok(())
    }

    fn write_triple(&mut self, triple: &Triple) -> io::Result<()> {
        match &triple.subject.0 {
            super::Subject::BlankNode(bn) => self.write_bnode(bn)?,
            super::Subject::NamedNode(n) => write!(self.buf, "{}", n)?,
        }
        write!(self.buf, " ")?;
        self.write_po(&triple.po[0])?;
        if triple.po.len() == 1 {
            write!(self.buf, ".")?;
            return Ok(());
        }
        write!(self.buf, ";")?;
        self.inc();
        self.new_line()?;
        self.write_po(&triple.po[1])?;

        if triple.po.len() == 2 {
            self.decr();
            write!(self.buf, ".")?;
            return Ok(());
        }

        for i in 2..triple.po.len() {
            write!(self.buf, ";")?;
            self.new_line()?;
            self.write_po(&triple.po[i])?;
        }

        write!(self.buf, ".")?;
        self.decr();
        Ok(())
    }
}

pub fn format_turtle(turtle: &Turtle, config: FormattingOptions) -> Option<String> {
    let buf: Buf = Cursor::new(Vec::new());
    let mut state = FormatState::new(config, buf);
    state.write_turtle(turtle).ok()?;
    String::from_utf8(state.buf.into_inner()).ok()
}

#[cfg(test)]
mod tests {
    use crate::lang::turtle::{formatter::format_turtle, turtle, turtle_tests::parse_it};

    #[test]
    fn easy_format() {
        let txt = r#"
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@base <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

[] a foaf:Name;
   foaf:knows <abc>;.
"#;

        let expected = r#"@base <http://www.w3.org/1999/02/22-rdf-syntax-ns#>.
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>.
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>.

[ ] a foaf:Name;
  foaf:knows <abc>.

"#;
        let output = parse_it(txt, turtle()).expect("Simple");
        let formatted = format_turtle(
            &output,
            lsp_types::FormattingOptions {
                tab_size: 2,
                ..Default::default()
            },
        )
        .expect("formatting");
        assert_eq!(formatted, expected);
    }

    #[test]
    fn harder_format_pos() {
        let txt = r#"
[] a foaf:Name;
   foaf:knows <abc>; foaf:knows2 <abc>.

"#;

        let expected = r#"[ ] a foaf:Name;
  foaf:knows <abc>;
  foaf:knows2 <abc>.

"#;
        let output = parse_it(txt, turtle()).expect("Simple");
        let formatted = format_turtle(
            &output,
            lsp_types::FormattingOptions {
                tab_size: 2,
                ..Default::default()
            },
        )
        .expect("formatting");
        assert_eq!(formatted, expected);
    }

    #[test]
    fn format_blanknodes() {
        let txt = r#"
        [ <a> foaf:Person; foaf:knows <abc>; foaf:knows <def> ] foaf:knows [
        a foaf:Person;
        foaf:knows <abc>;
        foaf:knows <def>;
        ] .

"#;

        let expected = r#"[ <a> foaf:Person;
  foaf:knows <abc>;
  foaf:knows <def>;
] foaf:knows [
  a foaf:Person;
  foaf:knows <abc>;
  foaf:knows <def>;
].

"#;
        let output = parse_it(txt, turtle()).expect("Simple");
        let formatted = format_turtle(
            &output,
            lsp_types::FormattingOptions {
                tab_size: 2,
                ..Default::default()
            },
        )
        .expect("formatting");
        assert_eq!(formatted, expected);
    }

    #[test]
    fn long_objectlist() {
        let txt = r#"
        <abc> a <something long>, <something longer still>, <something longer>, <something tes>, <soemthing eeeellssee>.
"#;

        let expected = r#"<abc> a <something long>,
  <something longer still>,
  <something longer>,
  <something tes>,
  <soemthing eeeellssee>.

"#;
        let output = parse_it(txt, turtle()).expect("Simple");
        let formatted = format_turtle(
            &output,
            lsp_types::FormattingOptions {
                tab_size: 2,
                ..Default::default()
            },
        )
        .expect("formatting");
        assert_eq!(formatted, expected);
    }
}
