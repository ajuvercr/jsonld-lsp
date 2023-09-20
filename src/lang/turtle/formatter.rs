use std::{
    io::{self, Cursor, Write},
    ops::Range,
};

use lsp_types::FormattingOptions;
use ropey::Rope;

use crate::model::{spanned, Spanned};

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
struct FormatState<'a> {
    indent_level: usize,
    indent: String,
    buf: Buf,
    line_start: u64,
    comments: &'a [Spanned<String>],
    comments_idx: usize,
    tail: Spanned<String>,
    line_count: usize,
}

impl<'a> FormatState<'a> {
    fn new(
        options: FormattingOptions,
        buf: Buf,
        comments: &'a [Spanned<String>],
        source: &'a Rope,
    ) -> Self {
        let mut indent = String::new();
        for _ in 0..options.tab_size {
            indent.push(' ');
        }

        let tail = spanned(
            String::new(),
            source.len_chars() + 1..source.len_chars() + 1,
        );
        Self {
            tail,
            line_start: 0,
            indent_level: 0,
            indent,
            buf,
            comments,
            comments_idx: 0,
            line_count: 0,
        }
    }

    fn check_comments(&mut self, span: &Range<usize>) -> io::Result<bool> {
        let mut first = true;
        loop {
            let current = self.comments.get(self.comments_idx).unwrap_or(&self.tail);

            if current.1.start > span.start {
                break;
            }

            first = false;
            write!(self.buf, "#{}", current.0)?;
            self.new_line()?;
            self.comments_idx += 1;
        }
        Ok(!first)
    }
    fn current_line_length(&self) -> u64 {
        self.buf.position() - self.line_start
    }
    fn new_line(&mut self) -> io::Result<()> {
        self.line_count += 1;
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

impl FormatState<'_> {
    fn write_turtle(&mut self, turtle: &Turtle) -> io::Result<()> {
        if let Some(ref b) = turtle.base {
            self.check_comments(&b.1)?;
            self.write_base(b)?;
            self.new_line()?;
        }
        for p in &turtle.prefixes {
            self.check_comments(&p.1)?;
            self.write_prefix(p)?;
            self.new_line()?;
        }

        let mut prev_line = 0;

        for t in &turtle.triples {
            if prev_line + 1 < self.line_count {
                self.new_line()?;
            }
            prev_line = self.line_count;
            self.check_comments(&t.1)?;
            self.write_triple(&t)?;
            self.new_line()?;
            // request_newline = t.0.po.len() > 1 || t.0.po[0].0.object.len() > 1;
        }
        self.new_line()?;

        for i in self.comments_idx..self.comments.len() {
            write!(self.buf, "#{}", self.comments[i].0)?;
            self.new_line()?;
        }

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
                let should_skip = if is_first_of_line {
                    write!(self.buf, " ")?;
                    self.write_po(&pos[0])?;
                    write!(self.buf, ";")?;
                    1
                } else {
                    0
                };
                for po in pos.iter().skip(should_skip) {
                    self.new_line()?;
                    self.check_comments(&po.1)?;
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

    fn write_collection(&mut self, coll: &Vec<Spanned<Term>>) -> io::Result<()> {
        if coll.is_empty() {
            return write!(self.buf, "( )");
        }

        let mut should_indent = false;
        let start = self.buf.position();
        let current_line = self.line_count;

        write!(self.buf, "( ")?;

        self.check_comments(&coll[0].1)?;
        self.write_term(&coll[0])?;

        for po in coll.iter().skip(1) {
            self.check_comments(&po.1)?;
            write!(self.buf, " ")?;
            self.write_term(&po)?;
            if self.current_line_length() > 80 {
                should_indent = true;
                break;
            }
        }
        write!(self.buf, " )")?;

        if should_indent {
            self.buf.set_position(start);
            self.line_count = current_line;
            write!(self.buf, "(")?;
            self.inc();
            for po in coll.iter() {
                self.new_line()?;
                self.check_comments(&po.1)?;
                self.write_term(&po)?;
            }
            self.decr();
            self.new_line()?;
            write!(self.buf, ")")?;
        }

        Ok(())
    }

    fn write_term(&mut self, term: &Term) -> io::Result<()> {
        match term {
            Term::Literal(s) => write!(self.buf, "{}", s)?,
            Term::BlankNode(b) => self.write_bnode(b)?,
            Term::NamedNode(n) => write!(self.buf, "{}", n)?,
            Term::Collection(ts) => self.write_collection(ts)?,
            Term::Invalid => return Err(io::Error::new(io::ErrorKind::Other, "")),
        }
        Ok(())
    }

    fn write_po(&mut self, po: &PO) -> io::Result<()> {
        write!(self.buf, "{} ", po.predicate.0)?;
        self.write_term(&po.object[0])?;
        let mut should_indent = false;

        let start = self.buf.position();
        let current_line = self.line_count;
        for i in 1..po.object.len() {
            write!(self.buf, ", ")?;
            self.write_term(&po.object[i])?;

            if self.current_line_length() > 80 {
                should_indent = true;
                break;
            }
        }

        if should_indent {
            self.buf.set_position(start);
            self.line_count = current_line;
            self.inc();
            for i in 1..po.object.len() {
                write!(self.buf, ",")?;
                self.new_line()?;
                self.check_comments(&po.object[i].1)?;
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
        self.check_comments(&triple.po[1].1)?;
        self.write_po(&triple.po[1])?;

        if triple.po.len() == 2 {
            self.decr();
            write!(self.buf, ".")?;
            return Ok(());
        }

        for i in 2..triple.po.len() {
            write!(self.buf, ";")?;
            self.new_line()?;
            self.check_comments(&triple.po[i].1)?;
            self.write_po(&triple.po[i])?;
        }

        write!(self.buf, ".")?;
        self.decr();
        Ok(())
    }
}

pub fn format_turtle(
    turtle: &Turtle,
    config: FormattingOptions,
    comments: &[Spanned<String>],
    source: &Rope,
) -> Option<String> {
    let buf: Buf = Cursor::new(Vec::new());
    let mut state = FormatState::new(config, buf, comments, source);
    state.write_turtle(turtle).ok()?;
    String::from_utf8(state.buf.into_inner()).ok()
}

#[cfg(test)]
mod tests {

    use chumsky::{Parser, Stream};
    use ropey::Rope;

    use crate::{
        lang::turtle::{formatter::format_turtle, parser::turtle, tokenizer, Turtle},
        model::{spanned, Spanned},
    };

    #[derive(Debug)]
    pub enum Err {
        Tokenizing,
        Parsing,
    }

    fn parse_turtle(inp: &str) -> Result<(Turtle, Vec<Spanned<String>>), Err> {
        let tokens = tokenizer::parse_tokens().parse(inp).map_err(|err| {
            println!("Token error {:?}", err);
            Err::Tokenizing
        })?;
        let end = inp.len() - 1..inp.len() + 1;

        let mut comments: Vec<_> = tokens
            .iter()
            .filter(|x| x.0.is_comment())
            .cloned()
            .map(|x| spanned(x.0.to_comment(), x.1))
            .collect();
        comments.sort_by_key(|x| x.1.start);

        let stream = Stream::from_iter(end, tokens.into_iter().filter(|x| !x.0.is_comment()));

        turtle()
            .parse(stream)
            .map_err(|err| {
                println!("Parse error {:?}", err);
                Err::Parsing
            })
            .map(|t| (t, comments))
    }

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
        let (output, comments) = parse_turtle(txt).expect("Simple");
        let formatted = format_turtle(
            &output,
            lsp_types::FormattingOptions {
                tab_size: 2,
                ..Default::default()
            },
            &comments,
            &Rope::from_str(txt),
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
        let (output, comments) = parse_turtle(txt).expect("Simple");
        let formatted = format_turtle(
            &output,
            lsp_types::FormattingOptions {
                tab_size: 2,
                ..Default::default()
            },
            &comments,
            &Rope::from_str(txt),
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
        let (output, comments) = parse_turtle(txt).expect("Simple");
        let formatted = format_turtle(
            &output,
            lsp_types::FormattingOptions {
                tab_size: 2,
                ..Default::default()
            },
            &comments,
            &Rope::from_str(txt),
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
        let (output, comments) = parse_turtle(txt).expect("Simple");
        let formatted = format_turtle(
            &output,
            lsp_types::FormattingOptions {
                tab_size: 2,
                ..Default::default()
            },
            &comments,
            &Rope::from_str(txt),
        )
        .expect("formatting");
        assert_eq!(formatted, expected);
    }

    #[test]
    fn short_collection() {
        let txt = r#"
        <abc> a (), (<abc> <def>).
"#;

        let expected = r#"<abc> a ( ), ( <abc> <def> ).

"#;
        let (output, comments) = parse_turtle(txt).expect("Simple");
        let formatted = format_turtle(
            &output,
            lsp_types::FormattingOptions {
                tab_size: 2,
                ..Default::default()
            },
            &comments,
            &Rope::from_str(txt),
        )
        .expect("formatting");
        assert_eq!(formatted, expected);
    }

    #[test]
    fn long_collection() {
        let txt = r#"
        <abc> a (), (<somevery very very long item> <and othersss> <and ottteeehs> <wheeeeeeeeeeeee>).
"#;

        let expected = r#"<abc> a ( ), (
  <somevery very very long item>
  <and othersss>
  <and ottteeehs>
  <wheeeeeeeeeeeee>
).

"#;
        let (output, comments) = parse_turtle(txt).expect("Simple");
        let formatted = format_turtle(
            &output,
            lsp_types::FormattingOptions {
                tab_size: 2,
                ..Default::default()
            },
            &comments,
            &Rope::from_str(txt),
        )
        .expect("formatting");
        assert_eq!(formatted, expected);
    }

    #[test]
    fn easy_comments() {
        let txt = r#"
# Test this is a cool test or something!
            # Another comment!

[] a foaf:Name;
   foaf:knows <abc>; foaf:knows2 <abc>.

"#;

        let expected = r#"# Test this is a cool test or something!
# Another comment!
[ ] a foaf:Name;
  foaf:knows <abc>;
  foaf:knows2 <abc>.

"#;
        let (output, comments) = parse_turtle(txt).expect("Simple");
        let formatted = format_turtle(
            &output,
            lsp_types::FormattingOptions {
                tab_size: 2,
                ..Default::default()
            },
            &comments,
            &Rope::from_str(txt),
        )
        .expect("formatting");
        assert_eq!(formatted, expected);
    }

    #[test]
    fn hard_comments() {
        let txt = r#"

[] a foaf:Name; # Nested comment
   foaf:knows <abc>;     # Another comment!
   foaf:knows2 <abc>.

   #trailing comments
"#;

        let expected = r#"[ ] a foaf:Name;
  # Nested comment
  foaf:knows <abc>;
  # Another comment!
  foaf:knows2 <abc>.

#trailing comments
"#;
        let (output, comments) = parse_turtle(txt).expect("Simple");
        let formatted = format_turtle(
            &output,
            lsp_types::FormattingOptions {
                tab_size: 2,
                ..Default::default()
            },
            &comments,
            &Rope::from_str(txt),
        )
        .expect("formatting");
        assert_eq!(formatted, expected);
    }

    #[test]
    fn bug_1() {
        let txt = r#"
[] a sh:NodeShape;
  sh:targetClass js:Echo;
  sh:property [
    sh:class :ReaderChannel;
    sh:path js:input;
    sh:name "Input Channel"
  ], [
    sh:class :WriterChannel;
    sh:path js:output;
    sh:name "Output Channel"
  ].

"#;

        let expected = r#"[ ] a sh:NodeShape;
  sh:targetClass js:Echo;
  sh:property [
    sh:class :ReaderChannel;
    sh:path js:input;
    sh:name "Input Channel";
  ], [
    sh:class :WriterChannel;
    sh:path js:output;
    sh:name "Output Channel";
  ].

"#;

        let (output, comments) = parse_turtle(txt).expect("Simple");
        let formatted = format_turtle(
            &output,
            lsp_types::FormattingOptions {
                tab_size: 2,
                ..Default::default()
            },
            &comments,
            &Rope::from_str(txt),
        )
        .expect("formatting");
        assert_eq!(formatted, expected);
    }
}
