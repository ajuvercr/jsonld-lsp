use std::{
    env, fs,
};

use jsonld_language_server::parser::parse;


fn main() {
    let src = fs::read_to_string(env::args().nth(1).expect("Expected file argument"))
        .expect("Failed to read file");

    let (json, errs) = parse(&src);
    println!("{:#?}", json);
    for e in errs {
        println!("> {}", e.msg);
    }
}
