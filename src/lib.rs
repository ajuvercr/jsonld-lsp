pub mod chumsky;
pub mod completion;
pub mod jump_definition;
pub mod reference;
pub mod semantic_token;

#[cfg(test)]
mod tests {
    #[test]
    fn test_1() {
        let spanned: json_spanned_value::spanned::Object = json_spanned_value::from_str(
            r#"
    {

        "teten": 32,
        "teten2": "32",
        "boo": {
            "yes": "more"
        }
    }
        "#,
        )
        .unwrap();

        println!("{:?}", spanned);
        println!("{:?}", spanned.span());

        for key in spanned.keys() {
            println!(
                "{}: {:?} {}",
                key.as_str(),
                key.span(),
                key.end() - key.start()
            );
            let obj = spanned.get(key.as_str()).unwrap();
            println!("  {:?} {}", obj.span(), obj.end() - obj.start());
        }

        assert!(false);
    }
}
