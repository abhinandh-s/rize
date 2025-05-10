use rize_syntax::*;

fn main() {
    let source = r#"
        let name: string = "Abhi";
        let name2: string = "Abhi2";
        let name3: string = "Abhi3";
    "#;

    let tokens = lex(source);
    let cst = parse_tokens_to_cst(&tokens);
    let ast = lower_to_ast(&cst);

    println!("CST: {:#?}", cst);
    println!("AST: {:#?}", ast);

    analyze(&ast);
    let compiled = compile(&ast);
    println!("Compiled Output:\n{}", compiled);
}

