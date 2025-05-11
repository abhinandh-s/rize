#![allow(clippy::unwrap_used)]

mod kind;
use std::sync::Arc;

pub use kind::SyntaxKind;

use anyhow::Error;
use tower_lsp::lsp_types::{
    SemanticToken, SemanticTokenType, SemanticTokens, SemanticTokensResult,
};

pub type Token = Arc<TokenData>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenData {
    pub kind: SyntaxKind,
    pub text: String,
}

#[derive(Debug, Clone)]
pub enum SyntaxElement {
    Token(Token),
    Node(SyntaxNode),
}

pub type SyntaxNode = Arc<SyntaxNodeData>;

#[derive(Debug, Clone)]
pub struct SyntaxNodeData {
    pub kind: SyntaxKind,
    pub children: Vec<SyntaxElement>,
}

impl SyntaxNodeData {
    pub fn new(kind: SyntaxKind, children: Vec<SyntaxElement>) -> Self {
        SyntaxNodeData { kind, children }
    }

    pub fn tokens(&self) -> Vec<&Token> {
        self.children
            .iter()
            .filter_map(|el| match el {
                SyntaxElement::Token(tok) => Some(tok),
                _ => None,
            })
            .collect()
    }

    pub fn child_nodes(&self) -> Vec<&SyntaxNode> {
        self.children
            .iter()
            .filter_map(|el| match el {
                SyntaxElement::Node(n) => Some(n),
                _ => None,
            })
            .collect()
    }

    pub fn kind(&self) -> SyntaxKind {
        self.kind
    }
}

pub fn lex(source: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut chars = source.chars().peekable();

    while let Some(&ch) = chars.peek() {
        match ch {
            c if c.is_whitespace() => {
                tokens.push(Token::new(TokenData {
                    kind: SyntaxKind::Whitespace,
                    text: " ".into(),
                }));
                chars.next();
            }
            ':' => {
                tokens.push(Token::new(TokenData {
                    kind: SyntaxKind::Colon,
                    text: ":".into(),
                }));
                chars.next();
            }
            '\n' => {
                tokens.push(Token::new(TokenData {
                    kind: SyntaxKind::NewLine,
                    text: ":".into(),
                }));
                chars.next();
            }
            '=' => {
                tokens.push(Token::new(TokenData {
                    kind: SyntaxKind::Equal,
                    text: "=".into(),
                }));
                chars.next();
            }
            ';' => {
                tokens.push(Token::new(TokenData {
                    kind: SyntaxKind::Semicolon,
                    text: ";".into(),
                }));
                chars.next();
            }
            '"' => {
                chars.next();
                let mut value = String::new();
                while let Some(&c) = chars.peek() {
                    if c == '"' {
                        chars.next();
                        break;
                    }
                    value.push(c);
                    chars.next();
                }
                tokens.push(Token::new(TokenData {
                    kind: SyntaxKind::StringLiteral,
                    text: value,
                }));
            }
            c if c.is_alphabetic() => {
                let mut ident = String::new();
                while let Some(&c) = chars.peek() {
                    if c.is_alphanumeric() || c == '_' {
                        ident.push(c);
                        chars.next();
                    } else {
                        break;
                    }
                }
                let kind = match ident.as_str() {
                    "let" => SyntaxKind::Let,
                    "string" => SyntaxKind::Type,
                    _ => SyntaxKind::Ident,
                };
                tokens.push(Token::new(TokenData { kind, text: ident }));
            }
            _ => {
                tokens.push(Token::new(TokenData {
                    kind: SyntaxKind::Error,
                    text: ch.to_string(),
                }));
                chars.next();
            }
        }
    }

    tokens
}

pub fn parse_tokens_to_cst(tokens: &[Token]) -> SyntaxNode {
    let mut i = 0;
    let mut decls = Vec::new();

    while i < tokens.len() {
        if tokens.get(i).map(|t| &t.kind) != Some(&SyntaxKind::Let) {
            break;
        }

        let mut children = Vec::new();

        children.push(SyntaxElement::Token(tokens[i].clone())); // let
        i += 1;

        if let Some(tok) = tokens.get(i) {
            if tok.kind == SyntaxKind::Ident {
                children.push(SyntaxElement::Token(tok.clone()));
                i += 1;
            }
        }

        if let Some(tok) = tokens.get(i) {
            if tok.kind == SyntaxKind::Colon {
                children.push(SyntaxElement::Token(tok.clone()));
                i += 1;
            }
        }

        if let Some(tok) = tokens.get(i) {
            if tok.kind == SyntaxKind::Type {
                children.push(SyntaxElement::Token(tok.clone()));
                i += 1;
            }
        }

        if let Some(tok) = tokens.get(i) {
            if tok.kind == SyntaxKind::Equal {
                children.push(SyntaxElement::Token(tok.clone()));
                i += 1;
            }
        }

        if let Some(tok) = tokens.get(i) {
            if tok.kind == SyntaxKind::StringLiteral {
                children.push(SyntaxElement::Token(tok.clone()));
                i += 1;
            }
        }

        if let Some(tok) = tokens.get(i) {
            if tok.kind == SyntaxKind::Semicolon {
                children.push(SyntaxElement::Token(tok.clone()));
                i += 1;
            }
        }

        decls.push(SyntaxElement::Node(
            SyntaxNodeData {
                kind: SyntaxKind::VarDecl,
                children,
            }.into(),
        ));
    }

    SyntaxNodeData::new(SyntaxKind::Root, decls).into()
}

#[derive(Debug)]
pub struct VarDecl {
    pub name: String,
    pub ty: String,
    pub value: String,
}

pub fn lower_to_ast(root: &SyntaxNode) -> Vec<VarDecl> {
    let mut decls = Vec::new();
    for node in root.child_nodes() {
        if node.kind() != SyntaxKind::VarDecl {
            continue;
        }

        let tokens = node.tokens();
        let name = tokens
            .iter()
            .find(|t| t.kind == SyntaxKind::Ident)
            .unwrap()
            .text
            .clone();
        let ty = tokens
            .iter()
            .find(|t| t.kind == SyntaxKind::Type)
            .unwrap()
            .text
            .clone();
        let value = tokens
            .iter()
            .find(|t| t.kind == SyntaxKind::StringLiteral)
            .unwrap()
            .text
            .clone();

        decls.push(VarDecl { name, ty, value });
    }

    decls
}

pub fn analyze(decls: &[VarDecl]) {
    for decl in decls {
        if decl.ty != "string" {
            println!("Error: Unsupported type '{}'", decl.ty);
        }
        if decl.value.is_empty() {
            println!("Warning: Empty string for '{}'", decl.name);
        }
    }
}

pub fn compile(decls: &[VarDecl]) -> String {
    let mut out = String::from("{\n");
    for d in decls {
        out.push_str(&format!("  \"{}\": \"{}\",\n", d.name, d.value));
    }
    out.push('}');
    out
}

pub fn semantic_tokens_full(text: &str) -> Result<Option<SemanticTokensResult>, tower_lsp::jsonrpc::Error> {
    let tokens = lex(text); // Token { kind, text }
    let mut semantic_tokens = vec![];

    let mut char_offset = 0;
    let mut prev_line = 0;
    let mut prev_start_char = 0;

    for token in tokens {
        let token_start = char_offset;
        let token_len = token.text.chars().count();
        char_offset += token_len;

        // Map byte offset to line and character position
        let prefix = &text[..token_start];
        let token_line = prefix.lines().count() - 1;
        let token_col = prefix.lines().last().map_or(0, |l| l.len());

        // Skip unknown tokens
        let kind = match token.kind {
            SyntaxKind::Let => SemanticTokenType::KEYWORD,
            SyntaxKind::Ident => SemanticTokenType::VARIABLE,
            SyntaxKind::Type => SemanticTokenType::TYPE,
            SyntaxKind::StringLiteral => SemanticTokenType::STRING,
            _ => continue,
        };

        let delta_line = token_line - prev_line;
        let delta_start = if delta_line == 0 {
            token_col - prev_start_char
        } else {
            token_col
        };

        semantic_tokens.push(SemanticToken {
            delta_line: delta_line as u32,
            delta_start: delta_start as u32,
            length: token_len as u32,
            token_type: token_type_index(kind),
            token_modifiers_bitset: 0,
        });

        prev_line = token_line;
        prev_start_char = token_col;

        // Advance by 1 for separating tokens
        char_offset += 1;
    }

    Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
        result_id: None,
        data: semantic_tokens,
    })))
}

fn token_type_index(typ: SemanticTokenType) -> u32 {
    match typ.as_str() {
        "keyword" => 0,
        "variable" => 1,
        "type" => 2,
        "string" => 3,
        _ => 0,
    }
}

pub fn provide_semantic_tokens(source: &str) -> Vec<SemanticToken> {
    let lexed = lex(source);
    let mut char_offset = 0;
    let mut current_line = 0;
    let mut prev_start_char = 0;
    let mut offset_start = 0;
    let mut semantic_tokens = vec![];

    for token in lexed {
        let len = token.text.chars().count();
        char_offset += len;
        if token.kind == SyntaxKind::NewLine {
            current_line += 1;
        }
        // Skip unknown tokens
        let kind = match token.kind {
            SyntaxKind::Let => SemanticTokenType::KEYWORD,
            SyntaxKind::Ident => SemanticTokenType::VARIABLE,
            SyntaxKind::Type => SemanticTokenType::TYPE,
            SyntaxKind::StringLiteral => SemanticTokenType::STRING,
            _ => {
                offset_start += token.text.chars().count();
                continue;
            }
        };

        semantic_tokens.push(SemanticToken {
            delta_line: current_line as u32,
            delta_start: offset_start as u32,
            length: len as u32,
            token_type: token_type_index(kind),
            token_modifiers_bitset: 0,
        });

        offset_start += token.text.chars().count();
    }
    semantic_tokens
}

#[cfg(test)]
mod qtests {
    use super::*;
    use quickcheck::quickcheck;

    quickcheck! {
        fn parsing_does_not_panic(input: String) -> bool {
            let tokens = lex(&input);
            let cst = parse_tokens_to_cst(&tokens);
            let _ast = lower_to_ast(&cst);
            true // if we reached here, no panic = pass
        }

        fn compile_outputs_valid_json(input: String) -> bool {
            let tokens = lex(&input);
            let cst = parse_tokens_to_cst(&tokens);
            let ast = lower_to_ast(&cst);
            let json = compile(&ast);
            serde_json::from_str::<serde_json::Value>(&json).is_ok()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_name2() {
        let input = "let name: string = \"Abhi\";";
        let lexed = lex(&input);
        assert_eq!(
            lexed,
            vec![
                Token::new(TokenData {
                    kind: SyntaxKind::Let,
                    text: "let".to_string()
                }),
                Token::new(TokenData {
                    kind: SyntaxKind::Whitespace,
                    text: " ".to_string()
                }),
                Token::new(TokenData {
                    kind: SyntaxKind::Ident,
                    text: "name".to_string()
                }),
                Token::new(TokenData {
                    kind: SyntaxKind::Colon,
                    text: ":".to_string()
                }),
                Token::new(TokenData {
                    kind: SyntaxKind::Whitespace,
                    text: " ".to_string()
                }),
                Token::new(TokenData {
                    kind: SyntaxKind::Type,
                    text: "string".to_string()
                }),
                Token::new(TokenData {
                    kind: SyntaxKind::Whitespace,
                    text: " ".to_string()
                }),
                Token::new(TokenData {
                    kind: SyntaxKind::Equal,
                    text: "=".to_string()
                }),
                Token::new(TokenData {
                    kind: SyntaxKind::Whitespace,
                    text: " ".to_string()
                }),
                Token::new(TokenData {
                    kind: SyntaxKind::StringLiteral,
                    text: "Abhi".to_string()
                }),
                Token::new(TokenData {
                    kind: SyntaxKind::Semicolon,
                    text: ";".to_string()
                }),
            ]
        );
        let mut char_offset = 0;
        let mut current_line = 0;
        let mut prev_start_char = 0;
        let mut offset_start = 0;
        let mut semantic_tokens = vec![];

        for token in lexed {
            let len = token.text.chars().count();
            char_offset += len;
            if token.kind == SyntaxKind::NewLine {
                current_line += 1;
            }
            // Skip unknown tokens
            let kind = match token.kind {
                SyntaxKind::Let => SemanticTokenType::KEYWORD,
                SyntaxKind::Ident => SemanticTokenType::VARIABLE,
                SyntaxKind::Type => SemanticTokenType::TYPE,
                SyntaxKind::StringLiteral => SemanticTokenType::STRING,
                _ => {
                    offset_start += token.text.chars().count();
                    continue;
                }
            };

            semantic_tokens.push(SemanticToken {
                delta_line: current_line as u32,
                delta_start: offset_start as u32,
                length: len as u32,
                token_type: token_type_index(kind),
                token_modifiers_bitset: 0,
            });

            offset_start += token.text.chars().count();
        }

        let input_sem = semantic_tokens;

        if let Some(first) = input_sem.first() {
            let line = first.delta_line;
            let delta_start = first.delta_start;
            let len = first.length;

            assert_eq!(line, 0);
            assert_eq!(delta_start, 0);
            assert_eq!(len, 3);
        }
        if let Some(first) = input_sem.get(1) {
            let line = first.delta_line;
            let delta_start = first.delta_start;
            let len = first.length;

            assert_eq!(line, 0);
            assert_eq!(delta_start, 4);
            assert_eq!(len, 4);
        }
        if let Some(semantic_token) = input_sem.get(2) {
            let line = semantic_token.delta_line;
            let delta_start = semantic_token.delta_start;
            let len = semantic_token.length;

            assert_eq!(line, 0);
            assert_eq!(delta_start, 10);
            assert_eq!(len, 6);
        }
    }
}
