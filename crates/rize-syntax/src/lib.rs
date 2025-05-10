#![allow(clippy::unwrap_used)]

#[derive(Debug, Clone, PartialEq)]
pub enum SyntaxKind {
    Let,
    Ident,
    Colon,
    Type,
    Equal,
    StringLiteral,
    Semicolon,
    Whitespace,
    Error,
    Root,
    VarDecl,
}

#[derive(Debug, Clone)]
pub struct Token {
   pub kind: SyntaxKind,
   pub text: String,
}

#[derive(Debug, Clone)]
pub enum SyntaxElement {
    Token(Token),
    Node(SyntaxNode),
}

#[derive(Debug, Clone)]
pub struct SyntaxNode {
    pub kind: SyntaxKind,
    pub children: Vec<SyntaxElement>,
}

impl SyntaxNode {
    pub fn new(kind: SyntaxKind, children: Vec<SyntaxElement>) -> Self {
        SyntaxNode { kind, children }
    }

    pub fn tokens(&self) -> Vec<&Token> {
        self.children.iter().filter_map(|el| match el {
            SyntaxElement::Token(tok) => Some(tok),
            _ => None,
        }).collect()
    }

    pub fn child_nodes(&self) -> Vec<&SyntaxNode> {
        self.children.iter().filter_map(|el| match el {
            SyntaxElement::Node(n) => Some(n),
            _ => None,
        }).collect()
    }

    pub fn kind(&self) -> SyntaxKind {
        self.kind.clone()
    }
}

pub fn lex(source: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut chars = source.chars().peekable();

    while let Some(&ch) = chars.peek() {
        match ch {
            c if c.is_whitespace() => {
                chars.next();
            }
            ':' => {
                tokens.push(Token { kind: SyntaxKind::Colon, text: ":".into() });
                chars.next();
            }
            '=' => {
                tokens.push(Token { kind: SyntaxKind::Equal, text: "=".into() });
                chars.next();
            }
            ';' => {
                tokens.push(Token { kind: SyntaxKind::Semicolon, text: ";".into() });
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
                tokens.push(Token {
                    kind: SyntaxKind::StringLiteral,
                    text: value,
                });
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
                tokens.push(Token { kind, text: ident });
            }
            _ => {
                tokens.push(Token {
                    kind: SyntaxKind::Error,
                    text: ch.to_string(),
                });
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

        decls.push(SyntaxElement::Node(SyntaxNode::new(SyntaxKind::VarDecl, children)));
    }

    SyntaxNode::new(SyntaxKind::Root, decls)
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
        let name = tokens.iter().find(|t| t.kind == SyntaxKind::Ident).unwrap().text.clone();
        let ty = tokens.iter().find(|t| t.kind == SyntaxKind::Type).unwrap().text.clone();
        let value = tokens.iter().find(|t| t.kind == SyntaxKind::StringLiteral).unwrap().text.clone();

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

