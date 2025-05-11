/// # Example
///```
/// use rize_syntax::syntaxkind;
///
/// syntaxkind! {
///     Identifier,
///     Number,
///     Plus,
///     Minus,
///     Star,
///     Slash,
///     LParen,
///     RParen,
///     IndentWhiteSpace,
/// }
///
/// assert_eq!(SyntaxKind::IndentWhiteSpace.to_string(), "INDENTWHITESPACE");
///
/// ```
#[macro_export]
macro_rules! syntaxkind {
    ( $( $variant:ident ),* $(,)? ) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        #[repr(u8)]
        pub enum SyntaxKind {
            $( $variant ),*
        }

        impl std::fmt::Display for SyntaxKind {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $( SyntaxKind::$variant => write!(f, "{}", stringify!($variant).to_uppercase())),*
                }
            }
        }
    };
}

syntaxkind! {
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
    NewLine
}
