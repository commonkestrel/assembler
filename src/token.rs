use crate::lex::Span;
use crate::parse::{Parse, Cursor};
use crate::Diagnostic;

/// Same idea as [syn](https://docs.rs/syn/latest/syn/macro.Token.html).
#[macro_export]
macro_rules! Token {
    [@define] => {$crate::token::Define}
}

macro_rules! parsable {
    // Delimeters and punctuation
    ($(match $token:ident($enum:ident::$varient:ident) => $name:ident),* $(,)?) => {
        $(
            #[derive(Debug, Clone)]
            pub struct $name {
                span: $crate::lex::Span
            }

            impl $crate::parse::Parse for $name {
                fn parse(cursor: &mut $crate::parse::Cursor) -> Result<Self, Diagnostic> {
                    match cursor.next() {
                        Some($crate::lex::Token { inner: $crate::lex::TokenInner::$token($crate::lex::$enum::$varient), span }) => Ok($name{ span: span.clone() }),
                         _ => Err(Diagnostic::error("Expected `(`")),
                    }
                }
            }
        )*
    }
}

/* Delimeters */

parsable! {
    match Delimeter(Delimeter::OpenParen) => OpenParen,
    match Delimeter(Delimeter::ClosedParen) => ClosedParen,
    match Delimeter(Delimeter::OpenBracket) => OpenBracket,
    match Delimeter(Delimeter::ClosedBracket) => ClosedBracket,
    match Delimeter(Delimeter::OpenBrace) => OpenBrace,
    match Delimeter(Delimeter::OpenBrace) => ClosedBrace,
}

/* Punctuation */

parsable! {
    match Punctuation(Punctuation::Eq) => Eq,
    match Punctuation(Punctuation::EqEq) => EqEq,
    match Punctuation(Punctuation::Ne) => Ne,
    match Punctuation(Punctuation::Lt) => Lt,
    match Punctuation(Punctuation::Le) => Le,
    match Punctuation(Punctuation::Gt) => Gt,
    match Punctuation(Punctuation::Ge) => Ge,
    match Punctuation(Punctuation::And) => And,
    match Punctuation(Punctuation::AndAnd) => AndAnd,
    match Punctuation(Punctuation::Or) => Or,
    match Punctuation(Punctuation::OrOr) => OrOr,
    match Punctuation(Punctuation::Caret) => Caret,
    match Punctuation(Punctuation::Not) => Not,
    match Punctuation(Punctuation::Plus) => Plus,
    match Punctuation(Punctuation::Minus) => Minus,
    match Punctuation(Punctuation::Slash) => Slash,
    match Punctuation(Punctuation::Star) => Star,
    match Punctuation(Punctuation::Shl) => Shl,
    match Punctuation(Punctuation::Shr) => Shr,
    match Punctuation(Punctuation::Comma) => Comma,
}

/* Pre-proc arguments */

pub struct Define(Span);
pub struct IfDef(Span);
pub struct IfNDef(Span);
pub struct If(Span);
pub struct Else(Span);
pub struct EndIf(Span);

