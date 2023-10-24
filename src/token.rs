use crate::ascii::AsciiStr;
use crate::lex::{self, Delimeter, PreProc, Punctuation, Span, Token, TokenInner, TokenStream};
use crate::parse::{Cursor, Parse};
use crate::Diagnostic;

/// A type-macro that expands to the name of the Rust type representation of a given token.
///
/// Commonly used in struct fields, the type of a `let` statement, or generics for a [`parse`][Parse::parse] call.
///
/// Same idea as [syn](https://docs.rs/syn/latest/syn/macro.Token.html).
#[macro_export]
macro_rules! Token {
    [=] => {$crate::token::Eq};
    [==] => {$crate::token::EqEq};
    [!=] => {$crate::token::Ne};
    [<] => {$crate::token::Lt};
    [<=] => {$crate::token::Le};
    [>] => {$crate::token::Gt};
    [>=] => {$crate::token::Ge};
    [&] => {$crate::token::And};
    [&&] => {$crate::token::AndAnd};
    [|] => {$crate::token::Or};
    [||] => {$crate::token::OrOr};
    [^] => {$crate::token::Caret};
    [!] => {$crate::token::Not};
    [~] => {$crate::token::Not};
    [+] => {$crate::token::Plus};
    [-] => {$crate::token::Minus};
    [/] => {$crate::token::Slash};
    [*] => {$crate::token::Star};
    [<<] => {$crate::token::Shl};
    [>>] => {$crate::token::Shr};
    [,] => {$crate::token::Comma};
    [:] => {$crate::token::Colon};
    [@macro] => {$crate::token::Macro};
    [@define] => {$crate::token::Define};
    [@ifdef] => {$crate::token::IfDef};
    [@ifndef] => {$crate::token::IfNDef};
    [@if] => {$crate::token::If};
    [@elif] => {$crate::token::Elif};
    [@else] => {$crate::token::Else};
    [@endif] => {$crate::token::EndIf};
    [@org] => {$crate::token::Org};
}

/// Creates a struct for a varient of [`TokenInner`][crate::lex::TokenInner] and implements [`Parse`] for it.
///
/// # Examples
///
/// `parsable!{ "+":  }
macro_rules! parsable {
    ($($symbol:literal$(, $alt:literal)?; match $token:ident($inner:pat) => $name:ident$({$($v:vis $field:ident: $ty:ty)*})?),* $(,)?) => {
        $(
            #[derive(Debug, Clone, PartialEq)]
            pub struct $name {
                pub span: $crate::lex::Span,
                $($($v $field: $ty),*)?
            }

            impl $crate::parse::Parse for $name {
                fn parse(cursor: &mut $crate::parse::Cursor) -> Result<Self, Diagnostic> {
                    match cursor.next() {
                        Some($crate::lex::Token { inner: $crate::lex::TokenInner::$token($inner), span }) => Ok($name{ span: span.clone(), $($($field: ::std::borrow::ToOwned::to_owned($field)),*)? }),
                        _ => Err(Diagnostic::error(concat!("Expected `", $symbol, "`"$(, "or `", $alt)?))),
                    }
                }
            }
        )*
    };
    ($($($symbol:ident)+; match $token:ident($inner:pat) => $name:ident$({$($v:vis $field:ident: $ty:ty)*})?),* $(,)?) => {
        $(
            #[derive(Debug, Clone, PartialEq)]
            pub struct $name {
                pub span: $crate::lex::Span,
                $($($v $field: $ty),*)?
            }

            impl $crate::parse::Parse for $name {
                fn parse(cursor: &mut $crate::parse::Cursor) -> Result<Self, Diagnostic> {
                    match cursor.next() {
                        Some($crate::lex::Token { inner: $crate::lex::TokenInner::$token($inner), span }) => Ok($name{ span: span.clone(), $($($field: ::std::borrow::ToOwned::to_owned($field)),*)? }),
                        _ => Err(Diagnostic::error(concat!("Expected",$(" ", stringify!($symbol)),+))),
                    }
                }
            }
        )*
    };
}

/* Delimeters */

parsable! {
    '('; match Delimeter(Delimeter::OpenParen) => OpenParen,
    ')'; match Delimeter(Delimeter::ClosedParen) => ClosedParen,
    '['; match Delimeter(Delimeter::OpenBracket) => OpenBracket,
    ']'; match Delimeter(Delimeter::ClosedBracket) => ClosedBracket,
    '{'; match Delimeter(Delimeter::OpenBrace) => OpenBrace,
    '}'; match Delimeter(Delimeter::OpenBrace) => ClosedBrace,
}

/* Punctuation */

parsable! {
    '='     ; match Punctuation(Punctuation::Eq) => Eq,
    "=="    ; match Punctuation(Punctuation::EqEq) => EqEq,
    "!="    ; match Punctuation(Punctuation::Ne) => Ne,
    '<'     ; match Punctuation(Punctuation::Lt) => Lt,
    "<="    ; match Punctuation(Punctuation::Le) => Le,
    '>'     ; match Punctuation(Punctuation::Gt) => Gt,
    ">="    ; match Punctuation(Punctuation::Ge) => Ge,
    '&'     ; match Punctuation(Punctuation::And) => And,
    "&&"    ; match Punctuation(Punctuation::AndAnd) => AndAnd,
    '|'     ; match Punctuation(Punctuation::Or) => Or,
    "||"    ; match Punctuation(Punctuation::OrOr) => OrOr,
    '^'     ; match Punctuation(Punctuation::Caret) => Caret,
    '!', '~'; match Punctuation(Punctuation::Not) => Not,
    '+'     ; match Punctuation(Punctuation::Plus) => Plus,
    '-'     ; match Punctuation(Punctuation::Minus) => Minus,
    '/'     ; match Punctuation(Punctuation::Slash) => Slash,
    '*'     ; match Punctuation(Punctuation::Star) => Star,
    "<<"    ; match Punctuation(Punctuation::Shl) => Shl,
    ">>"    ; match Punctuation(Punctuation::Shr) => Shr,
    ','     ; match Punctuation(Punctuation::Comma) => Comma,
    ':'     ; match Punctuation(Punctuation::Colon) => Colon,
}

/* Pre-proc arguments */

parsable! {
    "@[type]" ; match Ident(lex::Ident::PreProc(PreProc::Variable(ty))) => Variable {pub ty: lex::Ty},
    "@include"; match Ident(lex::Ident::PreProc(PreProc::Include)) => Include,
    "@macro"  ; match Ident(lex::Ident::PreProc(PreProc::Macro)) => Macro,
    "@define" ; match Ident(lex::Ident::PreProc(PreProc::Define)) => Define,
    "@ifdef"  ; match Ident(lex::Ident::PreProc(PreProc::IfDef)) => IfDef,
    "@ifndef" ; match Ident(lex::Ident::PreProc(PreProc::IfNDef)) => IfNDef,
    "@if"     ; match Ident(lex::Ident::PreProc(PreProc::If)) => If,
    "@else"   ; match Ident(lex::Ident::PreProc(PreProc::Else)) => Else,
    "@elif"   ; match Ident(lex::Ident::PreProc(PreProc::ElIf)) => ElIf,
    "@endif"  ; match Ident(lex::Ident::PreProc(PreProc::EndIf)) => EndIf,
    "@org"    ; match Ident(lex::Ident::PreProc(PreProc::Org)) => Org,
}

/* Identifiers */

parsable! {
    register; match Ident(lex::Ident::Register(inner)) => Register{pub inner: lex::Register},
    identifier; match Ident(lex::Ident::Ident(value)) => Ident{pub value: String},
    instruction; match Ident(lex::Ident::Instruction(instruction)) => Instruction{pub instruction: lex::Instruction},
    type; match Ident(lex::Ident::Ty(ty)) => Ty{pub ty: lex::Ty},
}

/* Literals */

parsable! {
    integer literal; match Immediate(value) => Immediate{pub value: u64},
    string literal; match String(value) => LitString{pub value: AsciiStr},
    character literal; match Char(ascii) => Char{pub ascii: u8},
    address; match Address(addr) => Address{pub addr: u16},
    doc string; match Doc(md) => Doc{pub md: String},
}

impl Parse for TokenStream {
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
        let mut stream = Vec::new();
        for tok in cursor {
            if let Token {
                span: _,
                inner: TokenInner::NewLine,
            } = tok
            {
                return Ok(stream);
            }

            stream.push(tok.clone());
        }
        Ok(stream)
    }
}
