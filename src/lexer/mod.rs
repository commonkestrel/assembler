use crate::ascii::{self, caret_decode, AsciiStr};
use std::{error::Error, str::FromStr};
use std::{fmt, path};
pub mod token;
pub mod stream;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    start: usize,
    end: usize,
    /// The original source file into which this span points.
    source: path::PathBuf,
}

impl Span {
    pub fn new(mut start: usize, mut end: usize, source: path::PathBuf) -> Self {
        if start > end {
            std::mem::swap(&mut start, &mut end);
        }

        Span {
            start: start,
            end: end,
            source,
        }
    }   

    pub fn start(&self) -> usize {
        self.start
    }

    pub fn end(&self) -> usize {
        self.end
    }

    pub fn source(&self) -> &path::PathBuf {
        &self.source
    }
}

struct Token {
    token: TokenType,
    span: Span,
}


enum TokenType {
    /// New Line.
    NewLine,
    /// Start of a doc comment (`///`).
    Doc,
    /// Any identifier.
    Ident(Ident),
    /// Any [Literal]
    Literal(Literal),
    /// `.`
    Dot,
    /// `:`
    Colon,
    /// `::`
    DoubleColon,
    /// `;`
    SemiColon,
    /// `,`
    Comma,
    /// `=>`
    FatArrow,

    /* C style pre-proc macros. */
    /// Include macro (`#include`).
    Include,
    /// Define macro (`#define`).
    Define,
    /// Un-define macro (`#undef`).
    Undef,
    /// If macro (`#if`).
    If,
    /// Else If macro (`#elif`).
    ElIf,
    /// If Defined macro (`#ifdef`).
    IfDef,
    /// If Not Defined macro (`#ifndef`).
    IfNDef,
    /// Else If Defined macro (`#elifdef`)
    ElIfDef,
    /// Else If Not Defined macro (`#elifndef`).
    ElIfNDef,
    /// End If macro (`#endif`).
    EndIf,

    /// `pub`
    Pub,
    /// `macro`
    Macro,

    /* Arithmetic Tokens. */
    /// `<`
    LessThan,
    /// `>`
    GreaterThan,
    /// `+`
    Plus,
    /// `-`
    Minus,
    /// `*`
    Star,
    /// `**`
    DoubleStar,
    /// `/`
    Slash,
    /// `<<`
    Shl,
    /// `>>`
    Shr,
    /// `!`
    Not,
    /// `^`
    Caret,
    /// `&`
    Ampersand,
    /// `|`
    Pipe,
    /// `=`
    Equal,

    /* Delimeters */
    /// `{`
    OpenBrace,
    /// `}`
    CloseBrace,
    /// `[`
    OpenBracket,
    /// `]`
    CloseBracket,
    /// `(`
    OpenParen,
    /// `)`
    CloseParen,
}
