use std::ops::Range;
use std::path::PathBuf;

use logos::{ Logos, Lexer };
use crate::ascii::{ AsciiStr, unescape_str, UnescapeError };
use crate::diagnostic::{ ResultScream, OptionScream };

#[derive(Logos, Clone, Debug, PartialEq)]
enum TokenInner {
    #[regex(r"[1-9][_0-9]*", TokenInner::decimal)]
    #[regex(r"0b[01][_01]+", TokenInner::binary)]
    #[regex(r"0x[0-9a-fA-F][_0-9a-fA-F]*", TokenInner::hexadecimal)]
    #[regex(r"0o[0-7][_0-7]*", TokenInner::octal)]
    Immediate(u8),

    #[regex(r#""([\x00-\x7F\\"])*""#, TokenInner::string)]
    #[regex(r##"r#"([\x00-\x7F\\"])*"#"##, TokenInner::raw_string)]
    String(AsciiStr),

    #[regex(r#"<[^"]*>"#, TokenInner::path)]
    #[regex(r#"<"[^"]*">"#, TokenInner::path_string)]
    Path(PathBuf),

    #[regex(r"'\\''", |_| 29 /* Ascii code for "'" */)]
    #[regex(r"'[\x00-\x7F]'", TokenInner::char)]
    #[regex(r"'\^[\x00-\x7F]'", TokenInner::caret)]
    Char(u8),
}

impl TokenInner {
    fn binary(lex: &mut Lexer<TokenInner>) -> Option<u8> {
        let slice = lex.slice().replace("_", "");
        u8::from_str_radix(&slice.strip_prefix("0b")?, 2).ok()
    }
    
    fn octal(lex: &mut Lexer<TokenInner>) -> Option<u8> {
        let slice = lex.slice().replace("_", "");
        u8::from_str_radix(&slice.strip_prefix("0o")?, 8).ok()
    }

    fn decimal(lex: &mut Lexer<TokenInner>) -> Option<u8> {
        let slice = lex.slice().replace("_", "");
        u8::from_str_radix(&slice, 10).ok()
    }

    fn hexadecimal(lex: &mut Lexer<TokenInner>) -> Option<u8> {
        let slice = lex.slice().replace("_", "");
        u8::from_str_radix(&slice.strip_prefix("0x")?, 16).ok()
    }

    fn string(lex: &mut Lexer<TokenInner>) -> Option<AsciiStr> {
        let slice = lex.slice().strip_prefix("\"")?.strip_suffix("\"")?;
        unescape_str(&slice).ok()?.into_owned().try_into().ok()
    }

    fn raw_string(lex: &mut Lexer<TokenInner>) -> Option<AsciiStr> {
        let slice = lex.slice().strip_prefix("r#\"")?.strip_suffix("#\"")?;
        unescape_str(&slice).ok()?.into_owned().try_into().ok()
    }

    fn char(lex: &mut Lexer<TokenInner>) -> u8 {
        todo!()
    }

    fn caret(lex: &mut Lexer<TokenInner>) -> Option<u8> {
        todo!()
    }

    fn path(lex: &mut Lexer<TokenInner>) -> Option<PathBuf> {
        let slice = lex.slice().strip_prefix("<")?.strip_suffix(">")?;
        PathBuf::try_from(slice).ok()
    }

    fn path_string(lex: &mut Lexer<TokenInner>) -> Option<PathBuf> {
        let slice = lex.slice().strip_prefix("<\"")?.strip_suffix("\">")?;
        PathBuf::try_from(slice).ok()
    }
}

#[derive(Debug, Clone)]
pub struct Span {
    range: Range<usize>,
    source: PathBuf,
}

impl Span {
    pub fn start(&self) -> usize {
        self.range.start
    }

    pub fn end(&self) -> usize {
        self.range.end
    }

    pub fn source(&self) -> &PathBuf {
        &self.source
    }
}
