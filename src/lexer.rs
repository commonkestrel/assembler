use std::fs::File;
use std::io::{BufRead, BufReader, ErrorKind};
use std::marker::PhantomData;
use std::ops::Range;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::str::FromStr;

use crate::ascii::{unescape_str, AsciiStr, UnescapeError};
use crate::diagnostic::{Diagnostic, OptionScream, ResultScream};
use logos::{Lexer, Logos};

pub type TokenStream = Vec<Token>;
pub type LexResult = std::result::Result<TokenStream, Errors>;
pub type Errors = Vec<Diagnostic>;

pub struct Token {
    pub inner: TokenInner,
    pub span: Span,
}

pub fn lex<'a, P: AsRef<Path>>(path: P) -> LexResult {
    let mut tokens: TokenStream = Vec::new();
    let mut errs: Errors = Vec::new();
    let source = Rc::new(path.as_ref().to_path_buf());
    let mut character = 0;

    match File::open(&path) {
        Ok(file) => {
            for (line_num, line) in BufReader::new(file).lines().enumerate() {
                match line {
                    Ok(line) => {
                        for (token, span) in TokenInner::lexer(&line).spanned() {
                            let spanned = (span.start + character)..(span.end + character);
                            match token {
                                Ok(tok) => tokens.push(Token {
                                    inner: tok,
                                    span: Span {
                                        line: line_num,
                                        range: spanned,
                                        source: source.clone(),
                                    },
                                }),
                                Err(mut err) => {
                                    err.set_span(Span {
                                        line: line_num,
                                        range: spanned,
                                        source: source.clone(),
                                    });
                                    errs.push(err);
                                }
                            }
                        }
                        // Add one as well to compensate for the newline.
                        character += line.len() + 1;

                        tokens.push(Token {
                            inner: TokenInner::NewLine,
                            span: Span {
                                line: line_num,
                                range: character - 1..character,
                                source: source.clone(),
                            },
                        })
                    }
                    Err(err) => {
                        errs.push(Diagnostic::error(match err.kind() {
                            ErrorKind::InvalidData => format!("encountered invalid data on line {line_num} (likely not valid UTF-8)"),
                            _ => format!("encountered an unexpected error while reading the input file on line {line_num}: {}", err.kind()),
                        }));
                        break;
                    }
                }
            }
        }
        Err(err) => errs.push(Diagnostic::error(match err.kind() {
            ErrorKind::NotFound => format!("unable to locate file: {}", path.as_ref().display()),
            ErrorKind::PermissionDenied => "insufficient permissions".to_string(),
            _ => format!(
                "encountered an unexpected error while opening input file: {}",
                err.kind()
            ),
        })),
    }

    if errs.is_empty() {
        Ok(tokens)
    } else {
        Err(errs)
    }
}

#[derive(Logos, Clone, Debug, PartialEq)]
#[logos(error = Diagnostic)]
pub enum TokenInner {
    #[regex(r"[1-9][_0-9]*", TokenInner::decimal)]
    #[regex(r"0b[01][_01]+", TokenInner::binary)]
    #[regex(r"0x[0-9a-fA-F][_0-9a-fA-F]*", TokenInner::hexadecimal)]
    #[regex(r"0o[0-7][_0-7]*", TokenInner::octal)]
    Immediate(u64),

    #[regex(r#""((\\")|[\x00-\x21\x23-\x7F])*""#, TokenInner::string)]
    #[regex(r##"r#"((\\")|[\x00-\x21\x23-\x7F])*"#"##, TokenInner::raw_string)]
    String(AsciiStr),

    #[regex(r#"<[^"]*>"#, TokenInner::path)]
    #[regex(r#"<"[^"]*">"#, TokenInner::path_string)]
    Path(PathBuf),

    #[regex(r"'[\x00-\x7F]'", TokenInner::char)]
    #[regex(r"'\\[\x00-\x7F]{1,3}'", TokenInner::escape)]
    Char(u8),

    #[regex(r"\n")]
    NewLine,

    #[regex(r"\[0b[01][_01]*\]", TokenInner::addr_bin)]
    #[regex(r"\[0o[0-7][_0-7]*\]", TokenInner::addr_oct)]
    #[regex(r"\[[0-9][_0-9]*\]", TokenInner::addr_dec)]
    #[regex(r"\[0x[0-9a-fA-F][_0-9a-fA-F]*\]", TokenInner::addr_hex)]
    Address(u16),
    
    #[regex(r"[_a-zA-Z][_a-zA-Z0-9]", Ident::any)]
    #[regex(r"%[_a-zA-Z][_a-zA-Z0-9]", Ident::macro_variable)]
    #[regex(r"\$[_a-zA-Z][_a-zA-Z0-9]", Ident::variable)]
    Ident(Ident),
}

impl TokenInner {
    fn binary(lex: &mut Lexer<TokenInner>) -> Option<u64> {
        let slice = lex.slice().replace("_", "");
        u64::from_str_radix(&slice.strip_prefix("0b")?, 2).ok()
    }

    fn octal(lex: &mut Lexer<TokenInner>) -> Option<u64> {
        let slice = lex.slice().replace("_", "");
        u64::from_str_radix(&slice.strip_prefix("0o")?, 8).ok()
    }

    fn decimal(lex: &mut Lexer<TokenInner>) -> Option<u64> {
        let slice = lex.slice().replace("_", "");
        u64::from_str_radix(&slice, 10).ok()
    }

    fn hexadecimal(lex: &mut Lexer<TokenInner>) -> Option<u64> {
        let slice = lex.slice().replace("_", "");
        u64::from_str_radix(&slice.strip_prefix("0x")?, 16).ok()
    }

    fn string(lex: &mut Lexer<TokenInner>) -> Result<AsciiStr, Diagnostic> {
        let slice = lex
            .slice()
            .strip_prefix("\"")
            .ok_or(Diagnostic::error("string not prefixed with `\"`"))?
            .strip_suffix("\"")
            .ok_or(Diagnostic::error("string not suffixed with `\"`"))?;

        unescape_str(&slice).map_err(|err| {
            Diagnostic::error(match err {
                UnescapeError::InvalidAscii(byte) => format!("invalid ASCII character: {byte}"),
                UnescapeError::UnmatchedBackslash(index) => {
                    format!("unmatched '\\' at string index {index}")
                }
            })
        })
    }

    fn raw_string(lex: &mut Lexer<TokenInner>) -> Result<AsciiStr, Diagnostic> {
        let slice = lex
            .slice()
            .strip_prefix("r#\"")
            .ok_or(Diagnostic::error("string not prefixed with `r#\"`"))?
            .strip_suffix("#\"")
            .ok_or(Diagnostic::error("string not suffixed with `\"#`"))?;

        unescape_str(&slice).map_err(|err| {
            Diagnostic::error(match err {
                UnescapeError::InvalidAscii(byte) => format!("invalid ASCII character: {byte}"),
                UnescapeError::UnmatchedBackslash(index) => {
                    format!("unmatched `\\` at string index {index}")
                }
            })
        })
    }

    fn char(lex: &mut Lexer<TokenInner>) -> Result<u8, Diagnostic> {
        let slice = lex
            .slice()
            .strip_prefix('\'')
            .ok_or(Diagnostic::error("string not prefixed with `'`"))?
            .strip_suffix('\'')
            .ok_or(Diagnostic::error("string not suffixed with `'`"))?;

        slice
            .as_bytes()
            .get(0)
            .copied()
            .ok_or(Diagnostic::error("no inner byte found in char"))
    }

    fn escape(lex: &mut Lexer<TokenInner>) -> Result<u8, Diagnostic> {
        let slice = lex
            .slice()
            .strip_prefix('\'')
            .ok_or(Diagnostic::error("char not prefixed with `'`"))?
            .strip_suffix('\'')
            .ok_or(Diagnostic::error("char not suffixed with `'`"))?;

        let escaped = unescape_str(slice).map_err(|err| {
            Diagnostic::error(match err {
                UnescapeError::InvalidAscii(byte) => format!("invalid ASCII character: {byte}"),
                UnescapeError::UnmatchedBackslash(index) => {
                    format!("unmatched `\\` at string index {index}")
                }
            })
        })?;
        Ok(escaped[0])
    }

    fn path(lex: &mut Lexer<TokenInner>) -> Option<PathBuf> {
        let slice = lex.slice().strip_prefix("<")?.strip_suffix(">")?;
        PathBuf::try_from(slice).ok()
    }

    fn path_string(lex: &mut Lexer<TokenInner>) -> Option<PathBuf> {
        let slice = lex.slice().strip_prefix("<\"")?.strip_suffix("\">")?;
        PathBuf::try_from(slice).ok()
    }

    fn addr_bin(lex: &mut Lexer<TokenInner>) -> Result<u16, Diagnostic> {
        let slice = lex
            .slice()
            .strip_prefix("[0b")
            .ok_or(Diagnostic::error(
                "binary address does not start with `[0b`",
            ))?
            .strip_suffix("]")
            .ok_or(Diagnostic::error("binary address does not end with `]`"))?
            .replace("_", "");

        u16::from_str_radix(&slice, 2).map_err(|err| {
            Diagnostic::error(format!("address must be a valid 16-bit integer: {err}"))
        })
    }

    fn addr_oct(lex: &mut Lexer<TokenInner>) -> Result<u16, Diagnostic> {
        let slice = lex
            .slice()
            .strip_prefix("[0o")
            .ok_or(Diagnostic::error(
                "binary address does not start with `[0o`",
            ))?
            .strip_suffix("]")
            .ok_or(Diagnostic::error("binary address does not end with `]`"))?;

        u16::from_str_radix(slice, 8).map_err(|err| {
            Diagnostic::error(format!("address must be a valid 16-bit integer: {err}"))
        })
    }
    fn addr_dec(lex: &mut Lexer<TokenInner>) -> Result<u16, Diagnostic> {
        let slice = lex
            .slice()
            .strip_prefix("[")
            .ok_or(Diagnostic::error("binary address does not start with `[`"))?
            .strip_suffix("]")
            .ok_or(Diagnostic::error("binary address does not end with `]`"))?;

        u16::from_str_radix(slice, 10).map_err(|err| {
            Diagnostic::error(format!("address must be a valid 16-bit integer: {err}"))
        })
    }
    fn addr_hex(lex: &mut Lexer<TokenInner>) -> Result<u16, Diagnostic> {
        let slice = lex
            .slice()
            .strip_prefix("[0x")
            .ok_or(Diagnostic::error(
                "binary address does not start with `[0x`",
            ))?
            .strip_suffix("]")
            .ok_or(Diagnostic::error("binary address does not end with `]`"))?;

        u16::from_str_radix(slice, 16).map_err(|err| {
            Diagnostic::error(format!("address must be a valid 16-bit integer: {err}"))
        })
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Ident {
    Register(Register),
    Keyword(Keyword),
    Instruction(Instruction),
    Variable(String),
    MacroVariable(String),
    Ident(String),
}

impl Ident {
    fn variable(lex: &mut Lexer<TokenInner>) -> Result<Ident, Diagnostic> {
        let slice = lex.slice().strip_prefix("$").ok_or(Diagnostic::error("variable not prefixed by `$`"))?;
        Ok(Ident::Variable(slice.to_owned()))
    }

    fn macro_variable(lex: &mut Lexer<TokenInner>) -> Result<Ident, Diagnostic> {
        let slice = lex.slice().strip_prefix("%").ok_or(Diagnostic::error("macro variable not prefixed by `%`"))?;
        Ok(Ident::MacroVariable(slice.to_owned()))
    }

    fn any(lex: &mut Lexer<TokenInner>) -> Ident {
        let slice = lex.slice();

        if let Ok(register) = Register::from_str(slice) {
            Ident::Register(register)
        } else if let Ok(keyword) = Keyword::from_str(slice) {
            Ident::Keyword(keyword)
        } else if let Ok(instruction) = Instruction::from_str(slice) {
            Ident::Instruction(instruction)
        } else {
            Ident::Ident(slice.to_owned())
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Register {
    /// GP register A.
    A,
    /// GP register B.
    B,
    /// GP register C.
    C,
    /// Data direction register.
    DD,
    /// GP register Z
    Z,
    /// Status register
    SREG,
    /// Memory index low.
    L,
    /// Memory index high.
    H,
}

impl FromStr for Register {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "a" => Ok(Register::A),
            "b" => Ok(Register::B),
            "c" => Ok(Register::C),
            "dd" => Ok(Register::DD),
            "z" => Ok(Register::Z),
            "s" | "sreg" => Ok(Register::SREG),
            "l" => Ok(Register::L),
            "h" => Ok(Register::H),
            _ => Err(()),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Keyword {
    Define,
    Include,
    Paste,
}

impl FromStr for Keyword {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            _ => Err(())
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Instruction {

}

impl FromStr for Instruction {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            _ => Err(())
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    line: usize,
    range: Range<usize>,
    source: Rc<PathBuf>,
}

impl Span {
    pub fn line(&self) -> usize {
        self.line
    }

    pub fn start(&self) -> usize {
        self.range.start
    }

    pub fn end(&self) -> usize {
        self.range.end
    }

    pub fn source(&self) -> &Path {
        &self.source
    }
}

mod tests {
    use super::*;

    #[test]
    fn numbers() {
        println!("{}", std::env::current_dir().unwrap().display());
        let lexed = match lex("./examples/numbers.asm") {
            Ok(tokens) => tokens,
            Err(errors) => {
                for error in errors {
                    error.force_emit();
                }
                Diagnostic::error("failed due to previous errors").scream();
            }
        };

        println!(
            "{:?}",
            lexed
                .into_iter()
                .map(|tok| tok.inner)
                .collect::<Vec<TokenInner>>()
        );
    }
}
