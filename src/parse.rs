//! WIP token tree parsing
//! Split into three passes:
//! 1. Preprocessor expansion
//! 2. Instruction parsing
//! 3. Macro expansion
//!
//! ## Preprocessor Expansion
//!
//! Preproc expansion includes the obvious things, like @define, @paste, etc.,
//! but also expressions (`(1 << 2) & (1 << 5)`), and macros.
//!
//! ## Instruction Parsing
//!
//! Parses tokens like `ldi A, 0x7F` into recognizable structures.
//!
//! ## Macro Expansion
//!
//! Recursivly expands macros, leaving the token tree with just instructions.

use bitflags::bitflags;
use std::collections::HashMap;
use std::str::FromStr;

use crate::diagnostic::Diagnostic;
use crate::lex::{self, PreProc, Register, Span, Token, TokenInner, TokenStream, Ty};
use crate::token::Ident;
use crate::Errors;
use crate::{error, spanned_error, spanned_warn, warn, Token};

pub fn parse(stream: TokenStream) -> Result<(), Errors> {
    let proc_stream = pre_proc(&stream)?;

    Err(vec![error!("Parsing not yet implemented")])
}

macro_rules! match_errors {
    ($ok:ident, $errors:ident, $ex:expr) => {
        match $ex {
            Ok(ok) => $ok.push(ok),
            Err(err) => $errors.push(err),
        }
    };
    ($errors:ident, $ex:expr) => {
        match $ex {
            Ok(ok) => ok,
            Err(err) => $errors.push(err),
        }
    };
}

struct ProcStream {
    stream: TokenStream,
    org: Option<u64>
}

fn pre_proc(stream: &[Token]) -> Result<ProcStream, Errors> {
    use TokenInner as TI;

    let mut cursor = Cursor::new(stream);

    let mut out = Vec::new();
    let mut defines: Vec<Define> = Vec::new();
    let mut errors = Vec::new();
    let mut org = None;

    while let Some(tok) = cursor.peek() {
        match tok.inner {
            TI::Ident(lex::Ident::PreProc(PreProc::Define)) => {
                match_errors!(defines, errors, cursor.parse());
            }
            TI::Ident(lex::Ident::PreProc(PreProc::If)) => {
                cursor.step();
            }
            _ => cursor.step(),
        }
    }

    if errors.is_empty() {
        Ok(ProcStream{ stream: out, org })
    } else {
        Err(errors)
    }
}

pub trait Parse: Sized {
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic>;
}

pub struct Cursor<'a> {
    buffer: &'a [Token],
    position: usize,
}

impl<'a> Cursor<'a> {
    pub fn new(buffer: &'a [Token]) -> Self {
        Self {
            buffer,
            position: 0,
        }
    }

    pub fn parse<R>(&mut self) -> Result<R, Diagnostic>
    where
        R: Parse,
    {
        R::parse(self)
    }

    pub fn peek(&self) -> Option<&'a Token> {
        self.buffer.get(self.position)
    }

    pub fn step(&mut self) {
        self.position += 1;
    }
}

impl<'a> Iterator for Cursor<'a> {
    type Item = &'a Token;

    fn next(&mut self) -> Option<Self::Item> {
        let ret = self.buffer.get(self.position);

        self.position += 1;

        ret
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Define {
    pub name: String,
    pub value: TokenStream,
}

impl Parse for Define {
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
        let _def: Token![@define] = cursor.parse()?;
        let name: Ident = cursor.parse()?;
        let assignment: TokenStream = cursor.parse()?;

        Ok(Define {
            name: name.value,
            value: assignment,
        })
    }
}

impl FromStr for Define {
    type Err = Diagnostic;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let pos = match s.find("=") {
            Some(pos) => pos,
            None => {
                return Ok(Define {
                    name: s.to_owned(),
                    value: Vec::new(),
                })
            }
        };

        let lexed = lex::lex_string(&s[pos + 1..]);
        let l = match lexed {
            Ok(l) => l,
            Err(errors) => {
                for err in errors {
                    err.emit();
                }
                error!("Unable to lex define due to previous errors").scream();
            }
        };

        Ok(Define {
            name: s[..pos].to_owned(),
            value: Cursor::new(&l).parse()?,
        })
    }
}

pub enum Expanded {
    Instruction(Instruction),
    Label(Ident),
    SubLabel(Ident),
    Variable(Variable),
}

pub enum Instruction {
    Add(Register, RegImm),
    Adc(Register, RegImm),
    Sub(Register, RegImm),
    Sbc(Register, RegImm),
    And(Register, RegImm),
    Or(Register, RegImm),
    Xor(Register, RegImm),
    Mv(Register, RegImm),
    Ld(Register),
    St(Register),
    Push(RegImm),
    Pop(Register),
    Jnz(Address),
}

pub enum RegImm {
    Register(Register),
    Immediate(u8),
}

pub enum Address {
    Literal(u16),
    Label(String),
}

pub struct Variable {}

bitflags! {
    #[repr(transparent)]
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct Types: u8 {
        const REG = 0b0000_0001;
        const ADDR = 0b0000_0010;
        const LABEL = 0b0000_0100;
        const STR = 0b0000_1000;
        const IMM8 = 0b0001_0000;
        const IMM16 = 0b0011_0000;
        const IMM32 = 0b0111_0000;
        const IMM64 = 0b1111_0000;
    }
}

pub struct Parameter {
    types: Types,
    name: String,
}

impl Parameter {
    fn fits(&self, token: &Token) -> bool {
        use TokenInner as TI;
        match token.inner {
            TI::String(_) => self.types.contains(Types::STR),
            TI::Immediate(imm) => match imm.checked_ilog2() {
                None | Some(0..=7) => self.types.contains(Types::IMM8),
                Some(8..=15) => self.types.contains(Types::IMM16),
                Some(16..=31) => self.types.contains(Types::IMM32),
                Some(32..=63) => self.types.contains(Types::IMM64),
                _ => unreachable!(),
            },
            TI::Address(_) => self.types.contains(Types::ADDR),
            TI::Ident(lex::Ident::Register(_)) => self.types.contains(Types::REG),
            TI::Ident(_) => self.types.contains(Types::LABEL),
            _ => false,
        }
    }
}

pub struct MacroDef {
    parameters: Vec<Parameter>,
    tokens: TokenStream,
}

impl MacroDef {
    pub fn fits(&self, tokens: &[Token]) -> bool {
        if self.parameters.len() != tokens.len() {
            return false;
        }

        self.parameters
            .iter()
            .zip(tokens)
            .all(|(param, token)| param.fits(token))
    }

    /// Must make sure that the provided parameters match this rule with [`MacroDef::fits`]
    pub fn expand(&self, parameters: &[Token]) -> Result<TokenStream, Diagnostic> {
        let mut expanded = TokenStream::with_capacity(self.tokens.len());

        let parameters: HashMap<String, &Token> = HashMap::from_iter(
            self.parameters
                .iter()
                .map(|p| p.name.to_owned())
                .zip(parameters.into_iter()),
        );

        for token in self.tokens.iter() {
            expanded.push(match &token.inner {
                TokenInner::Ident(lex::Ident::MacroVariable(name)) => {
                    (*parameters.get(name).ok_or(spanned_error!(
                        token.span.clone(),
                        "macro variable `{name}` not found in scope",
                    ))?)
                    .clone()
                }
                _ => token.clone(),
            })
        }

        Ok(expanded)
    }
}

pub struct Macro {
    name: String,
    definitions: Vec<MacroDef>,
}

impl Macro {
    fn expand(self, span: Span, parameters: Vec<Token>) -> Result<TokenStream, Diagnostic> {
        let rule = self
            .definitions
            .iter()
            .find(|def| def.fits(&parameters))
            .ok_or_else(|| spanned_error!(span, "no rules matched "))?;
        rule.expand(&parameters)
    }
}
