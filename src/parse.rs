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

use std::collections::HashMap;
use std::str::FromStr;

use crate::diagnostic::{Diagnostic, OptionalScream};
use crate::lex::{self, Register, Span, Token, TokenInner, TokenStream, Ty};
use crate::token::Ident;
use crate::Errors;
use crate::Token;

pub fn parse(stream: TokenStream) -> Result<(), Errors> {
    Err(vec![Diagnostic::error("Parsing not yet implemented")])
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
    name: String,
    value: Option<Token>,
}

impl Parse for Define {
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
        let _def: Token![@define] = cursor.parse()?;
        let name: Ident = cursor.parse()?;
        let assignment = cursor.next();

        Ok(Define {
            name: name.value,
            value: assignment.map(|tok| tok.to_owned()),
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
                    value: None,
                })
            }
        };

        Ok(Define {
            name: s[..pos].to_owned(),
            value: Some(s[pos + 1..].parse()?),
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

pub struct Parameter {
    types: Vec<Ty>,
    name: String,
}

impl Parameter {
    fn fits(&self, token: &TokenInner) -> bool {
        for ty in self.types.iter() {
            if (matches!(ty, Ty::Addr) && matches!(token, TokenInner::Address(_)))
                | (matches!(ty, Ty::Label)
                    && matches!(token, TokenInner::Ident(lex::Ident::Ident(_))))
                | (matches!(ty, Ty::Reg)
                    && matches!(token, TokenInner::Ident(lex::Ident::Register(_))))
            {
                return true;
            }
        }

        false
    }
}

pub struct MacroDef {
    parameters: Vec<Parameter>,
    tokens: TokenStream,
}

impl MacroDef {
    pub fn expand(self, parameters: Vec<Token>) -> TokenStream {
        let mut expanded = TokenStream::with_capacity(self.tokens.len());

        let parameters: HashMap<String, Token> = HashMap::from_iter(
            self.parameters
                .iter()
                .map(|p| p.name.to_owned())
                .zip(parameters.into_iter()),
        );

        for token in self.tokens {
            expanded.push(match token.inner {
                TokenInner::Ident(lex::Ident::MacroVariable(name)) => parameters
                    .get(&name)
                    .spanned_expect(
                        token.span,
                        format!("macro variable `{name}` not found in scope"),
                    )
                    .clone(),
                _ => token,
            })
        }

        expanded
    }
}

pub struct Macro {
    definitions: Vec<MacroDef>,
}

impl Macro {}
