use std::collections::HashMap;

use crate::Errors;
use crate::lex::{ self, TokenStream, TokenInner, Token, Span, Ident, Register };
use crate::diagnostic::{Diagnostic, OptionalScream};

pub fn parse(stream: TokenStream) -> Result<(Errors), Errors> {
    todo!()
}

trait Parse: Sized {
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic>;
}

impl<'a> Cursor<'a> {
    fn new(buffer: &'a [Token]) -> Self {
        Self {
            buffer,
            position: 0,
        }
    }

    fn next(&mut self) -> Option<Token> {
        let ret = self.buffer[self.position].clone();

        self.position += 1;

        Some(ret)
    }

    fn parse<R>(&mut self) -> Result<R, Diagnostic>
    where
        R: Parse
    {
        R::parse(self)
    }
}

struct Cursor<'a> {
    buffer: &'a [Token],
    position: usize,
}

pub enum Expanded {
    Instruction(Instruction),
    Label(Ident),
    SubLabel(Ident),
    Variable(Variable)
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

pub struct Variable {
    
}

enum Type {
    Reg,
    Addr,
    Label,
    Imm8,
    Imm16,
    Imm32,
    Imm64,
}

pub struct Parameter {
    types: Vec<Type>,
    name: String,
}

impl Parameter {
    fn fits(&self, token: &TokenInner) -> bool {
        for ty in self.types.iter() {
            if 
                (matches!(ty, Type::Addr) && matches!(token, TokenInner::Address(_))) | 
                (matches!(ty, Type::Label) && matches!(token, TokenInner::Ident(Ident::Ident(_)))) | 
                (matches!(ty, Type::Reg) && matches!(token, TokenInner::Ident(Ident::Register(_)))) {
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

        let parameters: HashMap<String, Token> = HashMap::from_iter(self.parameters.iter().map(|p| p.name.to_owned()).zip(parameters.into_iter()));

        for token in self.tokens {
            expanded.push(match token.inner {
                TokenInner::Ident(Ident::MacroVariable(name)) => parameters.get(&name).spanned_expect(token.span, format!("macro variable `{name}` not found in scope")).clone(),
                _ => token,
            })
        }

        expanded
    }
}

pub struct Macro {
    definitions: Vec<MacroDef>,
}

impl Macro {
    
}


