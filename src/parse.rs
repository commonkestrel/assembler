//! WIP token tree parsing
//!
//! # Parsing Steps
//! 1. Expand if statements, defines, and parse macros
//! 2. Expand Macros
//! 3. Parse expressions and variables

use bitflags::bitflags;
use std::collections::HashMap;
use std::str::FromStr;
use std::{mem, ops, slice, iter};

use crate::ascii::AsciiStr;
use crate::diagnostic::Diagnostic;
use crate::{eval, token};
use crate::lex::{self, PreProc, Punctuation, Register, Span, Token, TokenInner, TokenStream, Ty};
use crate::Errors;
use crate::token::{Ident, Immediate};
use crate::{error, spanned_error, spanned_warn, warn, Token};

pub fn parse(stream: TokenStream) -> Result<(), Errors> {
    let proc_stream = pre_proc(stream)?;

    Err(vec![error!("Parsing not yet implemented")])
}

macro_rules! match_errors {
    ($ok:ident, $errors:ident, $ex:expr) => {
        match $ex {
            Ok(ok) => $ok.push(ok),
            Err(err) => $errors.push(err),
        }
    };
}

enum Seg {
    Cseg,
    Dseg,
}

struct PostProc {
    macros: Vec<Macro>,
    variables: HashMap<String, u16>,
    code: 
}

struct Processed {
    code: Vec<Cseg>,
    data: Vec<Dseg>,
}

struct Dseg {
    dseg: token::Dseg,
    org: Option<u16>,
    variables: HashMap<String, u16>,
}

impl Dseg {
    fn size(&self) -> Result<u16, Diagnostic> {
        let mut size: u16 = 0;

        for variable in self.variables.values() {
            size = size.checked_add(*variable).ok_or_else(|| spanned_error!(self.dseg.span.clone(), "data segment out of range"))?;
        }

        Ok(size)
    }
}

struct Cseg {
    cseg: Option<token::Cseg>,
    org: Option<u16>,
}

struct Context {
    cseg_org: Option<u16>,
    dseg_org: Option<u16>,
    defines: HashMap<String, TokenStream>,
    stream: TokenStream,
    position: usize,
}

impl Context {
    fn cursor(&self) -> Cursor {
        self.stream.iter().skip(self.position).peekable()
    }
}

pub trait Parse {
    fn parse<R: Parsable>(&mut self) -> Result<R, Diagnostic>;
}

pub type Cursor<'a> = iter::Peekable<iter::Skip<slice::Iter<'a, Token>>>;

impl<'a> Parse for Cursor<'a> {
    fn parse<R: Parsable>(&mut self) -> Result<R, Diagnostic> {
        R::parse(self)
    }
}

fn pre_proc(mut stream: TokenStream) -> Result<(), Errors> {
    let mut ctx = Context {
        cseg_org: None,
        dseg_org: None,
        defines: HashMap::new(),
        stream,
        position: 0,
    };

    let mut errors = Vec::new();

    let mut cursor = ctx.cursor();

    while let Some(tok) = cursor.peek() {
        expand_preproc(tok, &mut ctx);
        cursor = ctx.cursor();
    }

    if !errors.is_empty() {
        return Err(errors);
    }

    Ok(())
}

fn expand_preproc(peek: &Token, ctx: &mut Context) -> Result<(), Diagnostic> {
    let mut cursor = ctx.cursor();
    
    use TokenInner as TI;
    match peek.inner {
        TI::Ident(lex::Ident::PreProc(PreProc::Define)) => {
            let def: Define = cursor.parse()?;
            ctx.defines.insert(def.name, def.value);
        },
        TI::Ident(lex::Ident::PreProc(PreProc::If)) => {
            let expanded = eval_if(ctx)?;
            ctx.stream.splice(expanded.1, expanded.0);
        }
        TI::Ident(lex::Ident::PreProc(PreProc::Org)) => {
            let origin = Org::parse(&mut cursor)?;
            if ctx.cseg_org.is_some() {
                return Err(spanned_error!(origin.span, "Duplicate definitions of origin")
                        .with_help("`@org` can only be used once per program"));
            } else {
                ctx.cseg_org = Some(origin.address);
            }
        },
        _ => match_errors!(out, errors, cursor.next().unwrap().clone().try_into()),
    }

    Ok(())
}

fn eval_if(ctx: &mut Context) -> Result<(TokenStream, ops::Range<usize>), Diagnostic> {
    use TokenInner as TI;

    let mut cursor = ctx.cursor();

    let start = ctx.position;
    let if_span = cursor
        .peek()
        .ok_or_else(|| {
            error!("`parse::eval_if` called without tokens").with_help(
                "This is a bug. Please report this at https://github.com/commonkestrel/assembler",
            )
        })?
        .span;

    let eval = if_expr(&mut cursor)?;

    let mut out = Vec::new();
    let mut depth = 0;

    while let Some(tok) = cursor.peek() {
        match tok.inner {
            TI::Ident(lex::Ident::PreProc(PreProc::If))
            | TI::Ident(lex::Ident::PreProc(PreProc::IfDef)) => {
                _ = cursor.next();
                depth += 1;
            }
            TI::Ident(lex::Ident::PreProc(PreProc::EndIf)) => {
                _ = cursor.next();
                if depth == 0 {
                    break;
                } else {
                    depth -= 1;
                }
            }
            TI::Ident(lex::Ident::PreProc(PreProc::ElIf)) => {
                if depth == 0 {
                    if eval {
                        return cursor
                            .position(|tok| {
                                matches!(tok.inner, TI::Ident(lex::Ident::PreProc(PreProc::EndIf)))
                            })
                            .map(|end| (out, (start..end + 1)))
                            .ok_or_else(|| {
                                spanned_error!(
                                    if_span.clone(),
                                    "Unmatched `@if` statement, expected `@endif`, found `EOL`"
                                )
                            });
                    } else {
                        return eval_if(ctx);
                    }
                }
            }
            TI::Ident(lex::Ident::PreProc(PreProc::Else)) => {
                if depth == 0 {
                    if eval {
                        return cursor
                            .position(|tok| {
                                matches!(tok.inner, TI::Ident(lex::Ident::PreProc(PreProc::EndIf)))
                            })
                            .map(|end| (out, start..end + 1))
                            .ok_or_else(|| {
                                spanned_error!(
                                    if_span.clone(),
                                    "Unmatched `@if` statement, expected `@endif`, found `EOL`"
                                )
                            });
                    } else {
                        while let Some(more) = cursor.next() {
                            if matches!(more.inner, TI::Ident(lex::Ident::PreProc(PreProc::EndIf))) {
                                
                            } else {
                                
                            }
                        };
                        return Err(error!(""));
                    }
                }
            }
            _ => {
                if eval {
                    out.push(tok.clone());
                }
            }
        }
    }

    Ok((out, start..cursor.position))
}

fn if_expr(
    cursor: &mut Cursor,
    defines: &HashMap<String, TokenStream>,
) -> Result<bool, Diagnostic> {
    let start = cursor.position;
    let end = cursor
        .position(|tok| tok.inner == TokenInner::NewLine)
        .ok_or_else(|| {
            spanned_error!(
                cursor.buffer[cursor.buffer.len() - 1].span.clone(),
                "expected newline after `@if` expression, found `EOF`"
            )
        })?;

    if start == end {
        return Err(spanned_error!(
            cursor.buffer[start].span.clone(),
            "expected expression, found newline"
        ));
    }

    let eval = eval::eval_no_paren(&cursor.buffer[start..end], defines)?;

    Ok(eval > 0)
}

pub trait Parsable: Sized {
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic>;
}

#[derive(Debug, Clone, PartialEq)]
pub struct Org {
    span: Span,
    address: u16,
}

impl Parsable for Org {
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
        let org: Token![@ORG] = cursor.parse()?;
        let addr: Immediate = cursor.parse()?;
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Define {
    pub name: String,
    pub value: TokenStream,
}

impl Parsable for Define {
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
        let _def: Token![@DEFINE] = cursor.parse()?;
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
        let mut l = match lexed {
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
            value: Cursor::new(&mut l).parse()?,
        })
    }
}

pub enum Expanded {
    Instruction(Instruction),
    Label(String),
    SubLabel(String),
    Variable(Variable),
}

pub enum Instruction {
    Add(Register, RegImm),
    Adc(Register, RegImm),
    Sub(Register, RegImm),
    Sbc(Register, RegImm),
    Nand(Register, RegImm),
    Or(Register, RegImm),
    Cmp(Register, RegImm),
    Mv(Register, RegImm),
    Ld(Register, Option<u16>),
    St(Register, Option<u16>),
    Lda(u16),
    Push(RegImm),
    Pop(Register),
    Jnz(Address),
    In(Register, RegImm),
    Out(RegImm, Register),
}

impl Instruction {
    pub fn len(&self) -> u16 {
        use Instruction as I;

        match self {
            I::Add(_, _) => 2,
            I::Adc(_, _) => 2,
            I::Sub(_, _) => 2,
            I::Sbc(_, _) => 2,
            I::Nand(_, _) => 2,
            I::Or(_, _) => 2,
            I::Cmp(_, _) => 2,
            I::Mv(_, _) => 2,
            I::Ld(_, Some(_)) => 3,
            I::Ld(_, None) => 1,
            I::St(_, Some(_)) => 3,
            I::St(_, None) => 1,
            I::Lda(_) => 3,
            I::Push(_) => 2,
            I::Pop(_) => 1,
            I::Jnz(_) => 3,
            I::In(_, _) => 2,
            I::Out(_, _) => 2,
        }
    }
}

pub enum RegImm {
    Register(Register),
    Immediate(u8),
}

pub enum Address {
    Literal(u16),
    Label(Label),
}

pub struct Label {
    pub span: Span,
    pub name: String,
}

pub struct Variable {
    span: Span,
}

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
