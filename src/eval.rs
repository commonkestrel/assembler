//! This is my best effort at implementing a recursive decent parser to
//! evaluate math expressions at the same time as pre-proc arguments.
//! 
//! This just supports the most important operations for integers:
//! - Addition (`+`)
//! - Subtraction (`-`)
//! - Multiplication (`*`)
//! - Division (`/`)
//! - Bitwise And (`&`)
//! - Bitwise Or (`|`)
//! - Bitwise Xor (`^`)
//! - Bitwise Not (`!` or `~`)
//! - Logical Shift Left (`<<`)
//! - Logical Shift Right (`>>`)
//! 
//! Precidence is as follows:
//! 1. `(...)`, `!`, `~`
//! 2. `*` and `/`
//! 3. `+`, `-`, `&`, `|`, `^`, `<<`, `>>`
//! 
//! Operators with the same precidence are evaluated left to right.
//! 
//! Floating point arithmetic is not planned.

use crate::{
    diagnostic::Diagnostic,
    lex::{Delimeter, Span, Token, TokenInner, Ident, Punctuation},
    parse::Define,
};
use std::{slice::Iter, fmt};
use std::iter::Peekable;

pub fn eval_expr(tokens: &[Token], defines: &[Define]) -> Result<Token, Diagnostic> {
    let span = match (tokens.first(), tokens.last()) {
        (
            Some(Token {
                inner: TokenInner::Delimeter(Delimeter::OpenParen),
                span: open_span,
            }),
            Some(Token {
                inner: TokenInner::Delimeter(Delimeter::ClosedParen),
                span: close_span,
            }),
        ) => Span {
            line: open_span.line,
            range: open_span.range.start..close_span.range.end,
            source: open_span.source.clone(),
        },
        _ => {
            return Err(Diagnostic::error(
                "Expressions must be wrapped in a set of parentheses",
            ))
        }
    };

    Tree::parse(tokens, defines).map(|tree| Token{ inner: TokenInner::Immediate(tree.eval()), span })
}

#[derive(Debug, Clone, PartialEq)]
enum Tree {
    Literal(u64),
    Define { inner: Box<Tree> },
    Add { left: Box<Tree>, right: Box<Tree> },
    Sub { left: Box<Tree>, right: Box<Tree> },
    Mul { left: Box<Tree>, right: Box<Tree> },
    Div { left: Box<Tree>, right: Box<Tree> },
    And { left: Box<Tree>, right: Box<Tree> },
    Or { left: Box<Tree>, right: Box<Tree> },
    Xor { left: Box<Tree>, right: Box<Tree> },
    Shl { left: Box<Tree>, right: Box<Tree> },
    Shr { left: Box<Tree>, right: Box<Tree> },
    Not { value: Box<Tree> },
}

impl Tree {
    fn parse(tokens: &[Token], defines: &[Define]) -> Result<Tree, Diagnostic> {
        let mut iter = tokens.iter().peekable();
        Tree::parse_f(&mut iter, defines)
    }

    /// Parses an expression.
    fn parse_e(tokens: &mut Peekable<Iter<Token>>, defines: &[Define]) -> Result<Tree, Diagnostic> {
        use TokenInner as TI;

        let mut a = Tree::parse_t(tokens, defines)?;

        while let Some(tok) = tokens.peek() {
            match tok.inner {
                TI::Punctuation(Punctuation::Plus) => {
                    tokens.next();
                    let b = Tree::parse_t(tokens, defines)?;
                    a = Tree::Add{ left: Box::new(a), right: Box::new(b) };
                },
                TI::Punctuation(Punctuation::Minus) => {
                    tokens.next();
                    let b = Tree::parse_t(tokens, defines)?;
                    a = Tree::Sub { left: Box::new(a), right: Box::new(b) }
                },
                TI::Punctuation(Punctuation::And) => {
                    tokens.next();
                    let b = Tree::parse_t(tokens, defines)?;
                    a = Tree::And { left: Box::new(a), right: Box::new(b) }
                },
                TI::Punctuation(Punctuation::Or) => {
                    tokens.next();
                    let b = Tree::parse_t(tokens, defines)?;
                    a = Tree::Or { left: Box::new(a), right: Box::new(b) }
                },
                TI::Punctuation(Punctuation::Caret) => {
                    tokens.next();
                    let b = Tree::parse_t(tokens, defines)?;
                    a = Tree::Xor { left: Box::new(a), right: Box::new(b) }
                },
                TI::Punctuation(Punctuation::Shl) => {
                    tokens.next();
                    let b = Tree::parse_t(tokens, defines)?;
                    a = Tree::Shl { left: Box::new(a), right: Box::new(b) }
                },
                TI::Punctuation(Punctuation::Shr) => {
                    tokens.next();
                    let b = Tree::parse_t(tokens, defines)?;
                    a = Tree::Shr { left: Box::new(a), right: Box::new(b) }
                }
                _ => return Ok(a)
            }
        }

        Err(Diagnostic::error("Expected full expression, found `eol`"))
    }

    /// Parses a terminal.
    fn parse_t(tokens: &mut Peekable<Iter<Token>>, defines: &[Define]) -> Result<Tree, Diagnostic> {
        use TokenInner as TI;

        let mut a = Tree::parse_f(tokens, defines)?;

        while let Some(tok) = tokens.peek() {
            match tok.inner {
                TI::Punctuation(Punctuation::Star) => {
                    tokens.next();
                    let b = Tree::parse_f(tokens, defines)?;
                    a = Tree::Mul{ left: Box::new(a), right: Box::new(b) };
                },
                TI::Punctuation(Punctuation::Slash) => {
                    tokens.next();
                    let b = Tree::parse_f(tokens, defines)?;
                    a = Tree::Div{ left: Box::new(a), right: Box::new(b) };
                },
                _ => return Ok(a),
            }
        }

        Err(Diagnostic::error("Expected full expression, found `eol`"))
    }

    /// Parses a factor.
    fn parse_f(tokens: &mut Peekable<Iter<Token>>, defines: &[Define]) -> Result<Tree, Diagnostic> {
        use TokenInner as TI;
        match tokens.next() {
            Some(tok) => match &tok.inner {
                TI::Immediate(imm) => Ok(Tree::Literal(*imm)),
                TI::Ident(id) => match id {
                    Ident::Ident(name) => {
                        for def in defines {
                            if def.name == name.as_ref() {
                                return Ok(Tree::parse(&def.value, defines)?);
                            }
                        }
                        Err(Diagnostic::spanned_error(tok.span.clone(), format!("Identifier `{name}` is not defined.")).with_help("Constants must be defined before they are used."))
                    },
                    _ => Err(Diagnostic::spanned_error(tok.span.clone(), "Unexpected identifier"))
                },
                TI::Delimeter(Delimeter::OpenParen) => {
                    let a = Tree::parse_e(tokens, defines)?;
                    if let Some(tok) = tokens.next() {
                        if let TI::Delimeter(Delimeter::ClosedParen) = tok.inner {
                            Ok(a)
                        } else {
                            Err(Diagnostic::spanned_error(tok.span.clone(), "Expected matching `)` at end of expression"))
                        }
                    } else {
                        Err(Diagnostic::error("No closing parenthesis for expression"))
                    }
                },
                TI::Punctuation(Punctuation::Not) => {
                    return Ok(Tree::Not{ value: Box::new(Tree::parse_f(tokens, defines)?) })
                }
                _ => todo!(),
            },
            None => Err(Diagnostic::error("No tokens found for factor")),
        }
    }

    fn eval(&self) -> u64 {
        use Tree as T;
        match self {
            T::Literal(lit) => *lit,
            T::Define{ inner } => inner.eval(),
            T::Add{ left, right } => left.eval() + right.eval(),
            T::Sub{ left, right } => left.eval() - right.eval(),
            T::Mul{ left, right } => left.eval() * right.eval(),
            T::Div{ left, right } => left.eval() / right.eval(),
            T::And{ left, right } => left.eval() & right.eval(),
            T::Or { left, right } => left.eval() | right.eval(),
            T::Xor { left, right } => left.eval() ^ right.eval(),
            T::Shl { left, right } => left.eval() << right.eval(),
            T::Shr { left, right } => left.eval() >> right.eval(),
            T::Not{ value } => !value.eval() ,
        }
    }
}

impl fmt::Display for Tree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Tree as T;
        match self {
            T::Literal(inner) => write!(f, "{inner}"),
            T::Define {inner} => write!(f, "{inner}"),
            T::Add { left, right } => write!(f, "({left}+{right})"),
            T::Sub { left, right } => write!(f, "({left}-{right})"),
            T::Mul { left, right } => write!(f, "({left}*{right})"),
            T::Div { left, right } => write!(f, "({left}/{right})"),
            T::And { left, right } => write!(f, "({left}+{right})"),
            T::Or { left, right } => write!(f, "({left}+{right})"),
            T::Xor { left, right } => write!(f, "({left}+{right})"),
            T::Shl { left, right } => write!(f, "({left}+{right})"),
            T::Shr { left, right } => write!(f, "({left}+{right})"),
            T::Not { value } => write!(f, "!{value}"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ResultScream;
    use std::ops::RangeInclusive;

    fn trim_expr(tokens: &[Token]) -> Result<RangeInclusive<usize>, Diagnostic> {
        let mut depth = 0isize;
        let mut start = None;

        for (i, token) in tokens.iter().enumerate() {
            match token.inner {
                TokenInner::Delimeter(Delimeter::OpenParen) => {
                    if depth == 0 {
                        start = Some(i);
                    }
                    depth += 1;
                }
                TokenInner::Delimeter(Delimeter::ClosedParen) => {
                    depth -= 1;
                    if depth == 0 {
                        return Ok(start.unwrap()..=i);
                    }
                    if depth < 0 {
                        return Err(Diagnostic::spanned_error(
                            token.span.clone(),
                            "Unmatched closing parenthesis",
                        ));
                    }
                }
                _ => {}
            }
        }

        Err(Diagnostic::error("Not an expression"))
    }

    #[test]
    fn addition() {
        let tokens = &crate::lex::lex_string("(3+4+5)")
            .expect_or_scream("Unable to lex expression `(3+4+5)`");
        let expr = &tokens[trim_expr(&tokens).unwrap()];
        let eval = eval_expr(expr, &[]).expect_or_scream("Unable to evaluate expression `(3+4+5)`");

        assert_eq!(eval.inner, TokenInner::Immediate(3 + 4 + 5));
    }

    #[test]
    fn pemdas() {
        let tokens = &crate::lex::lex_string("(3 * (3 + 4) - 5)")
            .expect_or_scream("Unable to lex expression `(3 * (3 + 4) - 5)`");
        let expr = &tokens[trim_expr(&tokens).unwrap()];
        let eval = eval_expr(expr, &[]).expect_or_scream("Unable to evaluate expression `(3 * (3 + 4) - 5)`");

        assert_eq!(eval.inner, TokenInner::Immediate(3 * (3 + 4) - 5));
    }
}
