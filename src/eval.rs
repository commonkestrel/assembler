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
//! It also supports boolean opperations (treating 0 as `false` and anything greater than 0 as `true`):
//! - Equality (`==`)
//! - Inequality (`!=`)
//! - Less than (`<`)
//! - Less than or equal to (`<=`)
//! - Greater than (`>`)
//! - Greater than or equal to (`>=`)
//! - Logical And (`&&`)
//! - Logical Or (`||`)
//!
//! Precidence is as follows:
//! 1. `(...)`, `!`, `~`
//! 2. `*` and `/`
//! 3. `+`, `-`, `&`, `|`, `^`, `<<`, `>>`
//! 4. `==`, `!=`, `<`, `<=`, `>`, `>=`
//! 5. `&&`, `||`
//!
//! Operators with the same precidence are evaluated left to right.
//! Defines are treated as if they are
//! wrapped in a set of parenthases.
//!
//! Macros are not allowed in expressions.
//!
//! Floating point arithmetic is not planned.
//!
//! # Examples
//! ```
//! let expr = lex_string("12*3 == 6*6").unwrap();
//! let eval = eval_no_paren(&expr);
//! assert_eq!(eval > 0, 12*3 == 6*6);
//! ```

use crate::{
    diagnostic::Diagnostic,
    error,
    lex::{Delimeter, Ident, Punctuation, Span, Token, TokenInner, TokenStream},
    parse::Define,
    spanned_error,
};
use std::{collections::HashMap, iter::Peekable};
use std::{fmt, slice::Iter};

pub fn eval_expr(
    tokens: &[Token],
    defines: &HashMap<String, TokenStream>,
) -> Result<Token, Diagnostic> {
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
            return Err(error!(
                "Expressions must be wrapped in a set of parentheses",
            ))
        }
    };

    #[cfg(test)]
    println!("{}", Tree::parse(tokens, defines)?);

    Tree::parse(tokens, defines).map(|tree| Token {
        inner: TokenInner::Immediate(tree.eval()),
        span,
    })
}

pub fn eval_no_paren(
    tokens: &[Token],
    defines: &HashMap<String, TokenStream>,
) -> Result<i128, Diagnostic> {
    #[cfg(test)]
    println!("{}", Tree::parse(tokens, defines)?);

    Tree::parse(tokens, defines).map(|tree| tree.eval())
}

#[derive(Debug, Clone, PartialEq)]
struct BinOp {
    left: Tree,
    right: Tree,
}

impl BinOp {
    #[inline]
    fn new(left: Tree, right: Tree) -> BinOp {
        BinOp { left, right }
    }

    fn boxed(left: Tree, right: Tree) -> Box<BinOp> {
        Box::new(BinOp::new(left, right))
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Tree {
    Literal(i128),
    Define { inner: Box<Tree> },
    Add(Box<BinOp>),
    Sub(Box<BinOp>),
    Mul(Box<BinOp>),
    Div(Box<BinOp>),
    And(Box<BinOp>),
    Or(Box<BinOp>),
    Xor(Box<BinOp>),
    Shl(Box<BinOp>),
    Shr(Box<BinOp>),
    Not { value: Box<Tree> },
    Eq(Box<BinOp>),
    Ne(Box<BinOp>),
    Lt(Box<BinOp>),
    Le(Box<BinOp>),
    Gt(Box<BinOp>),
    Ge(Box<BinOp>),
    CmpAnd(Box<BinOp>),
    CmpOr(Box<BinOp>),
}

impl Tree {
    fn parse(tokens: &[Token], defines: &HashMap<String, TokenStream>) -> Result<Tree, Diagnostic> {
        let mut iter = tokens.iter().peekable();
        Tree::parse_c(&mut iter, defines)
    }

    /// Parses a comparison
    fn parse_c(
        tokens: &mut Peekable<Iter<Token>>,
        defines: &HashMap<String, TokenStream>,
    ) -> Result<Tree, Diagnostic> {
        use TokenInner as TI;

        let mut a = Tree::parse_b(tokens, defines)?;

        while let Some(tok) = tokens.peek() {
            match tok.inner {
                TI::Punctuation(Punctuation::AndAnd) => {
                    tokens.next();
                    let b = Tree::parse_b(tokens, defines)?;
                    a = Tree::CmpAnd(BinOp::boxed(a, b));
                }
                TI::Punctuation(Punctuation::OrOr) => {
                    tokens.next();
                    let b = Tree::parse_b(tokens, defines)?;
                    a = Tree::CmpOr(BinOp::boxed(a, b));
                }
                _ => return Ok(a),
            }
        }

        Ok(a)
    }

    /// Parses a boolean operator
    fn parse_b(
        tokens: &mut Peekable<Iter<Token>>,
        defines: &HashMap<String, TokenStream>,
    ) -> Result<Tree, Diagnostic> {
        use TokenInner as TI;

        let mut a = Tree::parse_e(tokens, defines)?;

        while let Some(tok) = tokens.peek() {
            match tok.inner {
                TI::Punctuation(Punctuation::EqEq) => {
                    tokens.next();
                    let b = Tree::parse_e(tokens, defines)?;
                    a = Tree::Eq(BinOp::boxed(a, b));
                }
                TI::Punctuation(Punctuation::Ne) => {
                    tokens.next();
                    let b = Tree::parse_e(tokens, defines)?;
                    a = Tree::Ne(BinOp::boxed(a, b));
                }
                TI::Punctuation(Punctuation::Lt) => {
                    tokens.next();
                    let b = Tree::parse_e(tokens, defines)?;
                    a = Tree::Lt(BinOp::boxed(a, b));
                }
                TI::Punctuation(Punctuation::Le) => {
                    tokens.next();
                    let b = Tree::parse_e(tokens, defines)?;
                    a = Tree::Le(BinOp::boxed(a, b));
                }
                TI::Punctuation(Punctuation::Gt) => {
                    tokens.next();
                    let b = Tree::parse_e(tokens, defines)?;
                    a = Tree::Gt(BinOp::boxed(a, b));
                }
                TI::Punctuation(Punctuation::Ge) => {
                    tokens.next();
                    let b = Tree::parse_e(tokens, defines)?;
                    a = Tree::Ge(BinOp::boxed(a, b));
                }
                _ => return Ok(a),
            }
        }

        Ok(a)
    }

    /// Parses an expression.
    fn parse_e(
        tokens: &mut Peekable<Iter<Token>>,
        defines: &HashMap<String, TokenStream>,
    ) -> Result<Tree, Diagnostic> {
        use TokenInner as TI;

        let mut a = Tree::parse_t(tokens, defines)?;

        while let Some(tok) = tokens.peek() {
            match tok.inner {
                TI::Punctuation(Punctuation::Plus) => {
                    tokens.next();
                    let b = Tree::parse_t(tokens, defines)?;
                    a = Tree::Add(BinOp::boxed(a, b));
                }
                TI::Punctuation(Punctuation::Minus) => {
                    tokens.next();
                    let b = Tree::parse_t(tokens, defines)?;
                    a = Tree::Sub(BinOp::boxed(a, b));
                }
                TI::Punctuation(Punctuation::And) => {
                    tokens.next();
                    let b = Tree::parse_t(tokens, defines)?;
                    a = Tree::And(BinOp::boxed(a, b));
                }
                TI::Punctuation(Punctuation::Or) => {
                    tokens.next();
                    let b = Tree::parse_t(tokens, defines)?;
                    a = Tree::Or(BinOp::boxed(a, b));
                }
                TI::Punctuation(Punctuation::Caret) => {
                    tokens.next();
                    let b = Tree::parse_t(tokens, defines)?;
                    a = Tree::Xor(BinOp::boxed(a, b));
                }
                TI::Punctuation(Punctuation::Shl) => {
                    tokens.next();
                    let b = Tree::parse_t(tokens, defines)?;
                    a = Tree::Shl(BinOp::boxed(a, b));
                }
                TI::Punctuation(Punctuation::Shr) => {
                    tokens.next();
                    let b = Tree::parse_t(tokens, defines)?;
                    a = Tree::Shr(BinOp::boxed(a, b));
                }
                _ => return Ok(a),
            }
        }

        Ok(a)
    }

    /// Parses a terminal.
    fn parse_t(
        tokens: &mut Peekable<Iter<Token>>,
        defines: &HashMap<String, TokenStream>,
    ) -> Result<Tree, Diagnostic> {
        use TokenInner as TI;

        let mut a = Tree::parse_f(tokens, defines)?;

        while let Some(tok) = tokens.peek() {
            match tok.inner {
                TI::Punctuation(Punctuation::Star) => {
                    tokens.next();
                    let b = Tree::parse_f(tokens, defines)?;
                    a = Tree::Mul(BinOp::boxed(a, b));
                }
                TI::Punctuation(Punctuation::Slash) => {
                    tokens.next();
                    let b = Tree::parse_f(tokens, defines)?;
                    a = Tree::Div(BinOp::boxed(a, b));
                }
                _ => return Ok(a),
            }
        }

        Ok(a)
    }

    /// Parses a factor.
    fn parse_f(
        tokens: &mut Peekable<Iter<Token>>,
        defines: &HashMap<String, TokenStream>,
    ) -> Result<Tree, Diagnostic> {
        use TokenInner as TI;
        match tokens.next() {
            Some(tok) => match &tok.inner {
                TI::Immediate(imm) => Ok(Tree::Literal(*imm)),
                TI::Ident(id) => match id {
                    Ident::Ident(name) => {
                        return if let Some(tok) = defines.get(name) {
                            Tree::parse(tok, defines)
                        } else {
                            Err(spanned_error!(
                                tok.span.clone(),
                                "Identifier `{name}` is not defined.",
                            )
                            .with_help("Constants must be defined before they are used."))
                        };
                    }
                    _ => Err(Diagnostic::spanned_error(
                        tok.span.clone(),
                        "Unexpected identifier",
                    )),
                },
                TI::Delimeter(Delimeter::OpenParen) => {
                    let a = Tree::parse_c(tokens, defines)?;
                    if let Some(tok) = tokens.next() {
                        if let TI::Delimeter(Delimeter::ClosedParen) = tok.inner {
                            Ok(a)
                        } else {
                            Err(Diagnostic::spanned_error(
                                tok.span.clone(),
                                "Expected matching `)` at end of expression",
                            ))
                        }
                    } else {
                        Err(Diagnostic::error("No closing parenthesis for expression"))
                    }
                }
                TI::Punctuation(Punctuation::Not) => {
                    return Ok(Tree::Not {
                        value: Box::new(Tree::parse_f(tokens, defines)?),
                    })
                }
                _ => todo!(),
            },
            None => Err(Diagnostic::error("No tokens found for factor")),
        }
    }

    fn eval(&self) -> i128 {
        use Tree as T;
        match self {
            T::Literal(lit) => *lit,
            T::Define { inner } => inner.eval(),
            T::Add(bin) => bin.left.eval() + bin.right.eval(),
            T::Sub(bin) => bin.left.eval() - bin.right.eval(),
            T::Mul(bin) => bin.left.eval() * bin.right.eval(),
            T::Div(bin) => bin.left.eval() / bin.right.eval(),
            T::And(bin) => bin.left.eval() & bin.right.eval(),
            T::Or(bin) => bin.left.eval() | bin.right.eval(),
            T::Xor(bin) => bin.left.eval() ^ bin.right.eval(),
            T::Shl(bin) => bin.left.eval() << bin.right.eval(),
            T::Shr(bin) => bin.left.eval() >> bin.right.eval(),
            T::Not { value } => !value.eval(),
            T::Eq(bin) => (bin.left.eval() == bin.right.eval()) as i128,
            T::Ne(bin) => (bin.left.eval() != bin.right.eval()) as i128,
            T::Lt(bin) => (bin.left.eval() < bin.right.eval()) as i128,
            T::Le(bin) => (bin.left.eval() <= bin.right.eval()) as i128,
            T::Gt(bin) => (bin.left.eval() > bin.right.eval()) as i128,
            T::Ge(bin) => (bin.left.eval() >= bin.right.eval()) as i128,
            T::CmpAnd(bin) => ((bin.left.eval() > 0) && (bin.right.eval() > 0)) as i128,
            T::CmpOr(bin) => ((bin.left.eval() > 0) || (bin.right.eval() > 0)) as i128,
        }
    }
}

impl fmt::Display for Tree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Tree as T;
        match self {
            T::Literal(inner) => write!(f, "{inner}"),
            T::Define { inner } => write!(f, "{inner}"),
            T::Add(bin) => write!(f, "({}+{})", bin.left, bin.right),
            T::Sub(bin) => write!(f, "({}-{})", bin.left, bin.right),
            T::Mul(bin) => write!(f, "({}*{})", bin.left, bin.right),
            T::Div(bin) => write!(f, "({}/{})", bin.left, bin.right),
            T::And(bin) => write!(f, "({}+{})", bin.left, bin.right),
            T::Xor(bin) => write!(f, "({}+{})", bin.left, bin.right),
            T::Shl(bin) => write!(f, "({}+{})", bin.left, bin.right),
            T::Shr(bin) => write!(f, "({}+{})", bin.left, bin.right),
            T::Or(bin) => write!(f, "({}+{})", bin.left, bin.right),
            T::Not { value } => write!(f, "!{value}"),
            T::Eq(bin) => write!(f, "({}=={})", bin.left, bin.right),
            T::Ne(bin) => write!(f, "({}!={})", bin.left, bin.right),
            T::Lt(bin) => write!(f, "({}<{})", bin.left, bin.right),
            T::Le(bin) => write!(f, "({}<={})", bin.left, bin.right),
            T::Gt(bin) => write!(f, "({}>{})", bin.left, bin.right),
            T::Ge(bin) => write!(f, "({}>={})", bin.left, bin.right),
            T::CmpAnd(bin) => write!(f, "({}&&{})", bin.left, bin.right),
            T::CmpOr(bin) => write!(f, "({}||{})", bin.left, bin.right),
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

    fn test_expr(
        expr: &str,
        defines: Option<HashMap<String, TokenStream>>,
    ) -> Result<i128, Diagnostic> {
        let tokens = match crate::lex::lex_string(expr) {
            Ok(tok) => tok,
            Err(errors) => {
                for err in errors {
                    err.force_emit();
                }
                Diagnostic::error(format!("Unable to lex `{expr}` due to previous errors"))
                    .scream();
            }
        };

        let expr = &tokens[trim_expr(&tokens)?];
        let eval = eval_expr(expr, &defines.unwrap_or_default())?;

        match eval {
            Token {
                span: _,
                inner: TokenInner::Immediate(result),
            } => Ok(result),
            _ => unimplemented!(),
        }
    }

    #[test]
    fn addition() {
        let eval = test_expr("(3+4+5)", None).expect_or_scream("Unable to evaluate `(3+4+5)`");

        assert_eq!(eval, 3 + 4 + 5);
    }

    #[test]
    fn pemdas() {
        let eval =
            test_expr("(3 * (3 + 4) - 5)", None).expect_or_scream("Unable to evaluate `(3+4+5)`");

        assert_eq!(eval, 3 * (3 + 4) - 5);
    }

    #[test]
    fn define() {
        let defines = [(
            "TEST_DEFINE".to_owned(),
            crate::lex::lex_string("(3+4)").expect_or_scream("Unable to lex `3+4`"),
        )];
        let eval = test_expr("(3 * 6 - TEST_DEFINE)", Some(defines.into()))
            .expect_or_scream("Unable to evaluate `(3*6 - TEST_DEFINE)`");

        assert_eq!(eval, 3 * 6 - (3 + 4));
    }

    #[test]
    fn cmp_true() {
        let eval = match test_expr("(3*16 <= 4*16 && 3+3 == 2+4)", None) {
            Ok(e) => e,
            Err(err) => err.scream(),
        };

        assert_eq!(eval > 0, 3 * 16 <= 4 * 16 && 3 + 3 == 2 + 4)
    }

    #[test]
    fn cmp_false() {
        let eval = match test_expr("(3*16 <= 4*16 && 3+3 != 2+4)", None) {
            Ok(e) => e,
            Err(err) => err.scream(),
        };

        assert_eq!(eval > 0, 3 * 16 <= 4 * 16 && 3 + 3 != 2 + 4)
    }

    #[test]
    #[should_panic]
    fn no_paren() {
        let eval = test_expr("3+4+5", None).expect_or_scream("Unable to evaluate `(3+4+5)`");
    }

    #[test]
    fn bitshift() {
        let eval = match test_expr("((1<<3) | (1<<5))", None) {
            Ok(ok) => ok,
            Err(err) => err.scream(),
        };

        assert_eq!(eval, (1 << 3) | (1 << 5));
    }
}
