use crate::{
    diagnostic::Diagnostic,
    lex::{Delimeter, Span, Token, TokenInner, TokenStream},
};

pub fn eval_expr(tokens: TokenStream) -> Result<Token, Diagnostic> {
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

    todo!()
}
