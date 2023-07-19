use std::str::FromStr;
use std::fmt;
use crate::{ascii::{self, caret_decode, AsciiStr}, diagnostic::Diagnostic};
use super::Span;
use std::error::Error;

pub struct Ident {
    value: String, 
    span: Span,
}

macro_rules! define_keywords {
    ($($string:literal $vis:vis struct $name:ident)*) => {
        $(
            $vis struct $name {
                span: $crate::lexer::Span
            }


        )*
    }
}

define_keywords![

];

pub struct Lit {
    value: Literal,
    span: Span,
}

macro_rules! litint_impl {
    ($name:ident, $varient:path) => {
        pub fn $name<T>(span: Span, value: T) -> Lit
        where
            T: Into<u8>
        {
            Lit {
                value: Literal::Integer($varient(value.into())),
                span,
            }
        }
    };
}

impl Lit {
    litint_impl!(binary, LitInt::Binary);
    litint_impl!(octal, LitInt::Octal);
    litint_impl!(decimal, LitInt::Decimal);
    litint_impl!(hex, LitInt::Hexadecimal);

    pub fn ascii(span: Span, string: &[u8]) -> Lit {
        Lit {
            value: Literal::String(unsafe { AsciiStr::from_buffer_unchecked(string) }),
            span,
        }
    }

    pub fn str<T>(span: Span, string: T) -> Result<Lit, ()>
    where
        T: Into<String>
     {
        Ok(Lit {
            value: Literal::String(AsciiStr::from_buffer(string.into().as_bytes())?),
            span,
        })
    }

    pub fn bool(span: Span, value: bool) -> Lit {
        Lit {
            value: Literal::Bool(value),
            span,
        }
    }
}
#[derive(Clone, PartialEq, Debug)]
pub enum Literal {
    /// Integer literal (e.g. `247`, `0b1111_0111`, `0xF7`, `0o367`).
    Integer(LitInt),
    /// ASCII string literal (e.g. `"hello world!"`).
    String(AsciiStr),
    /// A boolean literal (e.g. `true` or `false`).
    Bool(bool),
    /// An ASCII character (e.g. `'b'`, `'%'`).
    ///
    /// Uses [caret notation](https://en.wikipedia.org/wiki/Caret_notation)
    /// for untypable ASCII values (e.g. `'^?'` == `NUL`).
    Char(u8),
}

#[derive(Clone, PartialEq, Debug)]
enum LiteralError {
    /// String is not valid ASCII
    StringNotAscii,
    /// Token is not recognized as a valid literal.
    NotRecognized,
    /// Error parsing integer literal as `u8`.
    IntegerError(std::num::ParseIntError),
    /// Character literal is not valid ASCII.
    CharNotAscii,
    /// Invalid character length.
    ///
    /// Only a length of 1 is allowed
    /// (2 for caret notation).
    InvalidCharLength(usize),
}

impl fmt::Display for LiteralError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LiteralError::StringNotAscii => write!(f, "string literal is not valid ASCII"),
            LiteralError::NotRecognized => write!(f, "token not recognized as a valid literal"),
            LiteralError::IntegerError(err) => write!(f, "unable to parse integer literal as `u8`: {err}"),
            LiteralError::CharNotAscii => write!(f, "character literal is not valid ASCII"),
            LiteralError::InvalidCharLength(len) => write!(f, "expected a character length of 1 (2 for caret notation), found character of length {len}"),
        }
    }
}

impl Error for LiteralError {}

impl From<std::num::ParseIntError> for LiteralError {
    fn from(value: std::num::ParseIntError) -> Self {
        LiteralError::IntegerError(value)
    }
}

impl From<ascii::CharDecodeError> for LiteralError {
    fn from(value: ascii::CharDecodeError) -> Self {
        match value {
            ascii::CharDecodeError::NotAscii => Self::CharNotAscii,
            ascii::CharDecodeError::InvalidLength(len) => Self::InvalidCharLength(len),
        }
    }
}

impl FromStr for Literal {
    type Err = LiteralError;

    fn from_str(imut_s: &str) -> Result<Self, Self::Err> {
        let mut s = imut_s.to_owned();
        if s.starts_with('"') && s.ends_with('"') {
            s = s[1..s.len() - 1].to_owned();
            Ok(Literal::String(
                s.try_into().map_err(|_| LiteralError::StringNotAscii)?,
            ))
        } else if s.starts_with('0') || s.chars().all(char::is_numeric) {
            Ok(Literal::Integer(LitInt::from_str(&s)?))
        } else if s.starts_with('\'') && s.starts_with('\'') {
            s = s[1..s.len() - 1].to_owned();
            Ok(Literal::Char(caret_decode(&s)?))
        } else if s == "true" {
            Ok(Literal::Bool(true))
        } else if s == "false" {
            Ok(Literal::Bool(false))
        } else {
            Err(LiteralError::NotRecognized)
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum LitInt {
    Binary(u8),
    Octal(u8),
    Decimal(u8),
    Hexadecimal(u8),
}

impl PartialOrd for LitInt {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.extract().cmp(&other.extract()))
    }
}

impl LitInt {
    fn extract(&self) -> u8 {
        match self {
            LitInt::Binary(n) => *n,
            LitInt::Octal(n) => *n,
            LitInt::Decimal(n) => *n,
            LitInt::Hexadecimal(n) => *n,
        }
    }
}

impl FromStr for LitInt {
    type Err = std::num::ParseIntError;

    fn from_str(imut_s: &str) -> Result<Self, Self::Err> {
        let s = if imut_s.starts_with("_") || imut_s.ends_with("_") {
            imut_s.replace("_", "")
        } else {
            imut_s.to_owned()
        };

        if s.starts_with("0b") && s[2..].chars().all(|c| c == '0' || c == '1') {
            Ok(LitInt::Binary(u8::from_str_radix(&s[2..], 2)?))
        } else if s.starts_with("0o") && s[2..].chars().all(|c| c >= '0' && c < '8') {
            Ok(LitInt::Octal(u8::from_str_radix(&s[2..], 8)?))
        } else if s.starts_with("0x") && s[2..].chars().all(|c| c.is_ascii_hexdigit()) {
            Ok(LitInt::Hexadecimal(u8::from_str_radix(&s[2..], 16)?))
        } else {
            Ok(LitInt::Decimal(u8::from_str(&s)?))
        }
    }
}



#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn integers() {
        assert_eq!(
            Literal::from_str("247"),
            Ok(Literal::Integer(LitInt::Decimal(247)))
        );
    }
}


