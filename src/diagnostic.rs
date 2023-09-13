use crate::VERBOSITY;

use super::lex::Span;

use colored::{ColoredString, Colorize};
use std::{
    fmt, fs,
    io::{self, Write},
};

#[derive(Debug, Clone, PartialEq)]
enum Location {
    Span(Span),
    Panic {
        path: String,
        line: u32,
        column: u32,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Diagnostic {
    /// Diagnostic priority level.
    level: Level,
    /// Diagnostic message.
    message: String,
    /// Target span.
    location: Option<Location>,
    /// Child messages.
    children: Vec<Child>,
}

macro_rules! diagnostic_levels {
    ($base:ident, $spanned:ident, $panicking:ident, $level:expr) => {
        #[doc = concat!("Creates a new diagnostic with the [`", stringify!($level), "`] level, and the given `message`.")]
        pub fn $base<T>(message: T) -> Self
        where
            T: Into<String>
        {
            Diagnostic {
                level: $level,
                message: message.into(),
                location: None,
                children: Vec::new(),
            }
        }

        #[doc = concat!("Creates a new diagnostic with the [`", stringify!($level), "`] level, and the given `span` and `message`.")]
        pub fn $spanned<T>(span: Span, message: T) -> Self
        where
            T: Into<String>
        {
            Diagnostic {
                level: $level,
                message: message.into(),
                location: Some(Location::Span(span)),
                children: Vec::new(),
            }
        }

        #[doc = concat!("Creates a new diagnostic with the [`", stringify!($level), "`] level, and the given panic `location` and `message`.")]
        pub fn $panicking<'a, T>(location: &'a std::panic::Location<'a>, message: T) -> Self
        where
            T: Into<String>
        {
            Diagnostic {
                level: $level,
                message: message.into(),
                location: Some(Location::Panic {
                    path: location.file().to_owned(),
                    line: location.line(),
                    column: location.column(),
                }),
                children: Vec::new(),
            }
        }
    };
}

impl Diagnostic {
    pub fn new<T>(level: Level, message: T) -> Self
    where
        T: Into<String>,
    {
        Diagnostic {
            level,
            message: message.into(),
            location: None,
            children: Vec::new(),
        }
    }

    pub fn spanned<T>(span: Span, level: Level, message: T) -> Self
    where
        T: Into<String>,
    {
        Diagnostic {
            level,
            message: message.into(),
            location: Some(Location::Span(span)),
            children: Vec::new(),
        }
    }

    pub fn panicking<'a, T>(
        location: &'a std::panic::Location<'a>,
        level: Level,
        message: T,
    ) -> Self
    where
        T: Into<String>,
    {
        Diagnostic {
            level,
            message: message.into(),
            location: Some(Location::Panic {
                path: location.file().to_owned(),
                line: location.line(),
                column: location.column(),
            }),
            children: Vec::new(),
        }
    }

    diagnostic_levels!(error, spanned_error, panicking_error, Level::Error);
    diagnostic_levels!(warning, spanned_warning, panicking_warning, Level::Warning);
    diagnostic_levels!(note, spanned_note, panicking_note, Level::Note);
    diagnostic_levels!(help, spanned_help, panicking_help, Level::Help);

    pub fn level(&self) -> Level {
        self.level
    }

    pub fn set_level(&mut self, level: Level) {
        self.level = level;
    }

    pub fn message(&self) -> &str {
        &self.message
    }

    pub fn set_message<T: Into<String>>(&mut self, message: T) {
        self.message = message.into();
    }

    pub fn span(&self) -> Option<&Span> {
        match self.location {
            Some(Location::Span(ref s)) => Some(s),
            _ => None,
        }
    }

    pub fn set_span(&mut self, span: Span) {
        self.location = Some(Location::Span(span));
    }

    pub fn format_message(&self, long: bool) -> ColoredString {
        if long {
            match self.level {
                Level::Error => format!("{}: {}", "error".red(), self.message).bold(),
                Level::Warning => format!("{}: {}", "warning".yellow(), self.message).bold(),
                Level::Note => format!("note: {}", self.message).bold(),
                Level::Help => {
                    format!("{}: {}", "help".truecolor(150, 150, 255), self.message).bold()
                }
            }
        } else {
            match self.level {
                Level::Error => format!(
                    "{}{} {}",
                    "error".red().bold(),
                    ":".bold(),
                    italic_code(&self.message)
                )
                .normal(),
                Level::Warning => format!(
                    "{}{} {}",
                    "warning".yellow().bold(),
                    ":".bold(),
                    italic_code(&self.message)
                )
                .normal(),
                Level::Note => {
                    format!("{} {}", "note:".bold(), italic_code(&self.message)).normal()
                }
                Level::Help => format!(
                    "{}{} {}",
                    "help".truecolor(150, 150, 255).bold(),
                    ":".bold(),
                    italic_code(&self.message)
                )
                .normal(),
            }
        }
    }

    pub fn emit(self) {
        if self.level <= *VERBOSITY.get().unwrap_or_scream() {
            self.force_emit()
        }
    }

    pub fn force_emit(self) {
        match self.level {
            Level::Error | Level::Warning => eprintln!("{}", self),
            _ => println!("{}", self),
        }
    }

    #[cold]
    #[track_caller]
    pub fn scream(self) -> ! {
        std::panic::set_hook(Box::new(|_| {}));

        self.force_emit();

        panic!();
    }
}

impl fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(Location::Span(ref span)) = self.location {
            let source = fs::read_to_string(span.source()).expect("source file should be readable");

            let indicies = indicies_from_indicies(&source, span.start(), span.end())
                .expect_or_scream("`Span` should be fully contained within the source file");
            let multi_line = (indicies.last_line - indicies.first_line) > 0;

            // Length of the number when converted to decimal, plus one for padding.
            let spaces = (indicies.last_line.checked_ilog10().unwrap_or(0) + 1) as usize;

            let description = if multi_line {
                let first = source
                    .lines()
                    .nth(indicies.first_line)
                    .expect_or_scream("`Span` should be contained in the source file");
                format!(
                    "{cap:>width$}\
                        {first:<spaces$}
                        {cap:>width$}
                    ",
                    cap = "|",
                    spaces = spaces.max(4),
                    width = (spaces + 1).max(5)
                )
            } else {
                let line = source
                    .lines()
                    .nth(indicies.first_line + 1)
                    .expect_or_scream("`Span` should be contained in the source file");
                format!(
                    "{cap:>width$}\
                        {n:<spaces$}| {line}\
                        {cap:>width$}
                    ",
                    n = indicies.first_line,
                    cap = "|",
                    width = spaces + 1,
                )
            };

            let children = self.children.iter().fold(String::new(), |fold, child| {
                fold + &format!("{:>width$} {}", "=", child, width = spaces + 1)
            });

            write!(
                f,
                "{}\n   --> {}:{}:{}\n{}\n{}",
                self.format_message(true),
                span.source().display(),
                indicies.first_line,
                indicies.first_char,
                description,
                children,
            )
        } else if let Some(Location::Panic {
            ref path,
            line,
            column,
        }) = self.location
        {
            write!(
                f,
                "{}, {}:{}:{}",
                self.format_message(false),
                path,
                line,
                column
            )
        } else {
            write!(f, "{}", self.format_message(false))
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Level {
    /// An error.
    Error = 1,
    /// A warning.
    Warning = 2,
    /// A help message.
    Help = 3,
    /// A note.
    Note = 4,
}

impl PartialEq<crate::Verbosity> for Level {
    fn eq(&self, other: &crate::Verbosity) -> bool {
        (*self as u8) == (*other as u8)
    }
}

impl PartialOrd<crate::Verbosity> for Level {
    fn partial_cmp(&self, other: &crate::Verbosity) -> Option<std::cmp::Ordering> {
        (*self as u8).partial_cmp(&(*other as u8))
    }
}

impl Default for Diagnostic {
    fn default() -> Self {
        // We use error here since the only place
        // this method is called is in `logos::Logos::error`
        Diagnostic::error("default error message")
    }
}

#[derive(Debug, Clone, PartialEq)]
struct Child {
    level: Level,
    message: String,
}

impl Child {
    #[inline]
    fn new<T>(level: Level, message: T) -> Child
    where
        T: Into<String>,
    {
        Child {
            level,
            message: message.into(),
        }
    }
}
struct Indicies {
    first_line: usize,
    first_char: usize,
    last_line: usize,
    last_char: usize,
}

fn indicies_from_indicies(s: &str, mut first: usize, mut last: usize) -> Option<Indicies> {
    let mut current_index = 0;
    if first > last {
        std::mem::swap(&mut first, &mut last);
    }
    let mut first_line = 0;
    let mut first_char = 0;
    let mut first_found = false;

    for (i, line) in s.lines().enumerate() {
        current_index += line.len();

        if current_index >= first && !first_found {
            first_char = first - (current_index - line.len());
            first_line = i;
            first_found = true;
        }

        if current_index >= last {
            let last_char = last - (current_index - line.len());
            return Some(Indicies {
                first_line,
                first_char,
                last_char,
                last_line: i,
            });
        }
    }

    None
}

fn italic_code(message: &str) -> String {
    let mut full = String::new();
    let mut inner = String::new();
    let mut inside = false;

    for c in message.chars() {
        if c == '`' {
            if inside {
                full = format!("{full}{}", inner.italic());
                inner.clear();
            }
            inside = !inside;
        }

        if inside && c != '`' {
            inner.push(c);
        } else {
            full.push(c);
        }
    }

    full
}

impl fmt::Display for Child {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.level {
            Level::Error => write!(
                f,
                "{}",
                format!("{}: {}", "error".red(), self.message).bold()
            ),
            Level::Warning => write!(
                f,
                "{}",
                format!("{}: {}", "warning".yellow(), self.message).bold()
            ),
            Level::Note => write!(f, "{}", format!("note: {}", self.message).bold()),
            Level::Help => write!(
                f,
                "{}",
                format!("{}: {}", "help".truecolor(150, 150, 255), self.message).bold()
            ),
        }
    }
}

pub trait ResultScream<T, E> {
    /// Returns the contained [`Ok`] value, consuming the `self` value.
    ///
    ///
    fn unwrap_or_scream(self) -> T
    where
        E: fmt::Debug;

    fn spanned_unwrap(self, span: Span) -> T
    where
        E: fmt::Debug;
    
    /// Returns the contained [`Ok`] value, consuming the `self` value.
    ///
    /// ### Panics
    ///
    /// Panics if the value is an [`Err`], with a diagnostic message including
    /// the passed message and the content of the [`Err`] being
    /// [`scream`][Diagnostic::scream]ed at an [`Error`][Level::Error] level.
    fn expect_or_scream<M: Into<String>>(self, message: M) -> T
    where
        E: fmt::Debug;
    
    fn spanned_expect<M: Into<String>>(self, span: Span, message: M) -> T
    where
        E: fmt::Debug;
        
    fn unwrap_err_or_scream(self) -> E
    where
        T: fmt::Debug;

    fn expect_err_or_scream<M: Into<String>>(self, message: M) -> E
    where
        T: fmt::Debug;
}

#[macro_export]
macro_rules! scream {
    () => {
        $crate::diagnostic::Diagnostic::error(format!())
    };
    ($message:expr) => {
        $crate::diagnostic::Diagnostic::error($message).scream();
    };
}

impl<T, E> ResultScream<T, E> for Result<T, E> {
    #[track_caller]
    #[inline(always)]
    fn unwrap_or_scream(self) -> T
    where
        E: fmt::Debug,
    {
        match self {
            Ok(ok) => ok,
            Err(err) => scream_with(
                "called `Result::unwrap_or_scream` on an `Err` value",
                &err,
            ),
        }
    }

    #[track_caller]
    #[inline(always)]
    fn spanned_unwrap(self, span: Span) -> T
    where
        E: fmt::Debug
    {
        match self {
            Ok(ok) => ok,
            Err(err) => scream_with_span(
                span,
                "called `Result::spanned_unwrap` on an `Err` value",
                &err,
            ),
        }
    }

    #[track_caller]
    #[inline(always)]
    fn expect_or_scream<M: Into<String>>(self, message: M) -> T
    where
        E: fmt::Debug,
    {
        match self {
            Ok(ok) => ok,
            Err(err) => scream_with(message.into().as_ref(), &err),
        }
    }

    #[track_caller]
    #[inline(always)]
    fn spanned_expect<M: Into<String>>(self, span: Span, message: M) -> T
    where
        E: fmt::Debug
    {
        match self {
            Ok(ok) => ok,
            Err(err) => scream_with_span(span, message.into().as_ref(), &err)
        }
    }

    #[track_caller]
    #[inline(always)]
    fn unwrap_err_or_scream(self) -> E
    where
        T: fmt::Debug,
    {
        match self {
            Ok(ok) => scream_with(
                "called `Result::unwrap_err_or_scream` on an `Ok` value",
                &ok,
            ),
            Err(err) => err,
        }
    }

    #[track_caller]
    #[inline(always)]
    fn expect_err_or_scream<M: Into<String>>(self, message: M) -> E
    where
        T: fmt::Debug,
    {
        match self {
            Ok(ok) => scream_with(message.into().as_ref(), &ok),
            Err(err) => err,
        }
    }
}

pub trait OptionalScream<T> {
    fn unwrap_or_scream(self) -> T;

    fn spanned_unwrap(self, span: Span) -> T;

    fn expect_or_scream<M: Into<String>>(self, message: M) -> T;

    fn spanned_expect<M: Into<String>>(self, span: Span, message: M) -> T;

    fn unwrap_none_or_scream(self)
    where
        T: fmt::Debug;

    fn expect_none_or_scream<M: Into<String>>(self, message: M)
    where
        T: fmt::Debug;
}

impl<T> OptionalScream<T> for Option<T> {
    #[track_caller]
    #[inline(always)]
    fn unwrap_or_scream(self) -> T {
        match self {
            Some(some) => some,
            None => scream("called `Option::unwrap_or_scream` on a `None` value"),
        }
    }

    #[track_caller]
    #[inline(always)]
    fn spanned_unwrap(self, span: Span) -> T {
        match self {
            Some(some) => some,
            None => spanned_scream(span, "called `Option::spanned_unwrap` on a `None` value")
        }
    }

    #[track_caller]
    #[inline(always)]
    fn expect_or_scream<M: Into<String>>(self, message: M) -> T {
        match self {
            Some(some) => some,
            None => scream(message.into().as_ref()),
        }
    }

    #[track_caller]
    #[inline(always)]
    fn spanned_expect<M: Into<String>>(self, span: Span, message: M) -> T {
        match self {
            Some(some) => some,
            None => spanned_scream(span, message.into().as_ref())
        }
    }

    #[track_caller]
    #[inline(always)]
    fn unwrap_none_or_scream(self)
    where
        T: fmt::Debug,
    {
        match self {
            Some(some) => scream_with(
                "called `Option::unwrap_none_or_scream` on `Some` value",
                &some,
            ),
            None => {}
        }
    }

    #[track_caller]
    #[inline(always)]
    fn expect_none_or_scream<M: Into<String>>(self, message: M)
    where
        T: fmt::Debug,
    {
        match self {
            Some(some) => scream_with(message.into().as_ref(), &some),
            None => {}
        }
    }
}

#[cold]
#[track_caller]
#[inline(never)]
fn scream(msg: &str) -> ! {
    let location = std::panic::Location::caller();

    if cfg!(debug_assertions) {
        Diagnostic::panicking_error(location, msg).scream()
    } else {
        Diagnostic::error(msg).scream()
    }
}

#[cold]
#[track_caller]
#[inline(never)]
fn spanned_scream(span: Span, msg: &str) -> ! {
    Diagnostic::spanned_error(span, msg).scream()
}

#[cold]
#[track_caller]
#[inline(never)]
fn scream_with(msg: &str, value: &dyn fmt::Debug) -> ! {
    let location = std::panic::Location::caller();

    if cfg!(debug_assertions) {
        Diagnostic::panicking_error(location, format!("{msg}: {value:?}")).scream()
    } else {
        Diagnostic::error(format!("{msg}: {value:?}")).scream()
    }
}

#[cold]
#[track_caller]
#[inline(never)]
fn scream_with_span(span: Span, msg: &str, value: &dyn fmt::Debug) -> ! {
    Diagnostic::spanned_error(span, format!("{msg}: {value:?}")).scream()
}
