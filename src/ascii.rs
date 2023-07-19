use std::slice::Iter;

#[derive(Clone, PartialEq)]
pub struct AsciiStr {
    inner: Vec<u8>,
}

impl AsciiStr {
    pub fn from_buffer(buf: &[u8]) -> Result<Self, ()> {
        for byte in buf {
            if *byte > 127 {
                return Err(());
            }
        }

        Ok(AsciiStr {
            inner: Vec::from(buf),
        })
    }

    pub unsafe fn from_buffer_unchecked(buf: &[u8]) -> Self {
        AsciiStr {
            inner: Vec::from(buf),
        }
    }
}

impl TryFrom<String> for AsciiStr {
    type Error = ();

    fn try_from(value: String) -> Result<Self, Self::Error> {
        if !value.is_ascii() {
            return Err(());
        }

        Ok(AsciiStr {
            inner: value.into_bytes(),
        })
    }
}

impl std::fmt::Debug for AsciiStr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", unsafe {
            std::str::from_utf8_unchecked(&self.inner)
        })
    }
}

impl AsciiStr {
    fn iter(&self) -> Iter<u8> {
        self.inner.iter()
    }
}

impl IntoIterator for AsciiStr {
    type Item = u8;
    type IntoIter = std::vec::IntoIter<u8>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}

pub enum CharDecodeError {
    NotAscii,
    InvalidLength(usize),
}

pub fn caret_decode(s: &str) -> Result<u8, CharDecodeError> {
    if s.is_empty() {
        return Err(CharDecodeError::InvalidLength(s.len()));
    } else if s.len() == 1 && s.is_ascii() {
        Ok(s.as_bytes()[0])
    } else if s.len() == 2 && s.starts_with('^') {
        let map = s.to_ascii_uppercase().as_bytes()[1];
        if ('?'..'_').contains(&(map as char)) {
            Ok(map ^ 0x40)
        } else {
            Err(CharDecodeError::NotAscii)
        }
    } else {
        Err(CharDecodeError::InvalidLength(s.len()))
    }
}
