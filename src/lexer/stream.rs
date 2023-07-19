use super::{token::{ Ident, Lit, Punct }};

pub struct TokenStream {
    trees: Vec<TokenTree>,
}

pub struct IntoIter(std::vec::IntoIter<TokenTree>);

impl IntoIter {
    pub fn parse<T>(&mut self) -> Option<T>
    where
        T: ParseStream
    {
        match self.next() {
            Some(token) => T::parse(token),
            None => None
        }
    }
}

impl Iterator for IntoIter {
    type Item = TokenTree;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

impl DoubleEndedIterator for IntoIter {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.next_back()
    }
}

impl IntoIterator for TokenStream {
    type Item = TokenTree;
    type IntoIter = IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter(self.trees.into_iter())
    }
}

pub enum TokenTree {
    Ident(Ident),
    Literal(Literal),
    Punct(Punct),
}

pub trait ParseStream: Sized {
    fn parse(tree: TokenTree) -> Option<Self>;
}
