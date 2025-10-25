use logos::{Logos, SpannedIter};

use crate::core::syntax::lexer::{
    stream::TokenStream,
    token::{Token, TokenKind},
};

pub mod stream;
pub mod token;

pub struct Lexer<'a> {
    tok_stream: SpannedIter<'a, TokenKind<'a>>,
    current_tok: Token<'a>,
    src: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        let mut tok_stream = TokenKind::lexer(src).spanned();
        let current_tok = tok_stream
            .next()
            .unwrap_or((Ok(TokenKind::EOF), src.len()..src.len()))
            .into();
        Self {
            src,
            tok_stream,
            current_tok,
        }
    }
}

impl<'a> TokenStream<'a> for Lexer<'a> {
    /// Consume a token
    fn next(&mut self) -> Token<'a> {
        let tok = std::mem::replace(
            &mut self.current_tok,
            self.tok_stream
                .next()
                .unwrap_or((Ok(TokenKind::EOF), self.src.len() - 1..self.src.len() - 1))
                .into(),
        );
        tok
    }

    /// Peek a token
    fn peek(&self) -> &Token<'a> {
        &self.current_tok
    }
}

// #[cfg(test)]
// mod test {
//     use logos::Logos;

//     use crate::core::syntax::lexer::TokenKind;

//     #[test]
//     fn lexing() {
//         let to_test = [
//             (
//                 "Hi, Atom 'atom.",
//                 vec![
//                     Ok(TokenKind::Ident("Hi")),
//                     Ok(TokenKind::Comma),
//                     Ok(TokenKind::KwAtom),
//                     Ok(TokenKind::Atom("'atom")),
//                     Ok(TokenKind::Dot),
//                 ],
//             ),
//             (
//                 "let me: Type you;",
//                 vec![
//                     Ok(TokenKind::KwLet),
//                     Ok(TokenKind::Ident("me")),
//                     Ok(TokenKind::Colon),
//                     Ok(TokenKind::KwType),
//                     Ok(TokenKind::Ident("you")),
//                     Ok(TokenKind::Semicolon),
//                 ],
//             ),
//         ];

//         for (source, tokens) in to_test {
//             let mut tokens = tokens.into_iter();
//             let mut lexer = TokenKind::lexer(source);

//             while let Some(token_lexed) = lexer.next() {
//                 if let Some(token) = tokens.next() {
//                     assert_eq!(token_lexed, token)
//                 } else {
//                     panic!("Token length is not enough!")
//                 }
//             }

//             if tokens.next() != None {
//                 panic!("Lexed token length is not enough!")
//             }
//         }
//     }
// }
