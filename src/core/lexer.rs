use logos::{Lexer, Logos};

fn slice_str_callback<'a>(lex: &mut Lexer<'a, Token<'a>>) -> &'a str {
    lex.slice()
}

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \r\t\n\f]+")]
pub enum Token<'a> {
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token(",")]
    Comma,
    #[token(".")]
    Dot,
    #[token(":")]
    Colon,
    #[token("*")]
    Star,
    #[token("->")]
    Arrow,
    #[token("=>")]
    DArrow,
    #[token("=")]
    Eq,
    #[token(";")]
    Semicolon,
    #[token("let")]
    KwLet,
    #[token("Atom")]
    KwAtom,
    #[token("Type")]
    KwType,

    #[regex(r"'[a-zA-Z_][a-zA-Z0-9_]*", slice_str_callback)]
    Atom(&'a str),
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", slice_str_callback)]
    Ident(&'a str),
}

#[cfg(test)]
mod test {
    use logos::Logos;

    use crate::core::lexer::Token;

    #[test]
    fn lexing() {
        let to_test = [
            (
                "Hi, Atom 'atom.",
                vec![
                    Ok(Token::Ident("Hi")),
                    Ok(Token::Comma),
                    Ok(Token::KwAtom),
                    Ok(Token::Atom("'atom")),
                    Ok(Token::Dot),
                ],
            ),
            (
                "let me: Type you;",
                vec![
                    Ok(Token::KwLet),
                    Ok(Token::Ident("me")),
                    Ok(Token::Colon),
                    Ok(Token::KwType),
                    Ok(Token::Ident("you")),
                    Ok(Token::Semicolon),
                ],
            ),
        ];

        for (source, tokens) in to_test {
            let mut tokens = tokens.into_iter();
            let mut lexer = Token::lexer(source);

            while let Some(token_lexed) = lexer.next() {
                if let Some(token) = tokens.next() {
                    assert_eq!(token_lexed, token)
                } else {
                    panic!("Token length is not enough!")
                }
            }

            if tokens.next() != None {
                panic!("Lexed token length is not enough!")
            }
        }
    }
}
