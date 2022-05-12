use std::io::{Read};
use utf8_read::{Char, Error, Reader, StreamPosition};

pub trait TokenError {
    fn no_matching_rule() -> Self;
}

pub enum Rule<S: Read, T, E: TokenError> {
    Static(String, T),
    Custom(Box<dyn Fn(&str) -> bool>, Box<dyn Fn(&mut Token<T, E>, &mut Lexer<S, T, E>)>)
}

pub struct LexerRules<S: Read, T, E: TokenError> {
    pub is_whitespace: Box<dyn Fn(char) -> bool>,
    pub rules: Vec<Rule<S, T, E>>
}

impl<'rules, S: Read, T, E: TokenError> LexerRules<S, T, E> {
    pub fn with_whitespace<F>(&mut self, func: F) -> &mut Self
        where F: 'static + Fn(char) -> bool
    {
        self.is_whitespace = Box::new(func);
        self
    }

    pub fn rule(&mut self, seq: String, value: T) -> &mut Self {
        self.rules.push(Rule::Static(seq, value));
        self
    }

    pub fn rule_custom<R, F>(&mut self, seq: R, builder: F) -> &mut Self
        where
            R: 'static + Fn(&str) -> bool,
            F: 'static + Fn(&mut Token<T, E>, &mut Lexer<S, T, E>)
    {
        self.rules.push(Rule::Custom(Box::new(seq), Box::new(builder)));
        self
    }
}

impl<S: Read, T, E: TokenError> LexerRules<S, T, E> {
    pub fn get_matching_rule<R: AsRef<str>>(&self, seq: R) -> Option<&Rule<S, T, E>> {
        let seq = seq.as_ref();
        self.rules.iter().find(|r| match r {
            Rule::Static(rule_seq, _) => rule_seq == seq,
            Rule::Custom(f, _) => (f)(seq),
        })
    }
}

impl<S: Read, T, E: TokenError> Default for LexerRules<S, T, E> {
    fn default() -> Self {
        LexerRules {
            is_whitespace: Box::new(char::is_whitespace),
            rules: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub struct Token<T, E> {
    pub val: Result<T, E>,
    pub str: String,
    pub pos: StreamPosition,
}

#[derive(Debug)]
pub enum ReadError {
    EOF,
    MalformedUTF8(usize, usize),
    IOError(std::io::Error),
}

pub struct Lexer<'rules, S: Read, T, E: TokenError> {
    reader: Reader<S>,
    rules: &'rules LexerRules<S, T, E>,
}

impl<'rules, S: Read, T: Clone, E: TokenError> Lexer<'rules, S, T, E> {
    pub fn new(stream: S, rules: &'rules LexerRules<S, T, E>) -> Self
    {
        Lexer {
            reader: Reader::new(stream),
            rules,
        }
    }

    pub fn build_tokens(mut self) -> Vec<Token<T, E>> {
        let mut tokens = Vec::new();
        while let Ok(token) = self.next_token() {
            tokens.push(token);
        }
        tokens
    }

    /// Reads characters from the input stream until a rule is found matching the sequence.
    pub fn next_token(&mut self) -> Result<Token<T, E>, ReadError> {
        let mut token = Token::<T, E> {
            val: Err(E::no_matching_rule()),
            str: String::new(),
            pos: StreamPosition::new(),
        };
        loop {
            match self.next_char() {
                Ok(c) => {
                    if (self.rules.is_whitespace)(c) {
                        if !token.str.is_empty() {
                            break;
                        }
                    } else {
                        if token.str.is_empty() { // If first char pushed
                            token.pos = self.reader.borrow_pos().clone();
                        }
                        token.str.push(c);
                        if let Some(rule) = self.rules.get_matching_rule(&token.str) {
                            match rule {
                                Rule::Static(_, val) => token.val = Ok((*val).clone()),
                                Rule::Custom(_, f) => (f)(&mut token, self),
                            }
                            break;
                        }
                    }
                },
                Err(ReadError::EOF) => if token.str.is_empty() {
                    return Err(ReadError::EOF);
                } else {
                    break;
                },
                Err(error) => return Err(error),
            }
        }
        Ok(token)
    }

    /// Returns the next char in the stream or None on EOF or end of transmission.
    pub fn next_char(&mut self) -> Result<char, ReadError> {
        match self.reader.next_char() {
            Ok(c) => match c {
                Char::Char(c) => Ok(c),
                Char::Eof | Char::NoData => Err(ReadError::EOF),
            }
            Err(Error::MalformedUtf8(pos, len)) => Err(ReadError::MalformedUTF8(pos.byte(), len)),
            Err(Error::IoError(error)) => Err(ReadError::IOError(error)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Lexer, LexerRules, TokenError};

    #[test]
    fn it_works() {
        #[derive(Debug, Copy, Clone, PartialEq)]
        enum Token {
            Plus,
            Minus,
            String,
        }

        #[derive(Debug, Copy, Clone, PartialEq)]
        enum Error {
            UnclosedQuotes,
            NoMatching,
        }

        impl TokenError for Error {
            fn no_matching_rule() -> Self {
                Error::NoMatching
            }
        }

        let mut rules = LexerRules::default();
        rules.with_whitespace(|c| c.is_whitespace())
            .rule(String::from('+'), Token::Plus)
            .rule(String::from('-'), Token::Minus)
            .rule_custom(|s| s == "\"", |token, lexer| {
                token.val = Ok(Token::String);
                loop {
                    match lexer.next_char() {
                        Ok('\"') => {
                            token.str.push('\"');
                            break;
                        },
                        Ok(c) => token.str.push(c),
                        Err(_) => token.val = Err(Error::UnclosedQuotes),
                    }
                }
            });

        let lexer = Lexer::<_, Token, Error>::new(
            "++- invalid \"abc\" ".as_bytes(),
            &rules
        );

        let tokens = lexer.build_tokens();

        assert_eq!(tokens.len(), 5);
        assert_eq!(tokens[2].val, Ok(Token::Minus));
    }
}
