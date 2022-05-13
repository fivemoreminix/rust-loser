use std::io::Read;
use std::rc::Rc;

use utf8_read::{Char, Error, Reader};

pub trait TokenError {
    fn no_matching_rule() -> Self;
}

#[derive(Debug, PartialEq)]
enum Match {
    Full,
    Possible,
    None,
}

pub enum Rule<T, E: TokenError> {
    Static(String, T),
    Custom(Box<dyn Fn(&str) -> bool>, Box<dyn Fn(TokenBuilder<T, E>) -> Vec<Token<T, E>>>),
}

impl<T, E: TokenError> Rule<T, E> {
    pub fn is_static(&self) -> bool {
        !self.is_custom()
    }

    pub fn is_custom(&self) -> bool {
        match self {
            Self::Custom(..) => true,
            _ => false,
        }
    }

    /// If the rule matches the sequence, a Match::Full is returned. If there is no match then a
    /// Match::None is returned. If the rule is static, then a Match::Possible can be returned if
    /// the sequence provided forms the beginning part of the rule.
    fn get_sequence_match(&self, seq: &str) -> Match {
        match self {
            Rule::Static(other, _) => if other == seq {
                Match::Full
            } else if other.starts_with(seq) {
                Match::Possible // TODO: consider removing the Match::Possible variant
            } else {
                Match::None
            },
            Rule::Custom(test, _) => if (test)(seq) {
                Match::Full
            } else {
                Match::None
            },
        }
    }
}

pub struct LexerRules<T, E: TokenError> {
    pub is_whitespace: Box<dyn Fn(char) -> bool>,
    pub rules: Vec<Rule<T, E>>,
}

impl<'rules, T, E: TokenError> LexerRules<T, E> {
    pub fn with_whitespace(&mut self, func: impl Fn(char) -> bool + 'static) -> &mut Self
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
            F: 'static + Fn(TokenBuilder<T, E>) -> Vec<Token<T, E>>
    {
        self.rules.push(Rule::Custom(Box::new(seq), Box::new(builder)));
        self
    }
}

impl<T, E: TokenError> LexerRules<T, E> {
    pub fn get_matching_rule<R: AsRef<str>>(&self, seq: R) -> Option<&Rule<T, E>> {
        let seq = seq.as_ref();
        self.rules.iter().find(|r| match r {
            Rule::Static(rule_seq, _) => rule_seq == seq,
            Rule::Custom(f, _) => (f)(seq),
        })
    }

    pub fn get_static_match(&self, token: &Token<T, E>) -> Option<&Rule<T, E>> {
        self.rules.iter().find(|rule|
            rule.is_static() && rule.get_sequence_match(&token.str) == Match::Full)
    }

    pub fn get_custom_match(&self, token: &Token<T, E>) -> Option<&Rule<T, E>> {
        self.rules.iter().find(|rule|
            rule.is_custom() && rule.get_sequence_match(&token.str) == Match::Full)
    }
}

impl<T, E: TokenError> Default for LexerRules<T, E> {
    fn default() -> Self {
        LexerRules {
            is_whitespace: Box::new(char::is_whitespace),
            rules: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Pos {
    /// Byte index where this position begins. Always points to a the beginning of a UTF-8 char.
    pub start: usize,
    /// Line index this char is at (based on LF (\n) and/or CRLF (\r\n) sequences).
    pub line: usize,
    /// Column char index (not byte index!).
    pub col: usize,
}

impl Pos {
    pub fn new() -> Pos {
        Pos { start: 0, line: 0, col: 0 }
    }
}

#[derive(Debug, Clone)]
pub struct Token<T, E> {
    pub val: Result<T, E>,
    pub str: String,
    pub pos: Pos,
}

#[derive(Debug, Clone)]
pub enum ReadError {
    EOF,
    MalformedUTF8(usize, usize),
    IOError(Rc<std::io::Error>),
}

pub struct TokenBuilder<T, E> {
    token: Token<T, E>,
}

impl<T, E> TokenBuilder<T, E> {
    pub fn new(token: Token<T, E>) -> Self {
        TokenBuilder {
            token
        }
    }

    pub fn get_string(&self) -> &str {
        &self.token.str
    }

    // Construct a new TokenBuilder by taking a substring. start is the index of the first character
    // and the end is the index after the last character to take. These form an exclusive range. A
    // val is also required to form the new underlying Token.
    pub fn token(&self, start: usize, end: usize, val: Result<T, E>) -> Token<T, E> {
        let pos = &self.token.pos;
        Token {
            val,
            str: self.token.str[start..end].to_owned(),
            pos: Pos {
                start: pos.start + start,
                line: pos.line,
                col: pos.col,
            },
        }
    }
}

pub struct Lexer<'rules, S: Read, T, E: TokenError> {
    reader: Reader<S>,
    rules: &'rules LexerRules<T, E>,
    next: Result<char, ReadError>,
}

impl<'rules, S: Read, T: Clone, E: TokenError> Lexer<'rules, S, T, E> {
    pub fn new(stream: S, rules: &'rules LexerRules<T, E>) -> Self
    {
        let mut l = Lexer {
            reader: Reader::new(stream),
            rules,
            next: Err(ReadError::EOF),
        };
        l.next = l.next_char_no_peek_mechanism();
        return l;
    }

    pub fn build_tokens(mut self) -> Vec<Token<T, E>> {
        let mut collection = Vec::new();
        while let Ok(mut tokens) = self.next_sequence() {
            collection.append(&mut tokens);
        }
        // TODO: handle next_sequence errors
        collection
    }

    /// Reads characters from the input stream until one or many rules are found matching the
    /// sequence.
    pub fn next_sequence(&mut self) -> Result<Vec<Token<T, E>>, ReadError> {
        // for each rule check if any match:
        //  if one matches, use that rule
        // keep a shrinking list of the rules that *can* match until either a static rule is a perfect
        // match, or if no static rules match when the whole sequence has been produced,
        // use the first custom rule. If there is not a matching custom rule, produce the no matching
        // rule token.

        let mut token = Token::<T, E> {
            val: Err(E::no_matching_rule()),
            str: String::new(),
            pos: Pos::new(),
        };
        loop {
            match self.next_char() {
                Ok(c) => {
                    let mut is_whitespace = false;
                    if (self.rules.is_whitespace)(c) {
                        if token.str.is_empty() {
                            return Ok(Vec::new());
                        }
                        is_whitespace = true;
                    } else {
                        if token.str.is_empty() { // If first char pushed
                            let pos = self.reader.borrow_pos();
                            let (line, col) = pos.line_position();
                            token.pos = Pos {
                                start: pos.byte() - 2,
                                line: line - 1,
                                col: col - 3,
                            };
                        }
                        token.str.push(c);
                    }

                    let static_match = self.rules.get_static_match(&token);
                    if let Some(Rule::Static(_, val)) = static_match {
                        token.val = Ok((*val).clone());

                        let mut vec = Vec::with_capacity(1);
                        vec.push(token);
                        return Ok(vec);
                    }

                    // This gives the end of parsing a token (via whitespace acting as a delim)
                    // to have a chance at finding a matching static rule for the token.
                    if is_whitespace {
                        break;
                    }
                }
                Err(ReadError::EOF) => if token.str.is_empty() {
                    return Err(ReadError::EOF);
                } else {
                    break;
                },
                Err(error) => return Err(error),
            }
        }

        let custom_match = self.rules.get_custom_match(&token);
        if let Some(Rule::Custom(_, f)) = custom_match {
            let tokens = (f)(TokenBuilder::new(token));
            return Ok(tokens);
        }

        let mut vec = Vec::with_capacity(1);
        vec.push(token);
        Ok(vec)
    }

    pub fn peek_char(&self) -> &Result<char, ReadError> {
        &self.next
    }

    /// Returns the next char in the stream or None on EOF or end of transmission.
    pub fn next_char(&mut self) -> Result<char, ReadError> {
        let next = self.next.clone();
        self.next = self.next_char_no_peek_mechanism();
        next
    }

    fn next_char_no_peek_mechanism(&mut self) -> Result<char, ReadError> {
        match self.reader.next_char() {
            Ok(c) => match c {
                Char::Char(c) => Ok(c),
                Char::Eof | Char::NoData => Err(ReadError::EOF),
            }
            Err(Error::MalformedUtf8(pos, len)) => Err(ReadError::MalformedUTF8(pos.byte(), len)),
            Err(Error::IoError(error)) => Err(ReadError::IOError(Rc::new(error))),
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
            .rule_custom(|s| s == "\"", |builder| {
                vec![]
            });

        let lexer = Lexer::<_, Token, Error>::new(
            "++- invalid \"abc\" ".as_bytes(),
            &rules,
        );

        let tokens = lexer.build_tokens();

        assert_eq!(tokens.len(), 5);
        assert_eq!(tokens[2].val, Ok(Token::Minus));
    }
}
