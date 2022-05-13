use std::io::Read;
use std::rc::Rc;

use utf8_read::{Char, Error, Reader};

pub trait TokenError {
    fn no_matching_rule() -> Self;
}

#[derive(Debug, PartialEq)]
enum Match {
    Full,
    Partial,
    None,
}

pub enum Rule<T, E: TokenError> {
    Static(String, T),
    Custom(Box<dyn Fn(char) -> bool>, Box<dyn Fn(TokenBuilder) -> Vec<Token<T, E>>>),
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
    fn get_sequence_match<S: AsRef<str>>(&self, seq: S) -> Match {
        let seq = seq.as_ref();
        match self {
            Rule::Static(other, _) => if other == seq {
                Match::Full
            } else if other.starts_with(seq) {
                Match::Partial // TODO: consider removing the Match::Partial variant
            } else {
                Match::None
            },
            Rule::Custom(test, _) => if seq.chars().all(|c| (test)(c)) {
                Match::Full
            } else {
                Match::None
            },
        }
    }
}

pub struct LexerRules<T, E: TokenError> {
    pub is_skipped: Box<dyn Fn(char) -> bool>,
    pub rules: Vec<Rule<T, E>>,
}

impl<'rules, T, E: TokenError> LexerRules<T, E> {
    pub fn skips(&mut self, func: impl Fn(char) -> bool + 'static) -> &mut Self
    {
        self.is_skipped = Box::new(func);
        self
    }

    pub fn rule(&mut self, seq: String, value: T) -> &mut Self {
        self.rules.push(Rule::Static(seq, value));
        self
    }

    pub fn rule_custom<R, F>(&mut self, seq: R, builder: F) -> &mut Self
        where
            R: 'static + Fn(char) -> bool,
            F: 'static + Fn(TokenBuilder) -> Vec<Token<T, E>>
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
            Rule::Custom(test, _) => seq.chars().all(|c| (test)(c)),
        })
    }

    pub fn get_static_match<S: AsRef<str>>(&self, seq: S) -> Option<&Rule<T, E>> {
        let seq = seq.as_ref();
        self.rules.iter().find(|rule| match rule {
            Rule::Static(str, _) => str == seq,
            _ => false,
        })
    }

    pub fn get_custom_match<S: AsRef<str>>(&self, seq: S) -> Option<&Rule<T, E>> {
        let seq = seq.as_ref();
        self.rules.iter().find(|rule|
            rule.is_custom() && rule.get_sequence_match(seq) == Match::Full)
    }
}

impl<T, E: TokenError> Default for LexerRules<T, E> {
    fn default() -> Self {
        LexerRules {
            is_skipped: Box::new(char::is_whitespace),
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

    pub fn offset(&self, amt: usize) -> Self {
        Pos { start: self.start + amt, line: self.line, col: self.col + amt }
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

pub struct TokenBuilder {
    str: String,
    pos: Pos,
}

impl TokenBuilder {
    pub fn new(str: String, pos: Pos) -> Self {
        TokenBuilder { str, pos }
    }

    pub fn get_string(&self) -> &str {
        &self.str
    }

    // Construct a new TokenBuilder by taking a substring. start is the index of the first character
    // and the end is the index after the last character to take. These form an exclusive range. A
    // val is also required to form the new underlying Token.
    pub fn token<T, E>(&self, start: usize, end: usize, val: Result<T, E>) -> Token<T, E> {
        let pos = &self.pos;
        Token {
            val,
            str: self.str[start..end].to_owned(),
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

        let mut tokens = Vec::new();

        let (word, origin_pos, read_error) = self.next_word();

        if word.is_empty() {
            return Err(read_error.unwrap());
        }

        // Short circuit for words that are a complete and single rule
        let rule = self.rules.get_matching_rule(&word);
        if rule.is_some() {
            return Ok(Self::build_tokens_for_rule(rule.unwrap(), &word, origin_pos));
        }

        // let mut static_rules_possible = false;
        let mut start = 0usize; // Index of word's substring start char
        let mut substr: &str = &word;
        let mut current_pos = Pos::new();
        let mut custom_match_previous = None;

        for (i, c) in word.chars().enumerate() {
            // peek a char
            // if there are no static rules already we're building a full word
            // so if this c doesn't match the func of any custom tester
            // then we need to stop reading chars

            substr = &word[start..=i];

            if i - start == 0 { // If first char in a substr
                current_pos = origin_pos.offset(i);
            }

            // Split the word into sub-words for custom rules when we find a character that does
            // not meet the requirements of a previously available custom rule.
            let custom_match = self.rules.get_custom_match(substr);
            if custom_match_previous.is_some() && custom_match.is_none() {
                tokens.append(&mut Self::build_tokens_for_rule(custom_match_previous.unwrap(), &word[start..i], current_pos.clone()));
                start = i;
                substr = &word[start..=i];
                current_pos = origin_pos.offset(start);
                custom_match_previous = None;
            } else {
                custom_match_previous = custom_match;
            }

            let static_match = self.rules.get_static_match(substr);
            if static_match.is_some() {
                tokens.append(&mut Self::build_tokens_for_rule(static_match.unwrap(), substr, current_pos.clone()));
                return Ok(tokens);
            }
        }

        // Take the custom match since no static match could be found during the loop.
        let custom_match = self.rules.get_custom_match(substr);
        if custom_match.is_some() {
            return Ok(Self::build_tokens_for_rule(custom_match.unwrap(), substr, current_pos));
        }

        tokens.push(Token {
            val: Err(E::no_matching_rule()),
            str: substr.to_owned(),
            pos: current_pos,
        });
        Ok(tokens)
    }

    fn build_tokens_for_rule(rule: &Rule<T, E>, seq: &str, pos: Pos) -> Vec<Token<T, E>> {
        match rule {
            Rule::Static(_, val) => {
                let token = Token {
                    val: Ok((*val).clone()),
                    str: seq.to_owned(),
                    pos,
                };
                let mut vec = Vec::with_capacity(1);
                vec.push(token);
                vec
            }
            Rule::Custom(_, f) => {
                (f)(TokenBuilder::new(seq.to_owned(), pos))
            }
        }
    }

    pub fn next_word(&mut self) -> (String, Pos, Option<ReadError>) {
        let mut word = String::new();
        let mut pos = Pos::new();

        loop {
            let c_result = self.next_char();

            if let Err(err) = c_result {
                return (word, pos, Some(err));
            }

            let c = unsafe { c_result.unwrap_unchecked() };

            if (self.rules.is_skipped)(c) {
                if word.is_empty() {
                    continue;
                } else {
                    break;
                }
            }

            if word.is_empty() {
                pos = {
                    let spos = self.reader.borrow_pos();
                    let (line, col) = spos.line_position();
                    Pos {
                        start: spos.byte() - 2,
                        line: line - 1,
                        col: col - 3,
                    }
                };
            }

            word.push(c);
        }
        (word, pos, None)
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
        rules.skips(|c| c.is_whitespace())
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
