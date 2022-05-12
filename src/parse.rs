// goals of parsing:
//  * build a tree of operations
//  * make code reusable
//  * use high-level constructs when possible (like expects '=' next)
//  * have accurate error reporting and don't hault parsing on errors encountered
//  * the parser api should not be representative of the method used to parse the instructions
//  * able to peek forward any number of tokens, but a consume pattern


/*
if_rule:
    expects 'if'
    expects '('
        loop {
            let cond = expects condition_rule
            maybe ',':
                if one present, resume loop
                else: break loop
        }
    expects ')'
    expects statement_or_block_rule
    produces if_object
 */

use crate::lex::Token;

pub trait ParseError {}

pub trait Rule<T, TokE, E: ParseError> {
    type Product;
    fn build(ctx: &mut ParseContext<T, TokE>) -> Result<Self::Product, E>;
}

// The data structure passed to rules to test their values. Should implement clone so that rules
// can be tested without incurring much cost.
#[derive(Debug, Clone)]
pub struct ParseContext<'t, T, TokE> {
    tokens: &'t [Token<T, TokE>],
    next: usize,
}

impl<'t, T, TokE> ParseContext<'t, T, TokE> {
    pub fn new(tokens: &'t [Token<T, TokE>]) -> Self {
        ParseContext {
            tokens,
            next: 0,
        }
    }

    pub fn expect_next<E>(&mut self, val: Result<T, TokE>, err: E) -> Result<(), E>
        where
            T: PartialEq,
            TokE: PartialEq,
    {
        let tok = self.next_token();
        if tok.is_none() || unsafe { tok.unwrap_unchecked() }.val != val {
            Err(err)
        } else {
            Ok(())
        }
    }

    pub fn next_token(&mut self) -> Option<&Token<T, TokE>> {
        if self.next < self.tokens.len() {
            let token = unsafe { self.tokens.get_unchecked(self.next) };
            self.next += 1;
            Some(token)
        } else {
            None
        }
    }

    pub fn peek_token(&self, ahead: usize) -> Option<&Token<T, TokE>> {
        self.tokens.get(self.next + ahead)
    }
}

pub fn parse_rule<T, TokE, E, R>(tokens: &[Token<T, TokE>]) -> Result<R::Product, E>
    where
        E: ParseError,
        R: Rule<T, TokE, E>
{
    let mut context = ParseContext::new(tokens);
    Ok(R::build(&mut context)?)
}

#[cfg(test)]
mod test {
    use utf8_read::StreamPosition;

    use crate::lex::Token;
    use crate::parse::{parse_rule, ParseContext, Rule};

    #[test]
    fn basic_rule() {
        #[derive(Debug, PartialEq)]
        enum TokenKind {
            Plus,
            Minus,
        }

        #[derive(Debug, PartialEq)]
        enum TokenError {
            Unexpected,
        }

        #[derive(Debug)]
        enum ParseError {
            ExpectedPlus,
            ExpectedMinus,
        }

        impl crate::parse::ParseError for ParseError {}

        #[derive(Debug)]
        enum AST {
            Plus(Box<AST>),
            Minus(Box<AST>),
            None,
        }

        struct PlusMinusRule;

        impl Rule<TokenKind, TokenError, ParseError> for PlusMinusRule {
            type Product = AST;

            fn build(ctx: &mut ParseContext<TokenKind, TokenError>) -> Result<Self::Product, ParseError> {
                ctx.expect_next(Ok(TokenKind::Plus), ParseError::ExpectedPlus)?;

                ctx.expect_next(Ok(TokenKind::Minus), ParseError::ExpectedMinus)?;

                Ok(AST::Plus(Box::new(AST::Minus(Box::new(AST::None)))))
            }
        }

        let tokens = vec![
            Token { val: Ok(TokenKind::Plus), str: "+".to_owned(), pos: StreamPosition::new() },
            Token { val: Ok(TokenKind::Minus), str: "-".to_owned(), pos: StreamPosition::new() },
        ];

        let product = parse_rule::<TokenKind, TokenError, ParseError, PlusMinusRule>(&tokens);
        println!("{:?}", product);
    }
}
