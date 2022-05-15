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

// TODO: consider implementing the trait on a custom result type for bubbling errors up so
// that they are pushed onto a vec that has a predefined size.

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

    // pub fn take_tokens_until<F, E>(&mut self, cond: F, default: E) -> Result<Vec<&Token<T, TokE>>, E>
    //     where
    //         F: FnMut(&Token<T, TokE>) -> Option<Result<(), E>>
    // {
    //     let mut tokens = Vec::new();
    //     loop {
    //         match self.next_token() {
    //             Some(tok) => match (cond)(tok) {
    //                 Some(Ok(_)) => return Ok(tokens),
    //                 Some(Err(e)) => return Err(e),
    //                 None => tokens.push(tok),
    //             },
    //             _ => return Err(default),
    //         }
    //     }
    // }

    pub fn expect_token<E>(&mut self, vals: &[Result<T, TokE>], err: E) -> Result<&Token<T, TokE>, E>
        where
            T: PartialEq,
            TokE: PartialEq,
    {
        match self.next_token() {
            Some(tok) => {
                for val in vals {
                    if &tok.val == val {
                        return Ok(tok);
                    }
                }
                Err(err)
            },
            _ => Err(err),
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
