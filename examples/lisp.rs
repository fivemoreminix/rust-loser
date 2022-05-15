use rust_loser::{lex, parse};
use rust_loser::parse::Rule;

#[derive(Debug, Copy, Clone, PartialEq)]
enum Token {
    LParen,
    RParen,
    Number,
    Identifier,
}

#[derive(Debug, Copy, Clone, PartialEq)]
enum TokenError {
    NoMatchingRule,
}

impl lex::TokenError for TokenError {
    fn no_matching_rule() -> Self {
        TokenError::NoMatchingRule
    }
}

#[derive(Debug, Clone)]
enum Expr {
    Func(String, Vec<Expr>),
    Var(String),
    Num(f64),
}

#[derive(Debug, Copy, Clone)]
enum ParseError {
    ExpectedLParen,
    ExpectedRParen,
    ExpectedIdOrNum,
    UnexpectedEOF,
}

impl parse::ParseError for ParseError {}

struct ExprRule;

impl parse::Rule<Token, TokenError, ParseError> for ExprRule {
    type Product = Expr;

    fn build(ctx: &mut parse::ParseContext<Token, TokenError>) -> Result<Self::Product, ParseError> {
        ctx.expect_token(&[Ok(Token::LParen)], ParseError::ExpectedLParen)?;
        let tok = ctx.expect_token(&[Ok(Token::Identifier), Ok(Token::Number)], ParseError::ExpectedIdOrNum)?;
        let tok_str = tok.str.to_owned();

        if tok.val == Ok(Token::Number) {
            ctx.expect_token(&[Ok(Token::RParen)], ParseError::ExpectedRParen)?;
            return Ok(Expr::Num(tok_str.parse().unwrap()));
        }

        let tok2 = ctx.peek_token(0);
        if tok2.is_none() {
            return Err(ParseError::UnexpectedEOF);
        }

        if tok2.unwrap().val == Ok(Token::RParen) {
            let _ = ctx.next_token().unwrap();
            return Ok(Expr::Var(tok_str));
        }

        let mut params = Vec::new();
        loop {
            if let Some(tok) = ctx.peek_token(0) {
                if tok.val == Ok(Token::RParen) {
                    break;
                }
            } else {
                return Err(ParseError::UnexpectedEOF);
            }
            params.push(ExprRule::build(ctx)?);
        }

        ctx.expect_token(&[Ok(Token::RParen)], ParseError::ExpectedRParen)?;
        Ok(Expr::Func(tok_str, params))
    }
}

fn main() {
    let mut rules = lex::LexerRules::<Token, TokenError>::default();
    rules
        .skips(|c| c.is_whitespace())
        .rule(String::from('('), Token::LParen)
        .rule(String::from(')'), Token::RParen)
        .rule_custom(|c| c.is_digit(10), |builder| {
            vec![builder.token(0, builder.get_string().len(), Ok(Token::Number))]
        })
        .rule_custom(|c| c != '(' && c != ')', |builder| {
            vec![builder.token(0, builder.get_string().len(), Ok(Token::Identifier))]
        });

    let source = "(print (+ (25) (+ (myvar) (300) ( abcdefghi )) ) )";

    let tokens = lex::Lexer::new(source.as_bytes(), &rules).build_tokens();
    println!("Tokens: {:#?}\n", &tokens);

    let mut ctx = parse::ParseContext::new(&tokens);
    let expr = ExprRule::build(&mut ctx);
    println!("Tree: {:#?}", expr);
}
