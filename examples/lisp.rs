use rust_loser::lex;

#[derive(Debug, Copy, Clone)]
enum Token {
    LParen,
    RParen,
    Number,
    Identifier,
}

#[derive(Debug, Copy, Clone)]
enum TokenError {
    NoMatchingRule,
}

impl lex::TokenError for TokenError {
    fn no_matching_rule() -> Self {
        TokenError::NoMatchingRule
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

    let source = "(print 2)";

    let tokens = lex::Lexer::new(source.as_bytes(), &rules).build_tokens();
    println!("Tokens: {:#?}\n", tokens);
}
