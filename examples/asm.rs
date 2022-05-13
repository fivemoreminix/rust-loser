use rust_loser::lex;

#[derive(Debug, Clone)]
enum Token {
    Mov,
    Add,
    Sub,
    Mul,
    Div,
    Ret,
    Identifier,
    Comma,
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
        .rule(String::from(","), Token::Comma)
        .rule(String::from("mov"), Token::Mov)
        .rule(String::from("add"), Token::Add)
        .rule(String::from("sub"), Token::Sub)
        .rule(String::from("mul"), Token::Mul)
        .rule(String::from("div"), Token::Div)
        .rule(String::from("ret"), Token::Ret)
        .rule_custom(|s| s.chars().next().unwrap().is_alphabetic(), |builder| {
            for (i, c) in builder.get_string().chars().enumerate() {
                if !c.is_alphabetic() {
                    let mut tokens = Vec::new();
                    tokens.push(builder.token(0, i, Ok(Token::Identifier)));
                    tokens.push(builder.token(i, builder.get_string().len(), Err(TokenError::NoMatchingRule)));
                    return tokens;
                }
            }
            vec![builder.token(0, builder.get_string().len(), Ok(Token::Identifier))]
        });

    let source = "  mov eax ecx
  mul eax ecx
  mov rax eax

  ret";

    let tokens = lex::Lexer::new(source.as_bytes(), &rules).build_tokens();
    println!("Tokens: {:#?}\n", tokens);
}
