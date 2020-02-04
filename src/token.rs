// Reference: https://chr4.org/blog/2016/12/09/writing-an-interpreter-in-rust/
#[derive(Debug, PartialEq)]
pub enum TokenType {
    Illegal,
    EndOfFile,

    // Identifiers + literals
    Ident,
    Integer,

    // Operators,
    Assign,
    Plus,
    Minus,

    // Delimiters
    Comma,
    Semicolon,

    // Parens
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,

    // Keywords
    Function,
    Let
    
}

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

pub fn lookup_ident(ident: &str) -> TokenType {
    match ident {
        "function" => TokenType::Function,
        "let" => TokenType::Let,
        "true" => TokenType::True,
        "false" => TokenType::False,
        "if" => TokenType::If,
        "else" => TokenType::Else,
        "return" => TokenType::Return,
        _ => TokenType::Ident,
    }
}

#[test]
fn lookup_ident_test() {
    assert_eq!(lookup_ident("if"), TokenType::If);
}



