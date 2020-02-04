use token;

pub struct Lexer<'a> {
    input: &'a str,
    position: usize,
    read_position: usize,
    ch: Option<char>,
}
impl <'a> Lexer<'a> {
    pub fn new(input: &str)-> Lexer {
        let mut l = Lexer {
            input: input,
            position: 0,
            read_position:0,
            ch: None,
        };
        l.read_char();
        return l;
    }
}
let mut tok = token::Token {
    token_type: token::TokenTypeIllegal,
    literal: String::new(),
}

pub fn next_token(&mut self){
    let mut tok = token::Token {
        token_type: token::TokenTypeIllegal,
        literal: String.new(),
    };
    self.skip_whitespace();
    

}
