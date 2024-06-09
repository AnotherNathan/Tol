use std::{iter::FusedIterator, ops::Range};

use itertools::Itertools;

pub fn tokenize(source: &str) -> Lexer {
    Lexer {
        source,
        byte_pos: 0,
    }
}

pub struct Lexer<'a> {
    source: &'a str,
    byte_pos: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Token<'a> {
    NewLine,
    Dot,
    Colon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Less,
    Greater,
    Exclamation,
    Equal,
    Plus,
    Minus,
    Slash,
    Asterisk,
    Ampersand,
    Identifier(&'a str),
    Literal(Literal<'a>),
    Unknown(char),
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Literal<'a> {
    Int(u128),
    Float(f64),
    String(&'a str),
    Rune(char),
}

pub struct TokenData<'a> {
    pub token: Token<'a>,
    pub span: Range<usize>,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = TokenData<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_white_spaces();

        let first_char = self.peek_char()?;
        match first_char {
            '\n' => return self.single_char_token(Token::NewLine, '\n'),
            '.' => return self.single_char_token(Token::Dot, '.'),
            ':' => return self.single_char_token(Token::Colon, ':'),
            '(' => return self.single_char_token(Token::LParen, '('),
            ')' => return self.single_char_token(Token::RParen, ')'),
            '{' => return self.single_char_token(Token::LBrace, '{'),
            '}' => return self.single_char_token(Token::RBrace, '}'),
            '[' => return self.single_char_token(Token::LBracket, '['),
            ']' => return self.single_char_token(Token::RBracket, ']'),
            '<' => return self.single_char_token(Token::Less, '<'),
            '>' => return self.single_char_token(Token::Greater, '>'),
            '!' => return self.single_char_token(Token::Exclamation, '!'),
            '=' => return self.single_char_token(Token::Equal, '='),
            '+' => return self.single_char_token(Token::Plus, '+'),
            '-' => return self.single_char_token(Token::Minus, '-'),
            '/' => return self.single_char_token(Token::Slash, '/'),
            '*' => return self.single_char_token(Token::Asterisk, '*'),
            '&' => return self.single_char_token(Token::Ampersand, '&'),
            _ => (),
        }

        if first_char.is_numeric() {
            let literal = self.parse_digit_literal();
            if literal.is_some() {
                return literal;
            }
        }

        if first_char == '"' {
            let literal = self.parse_string_literal();
            if literal.is_some() {
                return literal;
            }
        }

        if first_char == '\'' {
            let literal = self.parse_rune_literal();
            if literal.is_some() {
                return literal;
            }
        }

        if first_char.is_alphabetic() || first_char == '_' {
            let identifier = self.parse_identifier();
            if identifier.is_some() {
                return identifier;
            }
        }

        self.build_token_and_advance(Token::Unknown(first_char), first_char.len_utf8())
    }
}

impl<'a> FusedIterator for Lexer<'a> {}

impl<'a> Lexer<'a> {
    fn skip_white_spaces(&mut self) {
        while let Some(c) = self.peek_char() {
            if !c.is_whitespace() || c == '\n' {
                return;
            }

            self.byte_pos += c.len_utf8();
        }
    }

    fn single_char_token(&mut self, token: Token<'a>, c: char) -> Option<TokenData<'a>> {
        self.build_token_and_advance(token, c.len_utf8())
    }

    fn peek_char(&self) -> Option<char> {
        self.source.get(self.byte_pos..)?.chars().nth(0)
    }

    fn source_at_current_pos(&self) -> &'a str {
        self.source.get(self.byte_pos..).unwrap()
    }

    fn parse_digit_literal(&mut self) -> Option<TokenData<'a>> {
        let source = self.source_at_current_pos();
        let length = source
            .char_indices()
            .take_while(|(_, c)| c.is_ascii_digit() || *c == '.')
            .last()
            .map(|(i, _)| i + 1)
            .unwrap_or(0);
        let source = &self.source[self.byte_pos..(self.byte_pos + length)];
        if let Ok(val) = source.parse::<u128>() {
            return self.build_token_and_advance(Token::Literal(Literal::Int(val)), source.len());
        }
        if let Ok(val) = source.parse::<f64>() {
            return self.build_token_and_advance(Token::Literal(Literal::Float(val)), source.len());
        }

        None
    }

    fn parse_rune_literal(&mut self) -> Option<TokenData<'a>> {
        let source = self.source_at_current_pos();
        let closing = source.chars().nth(2)?;
        if closing != '\'' {
            return None;
        }
        let c = source.chars().nth(1).unwrap();
        return self.build_token_and_advance(
            Token::Literal(Literal::Rune(c)),
            c.len_utf8() + 2 * '\''.len_utf8(),
        );
    }

    fn parse_string_literal(&mut self) -> Option<TokenData<'a>> {
        let source = self.source_at_current_pos();
        let length = source
            .char_indices()
            .skip(1)
            .take_while_inclusive(|(_, c)| *c != '"')
            .last()?
            .0
            + 1;
        return self
            .build_token_and_advance(Token::Literal(Literal::String(&source[0..length])), length);
    }

    fn parse_identifier(&mut self) -> Option<TokenData<'a>> {
        let source = self.source_at_current_pos();
        let last_index = source
            .char_indices()
            .take_while(|(_, c)| c.is_alphabetic() || c.is_ascii_digit() || *c == '_')
            .last()?
            .0;
        let identifier = &source[0..=last_index];
        return self.build_token_and_advance(Token::Identifier(identifier), last_index + 1);
    }

    fn build_token_and_advance(
        &mut self,
        token: Token<'a>,
        length: usize,
    ) -> Option<TokenData<'a>> {
        let data = TokenData {
            token,
            span: self.byte_pos..(self.byte_pos + length),
        };
        self.byte_pos += length;
        Some(data)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn get_tokens() {
        let source = ":\n()[]{}<>!+-/=&* ident .123 12.4 _underscore_ident ";
        let mut lexer = tokenize(source);

        assert_eq!(Some(Token::Colon), next_token(&mut lexer));
        assert_eq!(Some(Token::NewLine), next_token(&mut lexer));
        assert_eq!(Some(Token::LParen), next_token(&mut lexer));
        assert_eq!(Some(Token::RParen), next_token(&mut lexer));
        assert_eq!(Some(Token::LBracket), next_token(&mut lexer));
        assert_eq!(Some(Token::RBracket), next_token(&mut lexer));
        assert_eq!(Some(Token::LBrace), next_token(&mut lexer));
        assert_eq!(Some(Token::RBrace), next_token(&mut lexer));
        assert_eq!(Some(Token::Less), next_token(&mut lexer));
        assert_eq!(Some(Token::Greater), next_token(&mut lexer));
        assert_eq!(Some(Token::Exclamation), next_token(&mut lexer));
        assert_eq!(Some(Token::Plus), next_token(&mut lexer));
        assert_eq!(Some(Token::Minus), next_token(&mut lexer));
        assert_eq!(Some(Token::Slash), next_token(&mut lexer));
        assert_eq!(Some(Token::Equal), next_token(&mut lexer));
        assert_eq!(Some(Token::Ampersand), next_token(&mut lexer));
        assert_eq!(Some(Token::Asterisk), next_token(&mut lexer));
        assert_eq!(Some(Token::Identifier("ident")), next_token(&mut lexer));
        assert_eq!(Some(Token::Dot), next_token(&mut lexer));
        assert_eq!(
            Some(Token::Literal(Literal::Int(123))),
            next_token(&mut lexer)
        );
        assert_eq!(
            Some(Token::Literal(Literal::Float(12.4))),
            next_token(&mut lexer)
        );
        assert_eq!(
            Some(Token::Identifier("_underscore_ident")),
            next_token(&mut lexer)
        );
    }

    fn next_token<'a>(lexer: &mut Lexer<'a>) -> Option<Token<'a>> {
        lexer.next().map(|t| t.token)
    }
}
