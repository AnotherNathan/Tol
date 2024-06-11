use std::iter::FusedIterator;

use super::span::Span;

pub fn tokenize(source: &str) -> Lexer {
    Lexer {
        source,
        byte_pos: 0,
        column: 0,
        line: 0,
    }
}

pub struct Lexer<'a> {
    source: &'a str,
    byte_pos: usize,
    line: usize,
    column: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum TokenKind {
    NewLine,
    Dot,
    Comma,
    Colon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Smaller,
    Greater,
    Exclamation,
    Equal,
    Plus,
    Dash,
    Slash,
    Asterisk,
    Ampersand,
    Bar,
    Identifier,
    Literal(Literal),
    Unknown(char),
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Literal {
    Int(u128),
    Float(f64),
    String,
    Rune(char),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_white_spaces();

        let first_char = self.peek_char()?;
        match first_char {
            '\n' => return self.build_token_and_advance(TokenKind::NewLine, "\n"),
            '.' => return self.build_token_and_advance(TokenKind::Dot, "."),
            ',' => return self.build_token_and_advance(TokenKind::Comma, ","),
            ':' => return self.build_token_and_advance(TokenKind::Colon, ":"),
            '(' => return self.build_token_and_advance(TokenKind::LParen, "("),
            ')' => return self.build_token_and_advance(TokenKind::RParen, ")"),
            '{' => return self.build_token_and_advance(TokenKind::LBrace, "{"),
            '}' => return self.build_token_and_advance(TokenKind::RBrace, "}"),
            '[' => return self.build_token_and_advance(TokenKind::LBracket, "["),
            ']' => return self.build_token_and_advance(TokenKind::RBracket, "]"),
            '<' => return self.build_token_and_advance(TokenKind::Smaller, "<"),
            '>' => return self.build_token_and_advance(TokenKind::Greater, ">"),
            '!' => return self.build_token_and_advance(TokenKind::Exclamation, "!"),
            '=' => return self.build_token_and_advance(TokenKind::Equal, "="),
            '+' => return self.build_token_and_advance(TokenKind::Plus, "+"),
            '-' => return self.build_token_and_advance(TokenKind::Dash, "-"),
            '/' => return self.build_token_and_advance(TokenKind::Slash, "/"),
            '*' => return self.build_token_and_advance(TokenKind::Asterisk, "*"),
            '&' => return self.build_token_and_advance(TokenKind::Ampersand, "&"),
            '|' => return self.build_token_and_advance(TokenKind::Bar, "|"),
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

        self.build_token_and_advance(
            TokenKind::Unknown(first_char),
            &self.source_at_current_pos()[0..first_char.len_utf8()],
        )
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

    fn peek_char(&self) -> Option<char> {
        self.source.get(self.byte_pos..)?.chars().nth(0)
    }

    fn source_at_current_pos(&self) -> &'a str {
        self.source.get(self.byte_pos..).unwrap()
    }

    fn parse_digit_literal(&mut self) -> Option<Token> {
        let source = self.source_at_current_pos();
        let length = source
            .chars()
            .take_while(|c| c.is_ascii_digit() || *c == '.')
            .map(|c| c.len_utf8())
            .sum();
        let literal = &source[0..length];
        if let Ok(val) = literal.parse::<u128>() {
            return self.build_token_and_advance(TokenKind::Literal(Literal::Int(val)), literal);
        }
        if let Ok(val) = literal.parse::<f64>() {
            return self.build_token_and_advance(TokenKind::Literal(Literal::Float(val)), literal);
        }

        None
    }

    fn parse_rune_literal(&mut self) -> Option<Token> {
        let source = self.source_at_current_pos();
        let closing = source.chars().nth(2)?;
        if closing != '\'' {
            return None;
        }
        let c = source.chars().nth(1).unwrap();
        let length = 2 * '\''.len_utf8() + c.len_utf8();
        let literal = &source[0..length];
        return self.build_token_and_advance(TokenKind::Literal(Literal::Rune(c)), literal);
    }

    fn parse_string_literal(&mut self) -> Option<Token> {
        let source = self.source_at_current_pos();
        let length = source
            .chars()
            .skip(1)
            .take_while(|c| *c != '"')
            .map(|c| c.len_utf8())
            .sum::<usize>()
            + 2;
        let literal = &source[0..length];
        return self.build_token_and_advance(TokenKind::Literal(Literal::String), literal);
    }

    fn parse_identifier(&mut self) -> Option<Token> {
        let source = self.source_at_current_pos();
        let length = source
            .chars()
            .take_while(|c| c.is_alphabetic() || c.is_ascii_digit() || *c == '_')
            .map(|c| c.len_utf8())
            .sum();
        let identifier = &source[0..length];
        return self.build_token_and_advance(TokenKind::Identifier, identifier);
    }

    fn build_token_and_advance(&mut self, kind: TokenKind, token: &str) -> Option<Token> {
        let length = token.len();
        let line_count = token.lines().count();
        let column_end = if line_count == 0 {
            self.column + token.chars().count()
        } else {
            token.lines().last().map(|l| l.chars().count()).unwrap_or(0)
        };
        let token = Token {
            kind,
            span: Span {
                file_pos: self.byte_pos..(self.byte_pos + length),
                line: self.line..(self.line + line_count),
                column: self.column..column_end,
            },
        };
        self.byte_pos += length;
        self.line += line_count;
        self.column = column_end;
        Some(token)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn get_tokens() {
        let source = ":\n()[]{}<>!+-/=&|* ident ,.123 12.4 _underscore_ident ";
        let mut lexer = tokenize(source);

        assert_next_token(&mut lexer, TokenKind::Colon, ":", source);
        assert_next_token(&mut lexer, TokenKind::NewLine, "\n", source);
        assert_next_token(&mut lexer, TokenKind::LParen, "(", source);
        assert_next_token(&mut lexer, TokenKind::RParen, ")", source);
        assert_next_token(&mut lexer, TokenKind::LBracket, "[", source);
        assert_next_token(&mut lexer, TokenKind::RBracket, "]", source);
        assert_next_token(&mut lexer, TokenKind::LBrace, "{", source);
        assert_next_token(&mut lexer, TokenKind::RBrace, "}", source);
        assert_next_token(&mut lexer, TokenKind::Smaller, "<", source);
        assert_next_token(&mut lexer, TokenKind::Greater, ">", source);
        assert_next_token(&mut lexer, TokenKind::Exclamation, "!", source);
        assert_next_token(&mut lexer, TokenKind::Plus, "+", source);
        assert_next_token(&mut lexer, TokenKind::Dash, "-", source);
        assert_next_token(&mut lexer, TokenKind::Slash, "/", source);
        assert_next_token(&mut lexer, TokenKind::Equal, "=", source);
        assert_next_token(&mut lexer, TokenKind::Ampersand, "&", source);
        assert_next_token(&mut lexer, TokenKind::Bar, "|", source);
        assert_next_token(&mut lexer, TokenKind::Asterisk, "*", source);
        assert_next_token(&mut lexer, TokenKind::Identifier, "ident", source);
        assert_next_token(&mut lexer, TokenKind::Comma, ",", &source);
        assert_next_token(&mut lexer, TokenKind::Dot, ".", &source);
        assert_next_token(
            &mut lexer,
            TokenKind::Literal(Literal::Int(123)),
            "123",
            &source,
        );
        assert_next_token(
            &mut lexer,
            TokenKind::Literal(Literal::Float(12.4)),
            "12.4",
            &source,
        );
        assert_next_token(
            &mut lexer,
            TokenKind::Identifier,
            "_underscore_ident",
            &source,
        );
    }

    fn assert_next_token(
        lexer: &mut Lexer,
        expected_kind: TokenKind,
        expected_token: &str,
        source: &str,
    ) {
        let token = match lexer.next() {
            Some(t) => t,
            None => panic!("expected there to be a token"),
        };

        assert_eq!(token.kind, expected_kind);
        assert_eq!(&source[token.span.file_pos], expected_token);
    }
}
