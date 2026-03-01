///! Lexer and parser for Hashlisp.
///!
///! Syntax is standard Scheme s-expressions:
///!   atom     ::= number | string | #t | #f | symbol
///!   list     ::= '(' expr* ')'
///!   dotted   ::= '(' expr+ '.' expr ')'
///!   quoted   ::= '\'' expr
///!   vector   ::= '#(' expr* ')'

use crate::heap::Heap;
use crate::symbol::SymbolTable;
use crate::value::Val;

// ── Token ──

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    LParen,
    RParen,
    Quote,
    Quasiquote,
    Unquote,
    UnquoteSplicing,
    Dot,
    HashLParen,      // #(
    Int(i64),
    Str(String),
    Symbol(String),
    Bool(bool),
    Char(char),
}

pub struct Lexer<'a> {
    input: &'a [u8],
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            input: input.as_bytes(),
            pos: 0,
        }
    }

    fn peek(&self) -> Option<u8> {
        self.input.get(self.pos).copied()
    }

    fn advance(&mut self) -> Option<u8> {
        let b = self.input.get(self.pos).copied()?;
        self.pos += 1;
        Some(b)
    }

    fn skip_whitespace_and_comments(&mut self) {
        loop {
            match self.peek() {
                Some(b) if b.is_ascii_whitespace() => {
                    self.pos += 1;
                }
                Some(b';') => {
                    // line comment
                    while let Some(b) = self.peek() {
                        self.pos += 1;
                        if b == b'\n' {
                            break;
                        }
                    }
                }
                _ => break,
            }
        }
    }

    fn is_delimiter(b: u8) -> bool {
        matches!(
            b,
            b'(' | b')' | b'\'' | b'`' | b',' | b'"' | b';' | b' ' | b'\t' | b'\n' | b'\r'
        )
    }

    fn read_string(&mut self) -> Result<String, String> {
        // opening " already consumed
        let mut s = String::new();
        loop {
            match self.advance() {
                None => return Err("unterminated string".into()),
                Some(b'"') => return Ok(s),
                Some(b'\\') => match self.advance() {
                    Some(b'n') => s.push('\n'),
                    Some(b't') => s.push('\t'),
                    Some(b'\\') => s.push('\\'),
                    Some(b'"') => s.push('"'),
                    Some(c) => {
                        return Err(format!("unknown escape \\{}", c as char));
                    }
                    None => return Err("unterminated escape in string".into()),
                },
                Some(b) if b < 0x80 => s.push(b as char),
                Some(b) => {
                    // UTF-8 multi-byte sequence
                    let len = if b & 0xE0 == 0xC0 { 2 }
                         else if b & 0xF0 == 0xE0 { 3 }
                         else if b & 0xF8 == 0xF0 { 4 }
                         else { return Err("invalid UTF-8 byte in string".into()); };
                    let start = self.pos - 1;
                    for _ in 1..len {
                        match self.advance() {
                            Some(c) if c & 0xC0 == 0x80 => {}
                            _ => return Err("invalid UTF-8 continuation byte in string".into()),
                        }
                    }
                    let slice = std::str::from_utf8(&self.input[start..self.pos])
                        .map_err(|e| format!("invalid UTF-8 in string: {e}"))?;
                    s.push_str(slice);
                }
            }
        }
    }

    fn read_symbol_or_number(&mut self) -> Token {
        let start = self.pos;
        while let Some(b) = self.peek() {
            if Self::is_delimiter(b) {
                break;
            }
            self.pos += 1;
        }
        let text = std::str::from_utf8(&self.input[start..self.pos]).unwrap();

        // Try integer
        if let Ok(i) = text.parse::<i64>() {
            return Token::Int(i);
        }
        Token::Symbol(text.to_string())
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, String> {
        let mut tokens = Vec::new();
        loop {
            self.skip_whitespace_and_comments();
            match self.peek() {
                None => return Ok(tokens),
                Some(b'(') => {
                    self.advance();
                    tokens.push(Token::LParen);
                }
                Some(b')') => {
                    self.advance();
                    tokens.push(Token::RParen);
                }
                Some(b'\'') => {
                    self.advance();
                    tokens.push(Token::Quote);
                }
                Some(b'`') => {
                    self.advance();
                    tokens.push(Token::Quasiquote);
                }
                Some(b',') => {
                    self.advance();
                    if self.peek() == Some(b'@') {
                        self.advance();
                        tokens.push(Token::UnquoteSplicing);
                    } else {
                        tokens.push(Token::Unquote);
                    }
                }
                Some(b'"') => {
                    self.advance();
                    let s = self.read_string()?;
                    tokens.push(Token::Str(s));
                }
                Some(b'#') => {
                    self.advance();
                    match self.peek() {
                        Some(b't') => {
                            self.advance();
                            // Check it's not part of a longer symbol
                            if self.peek().map_or(true, Self::is_delimiter) {
                                tokens.push(Token::Bool(true));
                            } else {
                                // read rest as symbol
                                self.pos -= 2; // backtrack
                                tokens.push(self.read_symbol_or_number());
                            }
                        }
                        Some(b'f') => {
                            self.advance();
                            if self.peek().map_or(true, Self::is_delimiter) {
                                tokens.push(Token::Bool(false));
                            } else {
                                self.pos -= 2;
                                tokens.push(self.read_symbol_or_number());
                            }
                        }
                        Some(b'(') => {
                            self.advance();
                            tokens.push(Token::HashLParen);
                        }
                        Some(b'\\') => {
                            self.advance();
                            // character literal: #\a  or  #\newline  etc
                            let start = self.pos;
                            while let Some(b) = self.peek() {
                                if Self::is_delimiter(b) {
                                    break;
                                }
                                self.pos += 1;
                            }
                            let name =
                                std::str::from_utf8(&self.input[start..self.pos]).unwrap();
                            let c = match name {
                                "space" => ' ',
                                "newline" => '\n',
                                "tab" => '\t',
                                "return" => '\r',
                                s if s.chars().count() == 1 => s.chars().next().unwrap(),
                                _ => return Err(format!("unknown character name: {name}")),
                            };
                            tokens.push(Token::Char(c));
                        }
                        _ => {
                            return Err("unexpected # sequence".into());
                        }
                    }
                }
                Some(b'.') => {
                    // Could be dot or a number starting with .
                    let next = self.input.get(self.pos + 1).copied();
                    if next.map_or(true, Self::is_delimiter) {
                        self.advance();
                        tokens.push(Token::Dot);
                    } else {
                        tokens.push(self.read_symbol_or_number());
                    }
                }
                Some(_) => {
                    tokens.push(self.read_symbol_or_number());
                }
            }
        }
    }
}

// ── Parser ──

pub struct Parser<'a> {
    tokens: &'a [Token],
    pos: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Parser { tokens, pos: 0 }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn advance(&mut self) -> Option<&Token> {
        let t = self.tokens.get(self.pos)?;
        self.pos += 1;
        Some(t)
    }

    fn expect(&mut self, expected: &Token) -> Result<(), String> {
        match self.advance() {
            Some(t) if t == expected => Ok(()),
            Some(t) => Err(format!("expected {expected:?}, got {t:?}")),
            None => Err(format!("expected {expected:?}, got EOF")),
        }
    }

    /// Parse one expression, returning it as a Val (using the heap for lists).
    pub fn parse_expr(
        &mut self,
        heap: &mut Heap,
        syms: &mut SymbolTable,
    ) -> Result<Val, String> {
        match self.advance() {
            None => Err("unexpected end of input".into()),
            Some(Token::Int(i)) => Ok(Val::int(*i)),
            Some(Token::Bool(b)) => Ok(Val::boolean(*b)),
            Some(Token::Char(c)) => Ok(Val::char_(*c)),
            Some(Token::Str(s)) => {
                let s = s.clone();
                Ok(heap.alloc_string(&s))
            }
            Some(Token::Symbol(name)) => {
                let name = name.clone();
                Ok(Val::symbol(syms.intern(&name)))
            }
            Some(Token::Quote) => {
                let quoted = self.parse_expr(heap, syms)?;
                let quote_sym = Val::symbol(syms.intern("quote"));
                let inner = heap.cons(quoted, Val::nil());
                Ok(heap.cons(quote_sym, inner))
            }
            Some(Token::Quasiquote) => {
                let body = self.parse_expr(heap, syms)?;
                let sym = Val::symbol(syms.intern("quasiquote"));
                let inner = heap.cons(body, Val::nil());
                Ok(heap.cons(sym, inner))
            }
            Some(Token::Unquote) => {
                let body = self.parse_expr(heap, syms)?;
                let sym = Val::symbol(syms.intern("unquote"));
                let inner = heap.cons(body, Val::nil());
                Ok(heap.cons(sym, inner))
            }
            Some(Token::UnquoteSplicing) => {
                let body = self.parse_expr(heap, syms)?;
                let sym = Val::symbol(syms.intern("unquote-splicing"));
                let inner = heap.cons(body, Val::nil());
                Ok(heap.cons(sym, inner))
            }
            Some(Token::LParen) => self.parse_list(heap, syms),
            Some(Token::HashLParen) => self.parse_vector(heap, syms),
            Some(Token::RParen) => Err("unexpected ')'".into()),
            Some(Token::Dot) => Err("unexpected '.'".into()),
        }
    }

    fn parse_list(
        &mut self,
        heap: &mut Heap,
        syms: &mut SymbolTable,
    ) -> Result<Val, String> {
        let mut elems = Vec::new();
        loop {
            match self.peek() {
                None => return Err("unterminated list".into()),
                Some(Token::RParen) => {
                    self.advance();
                    return Ok(heap.list(&elems));
                }
                Some(Token::Dot) => {
                    self.advance();
                    let cdr = self.parse_expr(heap, syms)?;
                    self.expect(&Token::RParen)?;
                    // build dotted list
                    let mut result = cdr;
                    for v in elems.iter().rev() {
                        result = heap.cons(*v, result);
                    }
                    return Ok(result);
                }
                _ => {
                    let expr = self.parse_expr(heap, syms)?;
                    elems.push(expr);
                }
            }
        }
    }

    fn parse_vector(
        &mut self,
        heap: &mut Heap,
        syms: &mut SymbolTable,
    ) -> Result<Val, String> {
        let mut elems = Vec::new();
        loop {
            match self.peek() {
                None => return Err("unterminated vector".into()),
                Some(Token::RParen) => {
                    self.advance();
                    return Ok(heap.alloc_vector(elems));
                }
                _ => {
                    let expr = self.parse_expr(heap, syms)?;
                    elems.push(expr);
                }
            }
        }
    }

    /// Parse all top-level expressions from the token stream.
    pub fn parse_all(
        &mut self,
        heap: &mut Heap,
        syms: &mut SymbolTable,
    ) -> Result<Vec<Val>, String> {
        let mut exprs = Vec::new();
        while self.pos < self.tokens.len() {
            exprs.push(self.parse_expr(heap, syms)?);
        }
        Ok(exprs)
    }
}

/// Convenience: parse a string into a list of Val expressions.
pub fn parse(
    input: &str,
    heap: &mut Heap,
    syms: &mut SymbolTable,
) -> Result<Vec<Val>, String> {
    let tokens = Lexer::new(input).tokenize()?;
    Parser::new(&tokens).parse_all(heap, syms)
}
