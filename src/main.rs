#![feature(once_cell)]

use std::lazy::SyncLazy;

use nom::{IResult, combinator::opt, bytes::complete::tag};
use regex::{Regex, RegexSet};

fn main() {
    let input = "1+sin(2,3)";
    // let input = "-1";
    // let input = "select 1,2 from t";
    // let input = "select 1 + 2 * cos (3) < sin (5 - 1), x from t";

    let tokenizer = Tokenizer::new(LEXERS);
    let tokens: Vec<WithSpan<Token>> = tokenizer.tokenize(input).collect();

    println!("{:#?}", tokens);

    // let rpn_tokens: Vec<WithSpan<Token>> = ShuntingYard::new(
    //     tokens
    //         .clone()
    //         .into_iter()
    //         .filter(|t| t.token != Token::Whitespace),
    //     |t| associativity(&t.token),
    // )
    // .collect();

    // println!("{:#?}", rpn_tokens);
    // let ast = parse(rpn_tokens).unwrap();

    // println!("{:#?}", tokens);
    // .map(|t| t.token)
    // let rpn_tokens: Vec<Token> = ShuntingYard::new(
    // println!("{:?}", ast);
}

// ------------- AST --------------

#[derive(Debug, Clone)]
pub struct SelectStmt {
    pub projections: Vec<Expr>,
    pub from: Vec<String>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Number(i32),
    UnaryOp {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    BinaryOp {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    FunctionCall {
        name: String,
        args: Vec<Expr>,
    }
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Plus,
    Minus,
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Plus,
    Minus,
    Multiply,
    Divide,
    Gt,
    Lt,
    Gte,
    Lte,
}

// ------------- Token Rules --------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    SELECT,
    FROM,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Keyword(Keyword),
    Comma,
    LParen,
    RParen,
    Plus,
    Minus,
    Multiply,
    Divide,
    Gt,
    Lt,
    Gte,
    Lte,
    Number(usize),
    Ident(String),
    Whitespace,
}

pub const KEYWORDS: &[(&str, Keyword)] = &[("SELECT", Keyword::SELECT), ("FROM", Keyword::FROM)];
pub const LEXERS: &[(&str, fn(&str) -> Token)] = &[
    (r"\s+", |_| Token::Whitespace),
    (r"\(", |_| Token::LParen),
    (r"\)", |_| Token::RParen),
    (r",", |_| Token::Comma),
    (r"\+", |_| Token::Plus),
    (r"-", |_| Token::Minus),
    (r"\*", |_| Token::Multiply),
    (r"/", |_| Token::Divide),
    (r">", |_| Token::Gt),
    (r"<", |_| Token::Lt),
    (r">=", |_| Token::Gte),
    (r"<=", |_| Token::Lte),
    (r"[0-9]+", |s| Token::Number(s.parse().unwrap())),
    (r"\w+", |s| {
        static KEYWORD_REGEX_SET: SyncLazy<RegexSet> = SyncLazy::new(|| {
            RegexSet::new(
                KEYWORDS
                    .iter()
                    .map(|(keyword, _)| format!("(?i)^{keyword}$")),
            )
            .unwrap()
        });
        match KEYWORD_REGEX_SET.matches(s).iter().next() {
            Some(idx) => Token::Keyword(KEYWORDS[idx].1),
            None => Token::Ident(s.to_string()),
        }
    }),
];

// ------------- General Tokenizer --------------

#[derive(Debug, Clone)]
pub struct WithSpan<T> {
    pub token: T,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct Position {
    pub line: usize,
    pub col: usize,
}

#[derive(Clone)]
pub struct Tokenizer<T> {
    lexers: Vec<(Regex, fn(&str) -> T)>,
}

impl<T> Tokenizer<T> {
    pub fn new(lexers: &[(&str, fn(&str) -> T)]) -> Self {
        // assert lexer not empty
        let lexers = lexers
            .iter()
            .map(|(lexer, convertor)| (Regex::new(&format!("^{lexer}")).unwrap(), *convertor))
            .collect();

        Self { lexers }
    }

    pub fn tokenize<'m, 't>(&'m self, input: &'t str) -> Tokenize<'m, 't, T> {
        Tokenize {
            tokenizer: self,
            input,
            position: Position::default(),
            offset: 0,
        }
    }
}

pub struct Tokenize<'m, 't, T: 't> {
    tokenizer: &'m Tokenizer<T>,
    input: &'t str,
    position: Position,
    offset: usize,
}

impl<'m, 't, T: 't> Iterator for Tokenize<'m, 't, T> {
    type Item = WithSpan<T>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.offset == self.input.len() {
            return None;
        }

        for (lexer, convertor) in &self.tokenizer.lexers {
            if let Some(matched) = lexer.find(&self.input[self.offset..]) {
                assert_eq!(matched.start(), 0);

                let token = matched.as_str();

                self.offset += token.len();

                // Calculate the span
                let old_position = self.position;
                let mut line_breaks = token
                    .bytes()
                    .rev()
                    .enumerate()
                    .filter(|(_, c)| *c == b'\n')
                    .peekable();
                match line_breaks.peek().cloned() {
                    Some((token_col, _)) => {
                        self.position.line += line_breaks.count();
                        self.position.col = token_col;
                    }
                    None => {
                        self.position.col += token.len();
                    }
                }

                return Some(WithSpan {
                    token: convertor(token),
                    span: Span {
                        start: old_position,
                        end: self.position,
                    },
                });
            }
        }

        None
    }
}

// ------------- Parser Rules --------------

// #[derive(Debug, Clone, Copy, PartialEq, Eq)]
// pub enum Associativity {
//     Prefix,
//     InfixL(u8),
//     InfixR(u8),
// }

// pub fn is_unary_op(token: &Token) -> bool {
//     match token {
//         Token::Plus => true,
//         Token::Minus => true,
//         Token::Multiply => false,
//         Token::Divide => false,
//         Token::Gt => false,
//         Token::Lt => false,
//         Token::Gte => false,
//         Token::Lte => false,
//         Token::Ident(_) => true,
//         Token::LParen => false,
//         Token::RParen => false,
//         Token::Comma => false,
//         Token::Number(_) => false,
//         Token::Keyword(_) => false,
//         Token::Whitespace => false,
//     }
// }

// pub fn associativity(token: &Token) -> Option<Associativity> {
//     match token {
//         Token::Plus => Some(Associativity::InfixL(2)),
//         Token::Minus => Some(Associativity::InfixL(2)),
//         Token::Multiply => Some(Associativity::InfixL(3)),
//         Token::Divide => Some(Associativity::InfixL(3)),
//         Token::Gt => Some(Associativity::InfixL(1)),
//         Token::Lt => Some(Associativity::InfixL(1)),
//         Token::Gte => Some(Associativity::InfixL(1)),
//         Token::Lte => Some(Associativity::InfixL(1)),
//         Token::Ident(_) => Some(Associativity::Prefix),
//         Token::LParen => None,
//         Token::RParen => None,
//         Token::Comma => None,
//         Token::Number(_) => None,
//         Token::Keyword(_) => None,
//         Token::Whitespace => None,
//     }
// }

// impl Associativity {
//     pub fn lbp(&self) -> usize {
//         match self {
//             Associativity::Prefix => usize::MAX,
//             Associativity::InfixL(prec) => prec,
//             Associativity::InfixR(prec) => prec,
//             Associativity::None => 0,
//         }
//     }

//     pub fn rbp(&self) -> usize {
//         match self {
//             Associativity::Prefix => todo!(),
//             Associativity::InfixL(_) => todo!(),
//             Associativity::InfixR(_) => todo!(),
//             Associativity::None => 0,
//         }
//     }
// }

// // pub fn parse(tokens: Vec<WithSpan<Token>>) -> Result<Expr, String> {
// //     todo!()
// // }

// fn expr(i: &[Token], rbp: usize) -> IResult<&[Token], Option<Expr>> {
//   alt()
// }


// pub struct ShuntingYard<T, I>
// where
//     T: Clone,
//     I: Iterator<Item = T>,
// {
//     input: I,
//     peeked: Option<T>,
//     stack: Vec<T>,
//     associativity: fn(&T) -> Associativity,
// }

// impl<T, I> ShuntingYard<T, I>
// where
//     T: Clone,
//     I: Iterator<Item = T>,
// {
//     pub fn new(input: I, associativity: fn(&T) -> Associativity) -> Self {
//         Self {
//             input: input,
//             peeked: None,
//             stack: Vec::new(),
//             associativity,
//         }
//     }
// }

// impl<T, I> Iterator for ShuntingYard<T, I>
// where
//     T: Clone,
//     I: Iterator<Item = T>,
// {
//     type Item = T;

//     fn next(&mut self) -> Option<Self::Item> {
//         loop {
//             match self.peeked.take().into_iter().chain(&mut self.input).next() {
//                 Some(token) => match (self.associativity)(&token) {
//                     Associativity::Unary => self.stack.push(token),
//                     Associativity::LBinary(fixty) => {
//                         if let Some(
//                             Associativity::LBinary(stack_fixty)
//                             | Associativity::RBinary(stack_fixty),
//                         ) = self.stack.last().map(self.associativity)
//                         {
//                             if stack_fixty >= fixty {
//                                 self.peeked = Some(token);
//                                 return self.stack.pop();
//                             }
//                         }
//                         self.stack.push(token);
//                     }
//                     Associativity::RBinary(fixty) => {
//                         if let Some(
//                             Associativity::LBinary(stack_fixty)
//                             | Associativity::RBinary(stack_fixty),
//                         ) = self.stack.last().map(self.associativity)
//                         {
//                             if stack_fixty > fixty {
//                                 self.peeked = Some(token);
//                                 return self.stack.pop();
//                             }
//                         }
//                         self.stack.push(token);
//                     }
//                     Associativity::LParen => self.stack.push(token),
//                     Associativity::RParen => {
//                         // todo: return error
//                         assert!(!self.stack.is_empty());
//                         let output = self.stack.pop().unwrap();
//                         match (self.associativity)(&output) {
//                             Associativity::LParen => {
//                                 if let Some(Associativity::Unary) =
//                                     self.stack.last().map(self.associativity)
//                                 {
//                                     return self.stack.pop();
//                                 }
//                             }
//                             _ => {
//                                 self.peeked = Some(token);
//                                 return Some(output);
//                             }
//                         }
//                     }
//                     Associativity::None => return Some(token),
//                 },
//                 None => {
//                     // todo: return error
//                     assert_ne!(
//                         self.stack.last().map(self.associativity),
//                         Some(Associativity::LParen)
//                     );
//                     return self.stack.pop();
//                 }
//             };
//         }
//     }
// }