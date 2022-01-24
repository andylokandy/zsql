// use pest_derive::Parser;
use pest::{
    iterators::Pair,
    prec_climber::{Assoc, Operator, PrecClimber},
    Parser as _,
};
use pest_derive::Parser;

#[derive(Debug, Clone, PartialEq)]
enum Expr {
    Number(i64),
    Float(f64),
    FunctionCall(String, Box<Expr>),
    BinaryOp {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum BinaryOp {
    Plus,
    Minus,
    Times,
    Divides,
}

#[derive(Parser)]
#[grammar = "expr.pest"]
struct ExprParser;

fn pair_to_expr(pair: Pair<Rule>, climber: &PrecClimber<Rule>) -> Expr {
    let primary = |pair| pair_to_expr(pair, climber);
    let infix = |left: Expr, op: Pair<Rule>, right: Expr| {
        let op = match op.as_rule() {
            Rule::Plus => BinaryOp::Plus,
            Rule::Minus => BinaryOp::Minus,
            Rule::Times => BinaryOp::Times,
            Rule::Divides => BinaryOp::Divides,
            _ => unreachable!(),
        };
        Expr::BinaryOp {
            op,
            left: Box::new(left),
            right: Box::new(right),
        }
    };

    match pair.as_rule() {
        Rule::Expr => climber.climb(pair.into_inner(), primary, infix),
        Rule::int => Expr::Number(pair.as_str().parse().unwrap()),
        Rule::float => Expr::Float(pair.as_str().parse().unwrap()),
        Rule::Group => pair_to_expr(pair.into_inner().next().unwrap(), climber),
        Rule::FunctionCall => {
            let mut inner = pair.into_inner();
            Expr::FunctionCall(
                inner.next().unwrap().as_str().to_string(),
                Box::new(pair_to_expr(inner.next().unwrap(), climber)),
            )
        }
        _ => unreachable!(),
    }
}

fn main() {
    let climber = PrecClimber::new(vec![
        Operator::new(Rule::Plus, Assoc::Left) | Operator::new(Rule::Minus, Assoc::Left),
        Operator::new(Rule::Times, Assoc::Left) | Operator::new(Rule::Divides, Assoc::Left),
    ]);

    // let input = "1+2";
    let input = "1+cos 2 *(-1000)";

    let expr_pair = ExprParser::parse(Rule::Expr, input)
        .unwrap()
        .next()
        .unwrap();
    let expr = pair_to_expr(expr_pair, &climber);

    dbg!(input);
    dbg!(expr);
}
