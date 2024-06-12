use anyhow::Result;
use pest::iterators::{Pair, Pairs};
use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "miniml.pest"]
struct MiniMLParser;

#[derive(Clone, PartialEq, Debug)]
enum Expression {
    True,
    False,
    Num(u32),
    Var(Variable),
    Nil,
    Let(Variable, Box<Expression>, Box<Expression>),
    Not(Box<Expression>),
    If(Box<Expression>, Box<Expression>, Box<Expression>),
    Succ(Box<Expression>),
    Pred(Box<Expression>),
    Fst(Box<Expression>),
    Snd(Box<Expression>),
    Hd(Box<Expression>),
    Tl(Box<Expression>),
    Pair(Box<Expression>, Box<Expression>),
    Fn(Variable, Box<Expression>),
    Eq(Box<Expression>, Box<Expression>),
    Cons(Box<Expression>, Box<Expression>),
    And(Box<Expression>, Box<Expression>),
    Add(Box<Expression>, Box<Expression>),
    Apply(Box<Expression>, Box<Expression>),
}

#[derive(Clone, PartialEq, Debug)]
struct Variable {
    ident: String,
}

fn parser(input: &str) -> Result<Expression> {
    let file = MiniMLParser::parse(Rule::file, input)
        .expect("Bad parse")
        .next()
        .unwrap();

    println!("{:?}", file);

    transform_parse_output(file)
}

fn transform_parse_output(input: Pair<Rule>) -> Result<Expression> {
    match input.as_rule() {
        Rule::x => Ok(Expression::Var(Variable {
            ident: input.as_span().as_str().to_string(),
        })),
        Rule::c_bool => match input.as_span().as_str() {
            "true" => Ok(Expression::True),
            "false" => Ok(Expression::False),
            _ => {
                panic!();
            }
        },
        Rule::c_num => Ok(Expression::Num(input.as_span().as_str().parse().unwrap())),
        _ => todo!(),
    }
}

#[test]
fn test_num() {
    assert_eq!(parser("1").unwrap(), Expression::Num(1));
}

#[test]
fn test_add() {
    assert_eq!(parser("1+2").unwrap(), Expression::Num(1));
}
