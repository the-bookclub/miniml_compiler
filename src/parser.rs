use anyhow::Result;
use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "miniml.pest"]
struct MiniMLParser;

#[derive(Clone, PartialEq, Debug)]
pub enum Expression {
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

#[derive(Clone, PartialEq, Debug, Eq, Hash)]
pub struct Variable {
    pub ident: String,
}

#[derive(Clone)]
enum PartialExpressionOperator {
    Apply,
    Add,
    And,
    Cons,
    Equals,
}

pub fn parser(input: &str) -> Result<Expression> {
    let file = MiniMLParser::parse(Rule::file, input)
        .expect("Bad parse")
        .next()
        .unwrap();

    println!("{:?}", file);
    println!("");

    transform_parse_output(file)
}

fn transform_parse_output(input: Pair<Rule>) -> Result<Expression> {
    println!("{:?}", input);
    println!("");
    match input.as_rule() {
        Rule::var_stmt => {
            let mut data = input.into_inner();
            Ok(Expression::Var(transform_variable(data.next().unwrap())?))
        }
        Rule::c_bool => match input.as_span().as_str() {
            "true" => Ok(Expression::True),
            "false" => Ok(Expression::False),
            _ => {
                panic!();
            }
        },
        Rule::c_num => Ok(Expression::Num(input.as_span().as_str().parse().unwrap())),
        Rule::e_zeroth => transform_e_rule(input, PartialExpressionOperator::Apply),
        Rule::e_first => transform_e_rule(input, PartialExpressionOperator::Add),
        Rule::e_second => transform_e_rule(input, PartialExpressionOperator::And),
        Rule::e_third => transform_e_rule(input, PartialExpressionOperator::Cons),
        Rule::e_fourth => transform_e_rule(input, PartialExpressionOperator::Equals),
        Rule::let_stmt => {
            let mut data = input.into_inner();
            let variable = transform_parse_output(data.next().unwrap())?;
            match variable {
                Expression::Var(v) => {
                    let e1 = transform_parse_output(data.next().unwrap())?;
                    let e2 = transform_parse_output(data.next().unwrap())?;
                    Ok(Expression::Let(v, Box::new(e1), Box::new(e2)))
                }
                _ => panic!(),
            }
        }
        Rule::not_stmt => {
            let mut data = input.into_inner();
            let e = transform_parse_output(data.next().unwrap())?;
            Ok(Expression::Not(Box::new(e)))
        }
        Rule::if_stmt => {
            let mut data = input.into_inner();
            let e1 = transform_parse_output(data.next().unwrap())?;
            let e2 = transform_parse_output(data.next().unwrap())?;
            let e3 = transform_parse_output(data.next().unwrap())?;
            Ok(Expression::If(Box::new(e1), Box::new(e2), Box::new(e3)))
        }
        Rule::succ => {
            let mut data = input.into_inner();
            let e = transform_parse_output(data.next().unwrap())?;
            Ok(Expression::Succ(Box::new(e)))
        }
        Rule::pair => {
            let mut data = input.into_inner();
            let e1 = transform_parse_output(data.next().unwrap())?;
            let e2 = transform_parse_output(data.next().unwrap())?;
            Ok(Expression::Pair(Box::new(e1), Box::new(e2)))
        }
        Rule::fst => {
            let mut data = input.into_inner();
            let e = transform_parse_output(data.next().unwrap())?;
            Ok(Expression::Fst(Box::new(e)))
        }
        Rule::snd => {
            let mut data = input.into_inner();
            let e = transform_parse_output(data.next().unwrap())?;
            Ok(Expression::Snd(Box::new(e)))
        }
        Rule::nil => Ok(Expression::Nil),
        Rule::hd => {
            let mut data = input.into_inner();
            let e = transform_parse_output(data.next().unwrap())?;
            Ok(Expression::Hd(Box::new(e)))
        }
        Rule::tl => {
            let mut data = input.into_inner();
            let e = transform_parse_output(data.next().unwrap())?;
            Ok(Expression::Tl(Box::new(e)))
        }
        Rule::pred => {
            let mut data = input.into_inner();
            let e = transform_parse_output(data.next().unwrap())?;
            Ok(Expression::Pred(Box::new(e)))
        }
        Rule::fn_stmt => {
            let mut data = input.into_inner();
            let var = transform_variable(data.next().unwrap())?;
            let e = transform_parse_output(data.next().unwrap())?;
            Ok(Expression::Fn(var, Box::new(e)))
        }
        _ => todo!(),
    }
}

fn transform_variable(input: Pair<Rule>) -> Result<Variable> {
    match input.as_rule() {
        Rule::x => Ok(Variable {
            ident: input.as_span().as_str().to_string(),
        }),
        _ => panic!(),
    }
}

fn transform_e_rule(input: Pair<Rule>, op: PartialExpressionOperator) -> Result<Expression> {
    let mut data = input.into_inner();
    let e_left = data.next().unwrap();
    let e_left_transformed = transform_parse_output(e_left)?;

    let e_right_prime = data.next().unwrap();
    transform_parse_output_partial(e_left_transformed, e_right_prime, op)
}

fn transform_parse_output_partial(
    left: Expression,
    input: Pair<Rule>,
    op: PartialExpressionOperator,
) -> Result<Expression> {
    let mut data = input.into_inner();
    if data.len() == 0 {
        return Ok(left);
    }

    let expression = transform_parse_output(data.next().unwrap())?;

    let left_boxed = Box::new(left);
    let exp_boxed = Box::new(expression);

    let complete_left = match op {
        PartialExpressionOperator::Apply => Expression::Apply(left_boxed, exp_boxed),
        PartialExpressionOperator::Add => Expression::Add(left_boxed, exp_boxed),
        PartialExpressionOperator::And => Expression::And(left_boxed, exp_boxed),
        PartialExpressionOperator::Cons => Expression::Cons(left_boxed, exp_boxed),
        PartialExpressionOperator::Equals => Expression::Eq(left_boxed, exp_boxed),
    };

    transform_parse_output_partial(complete_left, data.next().unwrap(), op.clone())
}

#[test]
fn test_num() {
    assert_eq!(parser("1").unwrap(), Expression::Num(1));
}

#[test]
fn test_multi_num() {
    assert_eq!(parser("123").unwrap(), Expression::Num(123));
}

#[ignore]
#[test]
fn test_add() {
    assert_eq!(
        parser("1 + 2").unwrap(),
        Expression::Add(Box::new(Expression::Num(1)), Box::new(Expression::Num(2)))
    );
}

#[test]
fn test_double_add() {
    assert_eq!(
        parser("1 + 2 + 3").unwrap(),
        Expression::Add(
            Box::new(Expression::Add(
                Box::new(Expression::Num(1)),
                Box::new(Expression::Num(2))
            )),
            Box::new(Expression::Num(3))
        )
    );
}

#[test]
fn test_and() {
    assert_eq!(
        parser("1 and 2").unwrap(),
        Expression::And(Box::new(Expression::Num(1)), Box::new(Expression::Num(2)))
    );
}

#[test]
fn test_double_and() {
    assert_eq!(
        parser("1 and 2 and 3").unwrap(),
        Expression::And(
            Box::new(Expression::And(
                Box::new(Expression::Num(1)),
                Box::new(Expression::Num(2))
            )),
            Box::new(Expression::Num(3))
        )
    );
}

#[test]
fn test_fn() {
    assert_eq!(
        parser("1 ( 2 )").unwrap(),
        Expression::Apply(Box::new(Expression::Num(1)), Box::new(Expression::Num(2)))
    );
}

#[test]
fn test_double_fn() {
    assert_eq!(
        parser("1 ( 2 ) ( 3 )").unwrap(),
        Expression::Apply(
            Box::new(Expression::Apply(
                Box::new(Expression::Num(1)),
                Box::new(Expression::Num(2))
            )),
            Box::new(Expression::Num(3))
        )
    );
}
