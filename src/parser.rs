use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, alphanumeric1, digit1};
use nom::combinator::{recognize, value};
use nom::multi::many0_count;
use nom::sequence::pair;
use nom::IResult;
use nom::Parser;

#[derive(Clone, PartialEq, Debug)]
pub enum Expression {
    True,
    False,
    Num(u32),
    Var(Variable),
    Nil,
    Let(Definition, Box<Expression>),
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

#[derive(Clone, PartialEq, Debug)]
pub struct Definition {
    pub name: Variable,
    pub value: Box<Expression>,
}

fn parser(input: &str) -> IResult<&str, Expression> {
    parse_e_top(input)
}

fn parse_variable(input: &str) -> IResult<&str, Variable> {
    // x = [a-zA-Z_][a-zA-Z0-9]*
    let (remainder, s) = recognize(pair(
        alt((alpha1, tag("_"))),
        many0_count(alt((alphanumeric1, tag("_")))),
    ))
    .parse(input)?;
    let v = Variable {
        ident: s.to_string(),
    };
    Ok((remainder, v))
}

fn parse_e_variable(input: &str) -> IResult<&str, Expression> {
    let (remainder, v) = parse_variable(input)?;
    let e = Expression::Var(v);
    Ok((remainder, e))
}

fn parse_bool(input: &str) -> IResult<&str, Expression> {
    alt((
        value(Expression::True, tag("true")),
        value(Expression::False, tag("false")),
    ))(input)
}

fn parse_num(input: &str) -> IResult<&str, Expression> {
    let (remainder, num) = digit1(input)?;
    let n: u32 = num.parse().unwrap();
    let e = Expression::Num(n);
    Ok((remainder, e))
}

fn parse_nil(input: &str) -> IResult<&str, Expression> {
    value(Expression::Nil, tag("nil"))(input)
}

fn parse_let(input: &str) -> IResult<&str, Expression> {
    let (remainder, _) = tag("let")(input)?;
    let (remainder, def) = parse_def(remainder)?;
    let (remainder, _) = tag("in")(remainder)?;
    let (remainder, e) = parse_e_top(remainder)?;
    let l = Expression::Let(def, Box::new(e));
    Ok((remainder, l))
}

fn parse_def(input: &str) -> IResult<&str, Definition> {
    let (remainder, var) = parse_variable(input)?;
    let (remainder, _) = tag("=")(remainder)?;
    let (remainder, e) = parse_e_top(remainder)?;
    let def = Definition {
        name: var,
        value: Box::new(e),
    };
    Ok((remainder, def))
}

fn parse_not(input: &str) -> IResult<&str, Expression> {
    let (remainder, _) = tag("not")(input)?;
    let (remainder, _) = tag("(")(remainder)?;
    let (remainder, e) = parse_e_top(remainder)?;
    let (remainder, _) = tag(")")(remainder)?;
    let n = Expression::Not(Box::new(e));
    Ok((remainder, n))
}

fn parse_if(input: &str) -> IResult<&str, Expression> {
    let (remainder, _) = tag("if")(input)?;
    let (remainder, cond) = parse_e_top(remainder)?;
    let (remainder, _) = tag("then")(remainder)?;
    let (remainder, e_true) = parse_e_top(remainder)?;
    let (remainder, _) = tag("else")(remainder)?;
    let (remainder, e_false) = parse_e_top(remainder)?;
    let i = Expression::If(Box::new(cond), Box::new(e_true), Box::new(e_false));
    Ok((remainder, i))
}

fn parse_succ(input: &str) -> IResult<&str, Expression> {
    let (remainder, _) = tag("succ")(input)?;
    let (remainder, _) = tag("(")(remainder)?;
    let (remainder, e) = parse_e_top(remainder)?;
    let (remainder, _) = tag(")")(remainder)?;
    let s = Expression::Succ(Box::new(e));
    Ok((remainder, s))
}

fn parse_pair(input: &str) -> IResult<&str, Expression> {
    let (remainder, _) = tag("<")(input)?;
    let (remainder, e1) = parse_e_top(remainder)?;
    let (remainder, _) = tag(",")(remainder)?;
    let (remainder, e2) = parse_e_top(remainder)?;
    let (remainder, _) = tag(")")(remainder)?;
    let p = Expression::Pair(Box::new(e1), Box::new(e2));
    Ok((remainder, p))
}

fn parse_fst(input: &str) -> IResult<&str, Expression> {
    let (remainder, _) = tag("fst")(input)?;
    let (remainder, _) = tag("(")(remainder)?;
    let (remainder, e) = parse_e_top(remainder)?;
    let (remainder, _) = tag(")")(remainder)?;
    let f = Expression::Fst(Box::new(e));
    Ok((remainder, f))
}

fn parse_snd(input: &str) -> IResult<&str, Expression> {
    let (remainder, _) = tag("snd")(input)?;
    let (remainder, _) = tag("(")(remainder)?;
    let (remainder, e) = parse_e_top(remainder)?;
    let (remainder, _) = tag(")")(remainder)?;
    let f = Expression::Snd(Box::new(e));
    Ok((remainder, f))
}

fn parse_hd(input: &str) -> IResult<&str, Expression> {
    let (remainder, _) = tag("hd")(input)?;
    let (remainder, _) = tag("(")(remainder)?;
    let (remainder, e) = parse_e_top(remainder)?;
    let (remainder, _) = tag(")")(remainder)?;
    let h = Expression::Hd(Box::new(e));
    Ok((remainder, h))
}

fn parse_tl(input: &str) -> IResult<&str, Expression> {
    let (remainder, _) = tag("tl")(input)?;
    let (remainder, _) = tag("(")(remainder)?;
    let (remainder, e) = parse_e_top(remainder)?;
    let (remainder, _) = tag(")")(remainder)?;
    let t = Expression::Tl(Box::new(e));
    Ok((remainder, t))
}

fn parse_pred(input: &str) -> IResult<&str, Expression> {
    let (remainder, _) = tag("pred")(input)?;
    let (remainder, _) = tag("(")(remainder)?;
    let (remainder, e) = parse_e_top(remainder)?;
    let (remainder, _) = tag(")")(remainder)?;
    let p = Expression::Succ(Box::new(e));
    Ok((remainder, p))
}

fn parse_e_null(input: &str) -> IResult<&str, Expression> {
    alt((
        parse_e_variable,
        parse_bool,
        parse_num,
        parse_let,
        parse_not,
        parse_if,
        parse_succ,
        parse_pair,
        parse_fst,
        parse_snd,
        parse_nil,
        parse_hd,
        parse_tl,
        parse_pred,
    ))(input)
}

fn parse_fn(input: &str) -> IResult<&str, Expression> {
    let (remainder, _) = tag("fn")(input)?;
    let (remainder, v) = parse_variable(remainder)?;
    let (remainder, _) = tag(".")(remainder)?;
    let (remainder, e) = parse_e_top(remainder)?;
    let f = Expression::Fn(v, Box::new(e));
    Ok((remainder, f))
}

fn parse_e_fifth(input: &str) -> IResult<&str, Expression> {
    alt((parse_fn, parse_e_null))(input)
}

fn parse_eq(input: &str) -> IResult<&str, Expression> {
    let (remainder, e1) = parse_e_top(input)?;
    let (remainder, _) = tag("==")(remainder)?;
    let (remainder, e2) = parse_e_top(remainder)?;
    let eq = Expression::Eq(Box::new(e1), Box::new(e2));
    Ok((remainder, eq))
}

fn parse_e_fourth(input: &str) -> IResult<&str, Expression> {
    alt((parse_eq, parse_e_fifth))(input)
}

fn parse_cons(input: &str) -> IResult<&str, Expression> {
    let (remainder, e1) = parse_e_top(input)?;
    let (remainder, _) = tag("::")(remainder)?;
    let (remainder, e2) = parse_e_top(remainder)?;
    let eq = Expression::Cons(Box::new(e1), Box::new(e2));
    Ok((remainder, eq))
}

fn parse_e_third(input: &str) -> IResult<&str, Expression> {
    alt((parse_cons, parse_e_fourth))(input)
}

fn parse_and(input: &str) -> IResult<&str, Expression> {
    let (remainder, e1) = parse_e_top(input)?;
    let (remainder, _) = tag("and")(remainder)?;
    let (remainder, e2) = parse_e_top(remainder)?;
    let eq = Expression::And(Box::new(e1), Box::new(e2));
    Ok((remainder, eq))
}

fn parse_e_second(input: &str) -> IResult<&str, Expression> {
    alt((parse_and, parse_e_third))(input)
}

fn parse_add(input: &str) -> IResult<&str, Expression> {
    let (remainder, e1) = parse_e_top(input)?;
    let (remainder, _) = tag("+")(remainder)?;
    let (remainder, e2) = parse_e_top(remainder)?;
    let eq = Expression::Add(Box::new(e1), Box::new(e2));
    Ok((remainder, eq))
}

fn parse_e_first(input: &str) -> IResult<&str, Expression> {
    alt((parse_add, parse_e_second))(input)
}

fn parse_apply_1(input: &str) -> IResult<&str, Expression> {
    let (remainder, e1) = parse_e_top(input)?;
    let (remainder, _) = tag("(")(remainder)?;
    let (remainder, e2) = parse_e_top(remainder)?;
    let (remainder, _) = tag(")")(remainder)?;
    let a = Expression::Apply(Box::new(e1), Box::new(e2));
    Ok((remainder, a))
}

fn parse_apply_2(input: &str) -> IResult<&str, Expression> {
    let (remainder, e1) = parse_e_top(input)?;
    let (remainder, e2) = parse_e_top(remainder)?;
    let a = Expression::Apply(Box::new(e1), Box::new(e2));
    Ok((remainder, a))
}

fn parse_e_zeroth(input: &str) -> IResult<&str, Expression> {
    alt((parse_apply_1, parse_apply_2, parse_e_first))(input)
}

fn parse_e_top_bracket(input: &str) -> IResult<&str, Expression> {
    let (remainder, _) = tag("(")(input)?;
    let (remainder, e) = parse_e_top(remainder)?;
    let (remainder, _) = tag(")")(remainder)?;
    Ok((remainder, e))
}

fn parse_e_top(input: &str) -> IResult<&str, Expression> {
    alt((parse_e_top_bracket, parse_e_zeroth))(input)
}

#[test]
fn test_num() {
    assert_eq!(parse_num("1"), Ok(("", Expression::Num(1))));
}

#[ignore]
#[test]
fn test_add() {
    assert_eq!(
        parse_add("1+2"),
        Ok((
            "",
            Expression::Add(Box::new(Expression::Num(1)), Box::new(Expression::Num(2)))
        ))
    );
}
