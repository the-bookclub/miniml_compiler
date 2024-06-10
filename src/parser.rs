use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, alphanumeric0, alphanumeric1, digit1};
use nom::combinator::recognize;
use nom::multi::{many0, many0_count};
use nom::sequence::{pair, tuple};
use nom::IResult;
use nom::Parser;

fn parser(input: &str) -> IResult<&str, &str> {}

fn parse_variable(input: &str) -> IResult<&str, &str> {
    // x = [a-zA-Z_][a-zA-Z0-9]*
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0_count(alt((alphanumeric1, tag("_")))),
    ))
    .parse(input)
}

fn parse_bool(input: &str) -> IResult<&str, &str> {
    alt((tag("true"), tag("false")))(input)
}

fn parse_num(input: &str) -> IResult<&str, &str> {
    digit1(input)
}

fn parse_nil(input: &str) -> IResult<&str, &str> {
    tag("nil")(input)
}

fn parse_let(input: &str) -> IResult<&str, (&str, &str, &str, &str)> {
    tuple((tag("let"), parse_def, tag("in"), parse_e_top))(input)
}

fn parse_def(input: &str) -> IResult<&str, (&str, &str, &str)> {
    tuple((parse_variable, tag("="), parse_e_top))(input)
}

fn parse_not(input: &str) -> IResult<&str, (&str, &str, &str, &str)> {
    tuple((tag("not"), tag("("), parse_e_top, tag(")")))(input)
}

fn parse_if(input: &str) -> IResult<&str, &str> {
    tuple((
        tag("if"),
        parse_e_top,
        tag("then"),
        parse_e_top,
        tag("else"),
        parse_e_top,
    ))(input)
}

fn parse_succ(input: &str) -> IResult<&str, &str> {
    tuple((tag("succ"), tag("("), parse_e_top, tag(")")))(input)
}

fn parse_pair(input: &str) -> IResult<&str, &str> {
    tuple((tag("<"), parse_e_top, tag(","), parse_e_top, tag(">")))(input)
}

fn parse_fst(input: &str) -> IResult<&str, &str> {
    tuple((tag("fst"), tag("("), parse_e_top, tag(")")))(input)
}

fn parse_snd(input: &str) -> IResult<&str, &str> {
    tuple((tag("snd"), tag("("), parse_e_top, tag(")")))(input)
}

fn parse_hd(input: &str) -> IResult<&str, &str> {
    tuple((tag("hd"), tag("("), parse_e_top, tag(")")))(input)
}

fn parse_tl(input: &str) -> IResult<&str, &str> {
    tuple((tag("tl"), tag("("), parse_e_top, tag(")")))(input)
}

fn parse_pred(input: &str) -> IResult<&str, &str> {
    tuple((tag("pred"), tag("("), parse_e_top, tag(")")))(input)
}

fn parse_e_null(input: &str) -> IResult<&str, &str> {
    alt((
        parse_variable,
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

fn parse_fn(input: &str) -> IResult<&str, &str> {
    tuple((tag("fn"), parse_variable, tag("."), parse_e_top))(input)
}

fn parse_e_fifth(input: &str) -> IResult<&str, &str> {
    alt((parse_fn, parse_e_null))(input)
}

fn parse_eq(input: &str) -> IResult<&str, &str> {
    tuple((parse_e_top, tag("="), parse_e_top))(input)
}

fn parse_e_fourth(input: &str) -> IResult<&str, &str> {
    alt((parse_eq, parse_e_fifth))(input)
}

fn parse_cons(input: &str) -> IResult<&str, &str> {
    tuple((parse_e_top, tag("::"), parse_e_top))(input)
}

fn parse_e_third(input: &str) -> IResult<&str, &str> {
    alt((parse_cons, parse_e_fourth))(input)
}

fn parse_and(input: &str) -> IResult<&str, &str> {
    tuple((parse_e_top, tag("and"), parse_e_top))(input)
}

fn parse_e_second(input: &str) -> IResult<&str, &str> {
    alt((parse_and, parse_e_third))(input)
}

fn parse_add(input: &str) -> IResult<&str, &str> {
    tuple((parse_e_top, tag("+"), parse_e_top))(input)
}

fn parse_e_first(input: &str) -> IResult<&str, &str> {
    alt((parse_add, parse_e_second))(input)
}

fn parse_apply_1(input: &str) -> IResult<&str, &str> {
    tuple((parse_e_top, tag("("), parse_e_top, tag(")")))(input)
}

fn parse_apply_2(input: &str) -> IResult<&str, &str> {
    tuple((parse_e_top, parse_e_top))(input)
}

fn parse_e_zeroth(input: &str) -> IResult<&str, &str> {
    alt((parse_apply_1, parse_apply_2, parse_e_first))(input)
}

fn parse_e_top_bracket(input: &str) -> IResult<&str, &str> {
    tuple((tag("("), parse_e_top, tag(")")))(input)
}

fn parse_e_top(input: &str) -> IResult<&str, &str> {
    alt((parse_e_top_bracket, parse_e_zeroth))(input)
}
