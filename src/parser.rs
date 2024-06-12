use pest::iterators::Pairs;
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

// fn parser(input: &str) -> IResult<&str, Expression> {
//     MiniMLParser::parse(Rule::e_top, input)
// }
