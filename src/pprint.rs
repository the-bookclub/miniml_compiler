//! pprint.rs: Pretty print Expressions!

use crate::exprs;
use crate::parser;

use exprs::expr_needs_paren;
use parser::Expression;
use parser::Expression::*;

/// Pretty print an expression into human-readable MiniML.
pub fn pprint(e: &Expression) -> String {
    match e {
        True => "True".to_string(),
        False => "False".to_string(),
        Num(n) => n.to_string(),
        Var(v) => v.clone().ident,
        Nil => "nil".to_string(),
        Let(var, bound_expr, body) => format!(
            "let {} = {} in {}",
            var.ident,
            pprint(bound_expr),
            pprint_parenthesize(body)
        ),
        Not(e) => pprint_single_arity_call("not", e),
        If(cond, yes, no) => format!(
            "if {} then {} else {}",
            pprint_parenthesize(cond),
            pprint_parenthesize(yes),
            pprint(no)
        ),
        Succ(e) => pprint_single_arity_call("succ", e),
        Pred(e) => pprint_single_arity_call("pred", e),
        Fst(e) => pprint_single_arity_call("fst", e),
        Snd(e) => pprint_single_arity_call("snd", e),
        Hd(e) => pprint_single_arity_call("hd", e),
        Tl(e) => pprint_single_arity_call("tl", e),
        Pair(e1, e2) => format!("<{}, {}>", pprint(e1), pprint(e2)),
        Fn(v, e) => format!("fn {}. {}", v.ident, pprint(e)),
        Eq(e1, e2) => format!("{} == {}", pprint_parenthesize(e1), pprint_parenthesize(e2)),
        Cons(e1, e2) => format!("{} :: {}", pprint_parenthesize(e1), pprint_parenthesize(e2)),
        And(e1, e2) => format!("{} && {}", pprint_parenthesize(e1), pprint_parenthesize(e2)),
        Add(e1, e2) => format!("{} + {}", pprint_parenthesize(e1), pprint_parenthesize(e2)),
        Apply(e1, e2) => format!("{} {}", pprint_parenthesize(e1), pprint_parenthesize(e2)),
    }
}

/// Pretty print an expression, mayber adding parentheses if needed.
fn pprint_parenthesize(e: &Expression) -> String {
    if expr_needs_paren(e) {
        format!("({})", pprint(e))
    } else {
        pprint(e)
    }
}

/// Utility function to print a call, to avoid duplicating code in pprint.
fn pprint_single_arity_call(s: &str, e: &Expression) -> String {
    if expr_needs_paren(e) {
        format!("{}({})", s, pprint(e))
    } else {
        format!("{} {}", s, pprint(e))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::exprs;
    use exprs::*;

    #[test]
    fn test_basic_pprint() {
        let expr = bAdd(bNum(3), bNum(5));
        assert_eq!(pprint(&*expr), "3 + 5");

        let expr = bLet(
            bVariable("x"),
            bSucc(bAdd(bNum(3), bVar("y"))),
            bPair(bEq(bVar("x"), bNum(5)), bTl(bCons(bFalse(), bNil()))),
        );
        assert_eq!(
            pprint(&*expr),
            "let x = succ(3 + y) in <x == 5, tl(False :: nil)>"
        );

        let expr = bFn("a", bFn("b", bAdd(bSucc(bVar("a")), bSucc(bVar("b")))));
        assert_eq!(pprint(&*expr), "fn a. fn b. (succ a) + (succ b)");
    }
}
