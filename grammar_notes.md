
%{ 
// Auxililary declarations (in C)    


%}

// YACC declarations section

%token EOF
// %token SEMISEMI
%token LET
%token EQUAL
%token MINUS
%token PLUS
%token TIMES
%token LESS
%token IF
%token THEN
%token ELSE
%token FUN
%token LPAREN
%token COLON
%token RPAREN
%token IS
%token TRUE
%token FALSE
%token TINT
%token TBOOL
%token TARROW

%token SEMICOLON
%token DIGIT
%token ALPHA

%%

// Rules section

file = EOF // produces []
     | (e: expr) EOF // produces [Expr e]
     | (e: expr) SEMISMI (lst: file) // produces Expr e :: lst
     | (ds: nonempty_list(def)) SEMISEMI (lst: file) // produces ds @ lst
     | (ds: nonempty_list(def)) EOF // produces ds

toplevel = (d: def) SEMISEMI // produces d
         | (e: expr) SEMISEMI // produces Expr e

def = LET (x: VAR) EQUAL (e: expr) // produces Def (x, e)

expr = mark_position(plain_expr)

plain_expr = (e: plain_app_expr) // Produces e
           | MINUS (n: INT) // Produces Int (-n)
           | (e1: expr) PLUS (e2: expr) // Produces Plus(e1, e2)
           | (e1: expr) MINUS (e2: expr) // Produces Minus(e1, e2)
           | (e1: expr) TIMES (e2: expr) // Produces Times(e1, e2)
           | (e1: expr) EQUAL (e2: expr) // Produces Equal(e1, e2)
           | (e1: expr) LESS (e2: expr) // Produces Less(e1, e2)
           | IF (e1: expr) THEN (e2: expr) ELSE (e3: expr) // Produces If (e1, e2, e3)
           | FUN (x: VAR) LPAREN (f: VAR) COLON (t1: ty) RPAREN COLON (t2: y) IS (e: expr) // Produces Fun(x, f, t1, t2, e)
        
app_expr =  mark_position(plain_app_expr)

plain_app_expr = (e: plain_simple_expr) // Produces e
               | (e1: app_expr) (e2: simple_expr) // Produces Apply(e1, e2)

simple_expr = mark_position(plain_simpl_expr)

plain_simple_expr = (x: VAR) // Produces Var x
                  | TRUE // Produces Bool true
                  | FALSE // Produces Bool false
                  | (n: INT) // Produces Int n
                  | LPAREN (e: plain_expr) RPAREN // Produces e

ty = TBOOL // Produces TBool
   | TINT // Produces TInt
   | (t1: ty) TARROW (t2: ty) // Produces TArrow(t1, t2)
   | LPAREN (t: ty) RPAREN // Produces t

SEMISEMI = SEMICOLON SEMICOLON
INT = DIGIT+

%%

// Auxiliary functions

yyerror(char const *s) {
    printf("yyerror %s", s);
}

yylex() {
    char c;
    c = getchar();

    // Single Character Tokens
    //token EQUAL
    if (c == '=') {
        return EQUAL;
    }
    //token MINUS
    if (c == '-') {
        return MINUS;
    }
    //token PLUS
    if (c == '+') {
        return PLUS;
    }
    //token TIMES
    if (c == '*') {
        return TIMES;
    }
    //token LESS
    if (c == '<') {
        return LESS;
    }
    //token LPAREN
    if (c == '(') {
        return LPAREN;
    }
    //token COLON
    if (c == ':') {
        return COLON;
    }
    //token RPAREN
    if (c == ')') {
        return RPAREN;
    }
    //token EOF
    if (c == '\0') {
        return EOF;
    }
    //token SEMICOLON
    if (c == ';') {
        return SEMICOLON;
    }

    //token LET
    //token IF
    //token THEN
    //token ELSE
    //token FUN
    //token IS
    //token TRUE
    //token FALSE
    //token TINT
    //token TBOOL
    //token TARROW

    //token DIGIT
    if(isdigit(c)) {
        yylval = c - '0';
        return DIGIT;
    }

    //token ALPHA
    if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) {
        yylval = c;
        return ALPHA;
    }

    // Skip whitespace or new lines
    if (c == ' ' || c == '\n' || c == '\r') {
        yylex();
    }


    return c;
}

main() {
    yyparse();
    return 1;
}
