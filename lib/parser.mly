%{
open Syntax

let add_type ident = (ident, Type.gen_type ())

let make_lambda params body =
  List.fold_right (fun param acc -> Lambda ((param, Type.gen_type ()), acc)) params body
%}

/* definition */
%token LPAREN RPAREN
%token <bool> BOOL
%token <int> INT
%token <Id.t> IDENT
%token NOT PLUS MINUS MUL DIV
%token EQ NEQ LT LEQ GT GEQ
%token LET IN FUN ARROW
%token SEMICOLON EOF
%token PREC_LET
%token PREC_UNARY_MINUS
%token PREC_UNARY_NOT

/* precedence */
%right SEMICOLON
%left EQ NEQ LT GT LEQ GEQ
%left PLUS MINUS
%left MUL DIV
%right PREC_LET
%right PREC_UNARY_MINUS
%right PREC_UNARY_NOT

%start <Syntax.expr> program
%%

program:
  | expr EOF { $1 }

expr:
  | simple_expr { $1 }
  | NOT expr { Not $2 } %prec PREC_UNARY_NOT
  | MINUS expr { Neg $2 } %prec PREC_UNARY_MINUS
  | expr PLUS expr { Add ($1, $3) }
  | expr MINUS expr { Sub ($1, $3) }
  | expr MUL expr { Mul ($1, $3) }
  | expr DIV expr { Div ($1, $3) }
  | expr EQ expr { Eq ($1, $3) }
  | expr NEQ expr { Not (Eq ($1, $3)) }
  | expr LEQ expr { Leq ($1, $3) }
  | expr LT expr { Not (Leq ($3, $1)) }
  | expr GEQ expr { Leq ($3, $1) }
  | expr GT expr { Not (Leq ($1, $3)) }
  | LET IDENT params EQ expr IN expr { Let (add_type $2, make_lambda $3 $5, $7) } %prec PREC_LET
  | expr SEMICOLON expr { Let ((Id.gen_tmp Type.Unit, Type.Unit), $1, $3) }

simple_expr:
  | simple_expr atomic_expr { App ($1, $2) }
  | atomic_expr { $1 }

atomic_expr:
  | FUN IDENT ARROW expr { Lambda (add_type $2, $4) }
  | LPAREN expr RPAREN { $2 }
  | LPAREN RPAREN { Unit }
  | BOOL { Bool $1 }
  | INT { Int $1 }
  | IDENT { Var $1 }

params:
  | IDENT params { $1 :: $2 }
  | /* empty */ { [] }
