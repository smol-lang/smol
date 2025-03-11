%{
open Syntax

let add_type ident = (ident, Type.gen_type ())

let make_lambda params body =
  List.fold_right (fun param acc ->
    Lambda
      { ident = (param, Type.gen_type ())
      ; body = acc
      }
  ) params body

let make_binding recurse ident params body =
  { recurse
  ; ident = add_type ident
  ; body = make_lambda params body
  }
%}

/* definition */
%token LPAREN RPAREN
%token <bool> BOOL
%token <int> INT
%token <Id.t> IDENT
%token NOT PLUS MINUS MUL DIV
%token LOR LAND
%token EQ NEQ LT LEQ GT GEQ
%token LET REC IN FUN ARROW AND
%token IF THEN ELSE
%token SEMICOLON EOF

/* precedence */
%nonassoc PREC_IF
%right SEMICOLON
%nonassoc PREC_LET
%nonassoc ARROW
%left LOR
%left LAND
%left EQ NEQ LT GT LEQ GEQ
%left PLUS MINUS
%left MUL DIV
%right PREC_UNARY_MINUS
%right PREC_UNARY_NOT

%start <Syntax.expr> program
%%

program:
  | expr EOF { $1 }

expr:
  | non_app { $1 }
  | app { $1 }

app:
  | app atomic_expr { App ($1, $2) }
  | atomic_expr atomic_expr { App ($1, $2) }

non_app:
  | atomic_expr { $1 }
  | NOT expr %prec PREC_UNARY_NOT { Not $2 }
  | MINUS expr %prec PREC_UNARY_MINUS { Neg $2 }
  | expr LOR expr { Or ($1, $3) }
  | expr LAND expr { And ($1, $3) }
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
  | LET REC let_and_bindings IN expr %prec PREC_LET
    { Lets 
        { bindings = $3
        ; nest_in = $5
        }
    }
  | LET let_bindings IN expr %prec PREC_LET
    { Lets
        { bindings = $2
        ; nest_in = $4
        }
    }
  | expr SEMICOLON expr
    { Lets
        { bindings = 
            [{ recurse = false
             ; ident = (Id.gen_tmp Type.Unit, Type.Unit)
             ; body = $1
             }]
        ; nest_in = $3
        }
    }
  | FUN IDENT ARROW expr
    { Lambda
        { ident = add_type $2
        ; body = $4
        }
    }
  | IF expr THEN expr ELSE expr %prec PREC_IF
    { If
        { cond = $2
        ; branch_true = $4
        ; branch_false = $6
        }
    }

atomic_expr:
  | LPAREN expr RPAREN { $2 }
  | LPAREN RPAREN { Unit }
  | BOOL { Bool $1 }
  | INT { Int $1 }
  | IDENT { Var $1 }

let_bindings:
  | IDENT params EQ expr { [make_binding false $1 $2 $4] }
  | IDENT params EQ expr AND let_bindings { make_binding false $1 $2 $4 :: $6 }

let_and_bindings:
  | IDENT params EQ expr { [make_binding true $1 $2 $4] }
  | IDENT params EQ expr AND let_and_bindings { make_binding true $1 $2 $4 :: $6 }

params:
  | IDENT params { $1 :: $2 }
  | /* empty */ { [] }
