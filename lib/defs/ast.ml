type delim =
  | NONE

type bcomp = 
 | GT | LT | GE | LE | EQ | NE 

type bop = 
 | CONJ | DISJ | BEQ | BNE

type aop = 
 | SUB
 | ADD
 | MULT
 | DIV
 | MOD

type aexp = 
 | SEQ of aexp * aexp
 | AEXP of aop * aexp * aexp
 | VAR of string
 | INT_VAL of int
 | DBL_VAL of float
 | CHR_VAL of char
 | ITE of bexp * aexp * aexp
 | WRITE_EXPR of aexp
 | CALL of string * aexp list
 | ASSIGN of string * int

and bexp =
 | TRUE | FALSE 
 | COMP of bcomp * aexp * aexp 
 | BEXP of bop * bexp * bexp

type decl = 
 | CONST_INT of string * int
 | CONST_DBL of string * float
 | FUNC of string * (string * string) list * aexp * string
 | MAIN of aexp

type prog = 
 | DEF_SEQ of decl * prog
 | MAIN of aexp
