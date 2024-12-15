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
 | VAL of int
 | ITE of bexp * aexp * aexp
 | WRITE_EXPR of aexp
 | CALL of string * aexp list
 | ASSIGN of string * int

and bexp =
 | TRUE | FALSE 
 | COMP of bcomp * aexp * aexp 
 | BEXP of bop * bexp * bexp

type decl = 
 | FUNC of string * string list * aexp

type prog = 
 | DEF_SEQ of decl * prog
 | MAIN of aexp
