open Defs.Ast
open Kast

let label_counter = ref 0
let fresh s =
  incr label_counter;
  Printf.sprintf "%s%n" s !label_counter

let prelude =
{|
@.str = private constant [4 x i8] c"%d\0A\00"

declare i32 @printf(i8*, ...)

define i32 @printInt(i32 %x) {
  %t0 = getelementptr [4 x i8], [4 x i8]* @.str, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %t0, i32 %x)
  ret i32 %x
}
|}


(*
 -- Format --
  Branching:
    br <type> %var, label %<true_jmp>, label %<false_jmp>

  Comparison
    %<varname> = icmp <cmptype> <type> %var1, %var2

  Assignment
    %<varname> = <operator> <type> <var/val>, <var/val>

  Return
    ret <type> <var/val>
*)

let s_of_aop = function 
 | SUB -> "sub i32"
 | ADD -> "add i32"
 | MULT -> "mul i32"
 | DIV -> "udiv i32"
 | MOD -> "urem i32"

let s_of_bop = function
 | GT -> "icmp sgt i32" 
 | LT -> "icmp slt i32"
 | GE -> "icmp sge i32"
 | LE -> "icmp sle i32"
 | EQ -> "icmp eq i32"
 | NE -> "icmp ne i32"

let s_of_args aargs =
  let rec aux _args acc = 
    match _args with
    | [] -> ""
    | a::[] -> acc ^ Printf.sprintf "i32 %s" a 
    | a::rest -> 
        let curr = acc ^ Printf.sprintf "i32 %s, " a in
        aux rest curr
    in
  aux aargs ""

let s_of_decl_args aargs =
  let rec aux _args acc = 
    match _args with
    | [] -> ""
    | (a, _)::[] -> acc ^ Printf.sprintf "i32 %%%s" a 
    | (a, _)::rest -> 
        let curr = acc ^ Printf.sprintf "i32 %%%s, " a in
        aux rest curr
    in
  aux aargs ""

let rec cps e k =
 match e with 
 | VAR(s) -> k(KVar(s, UNDEF))
 (* | VAL(i) -> k(KNum i) *)
 | AEXP(op, e1, e2) ->
  let label = (fresh "tmp") in
  let sop = (s_of_aop op) in
  (cps e1 (fun lhs ->
    cps e2 (fun rhs -> 
      KLet(label, KOp(sop, lhs, rhs), k(KVar(label, UNDEF))))))
 | ITE(COMP(op, ce1, ce2), e1, e2) ->
  let label = fresh "tmp" in
  let sop = s_of_bop op in 
  (cps ce1 (fun cv1 -> 
    cps ce2 (fun cv2 -> 
      KLet(label, KOp(sop, cv1, cv2),
        KIf(label, cps e1 k, cps e2 k)))))
 | SEQ(e1, e2) ->
  (cps e1 (fun _ -> 
    cps e2 (fun y -> k(y))))
 | CALL(fname, args) ->
  let rec aux aargs vs = 
    (match aargs with 
    | [] -> 
      let label = fresh "tmp" in
      KLet(label, KCall(fname, vs), k(KVar(label, UNDEF)))
    | a::xa -> (cps a (fun y -> aux xa (vs@[y])))) in
  aux args []
 | WRITE_EXPR(ex) ->
    let label = fresh "tmp" in
    (cps ex (fun y -> KLet(label, KWrite(y), k(KVar(label, UNDEF)))))
 | _ -> failwith "not implemented"

let cpsi e = (cps e (fun x -> KReturn(x)))

let rec compile_kval kval = 
 match kval with
 | KVar(name, _) -> 
  Printf.sprintf "%%%s" name
 | KNum(n) ->  
  Printf.sprintf "%n" n
 | KOp(op, var1, var2) ->
  Printf.sprintf "%s %s, %s" op (compile_kval var1) (compile_kval var2)
 | KCall(fname, args) ->
  Printf.sprintf "call i32 @%s(%s)" fname (s_of_args (List.map (compile_kval) args))
 | KWrite(kexp) -> 
  Printf.sprintf "call i32 @printInt(i32 %s)" (compile_kval kexp)

let rec compile_kexp kexp = 
 match kexp with 
 | KIf(varname, exp1, exp2) ->
  let if_branch = fresh "if_branch" in
  let else_branch = fresh "else_branch" in
  let condition = Printf.sprintf "  br i1 %%%s, label %%%s, label %%%s\n" varname if_branch else_branch in
  condition ^
  "\n" ^ if_branch ^ ":\n" ^
  compile_kexp(exp1) ^
  "\n" ^ else_branch ^ ":\n" ^
  compile_kexp(exp2)
 | KLet(name, expr, next) ->
  (Printf.sprintf "  %%%s = %s\n" name (compile_kval expr)) ^
  compile_kexp next
 | KReturn(exp) ->
  Printf.sprintf "  ret i32 %s\n" (compile_kval exp)


let rec compile_prog p =
  match p with 
 | DEF_SEQ(FUNC(name, args, exp, _), prog) ->
    let signiature = Printf.sprintf "\ndefine i32 @%s(%s) {\n" name (s_of_decl_args args) in
    signiature ^
    compile_kexp(cpsi exp)
    ^ "}\n" ^ 
    compile_prog prog
 | MAIN(exp) ->
  "\ndefine i32 @main() {\n" ^
  compile_kexp (cps exp (fun _ -> KReturn(KNum(0)))) ^
  "}\n"
  | _ -> failwith "Not Implemented DEF COMPILATION"

let compile prog = 
  prelude ^
  compile_prog prog

