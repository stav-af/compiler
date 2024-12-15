open Defs.Regex
open Defs.Ast

let indent_string indent = String.make indent ' '

let rec pretty_delim = function
  | NONE -> ""

and pretty_bcomp = function
  | GT -> ">"
  | LT -> "<"
  | GE -> ">="
  | LE -> "<="
  | EQ -> "=="
  | NE -> "!="

and pretty_bop = function
  | CONJ -> "&&"
  | DISJ -> "||"
  | BEQ -> "=="
  | BNE -> "!="

and pretty_aop = function
  | ADD -> "+"
  | SUB -> "-"
  | MULT -> "*"
  | DIV -> "/"
  | MOD -> "%"

and pretty_aexp indent = function
  | SEQ (a1, a2) ->
      Printf.sprintf "%sSEQ(\n%s,\n%s\n%s)" 
        (indent_string indent)
        (pretty_aexp (indent + 1) a1)
        (pretty_aexp (indent + 1) a2)
        (indent_string indent)
  | AEXP (op, a1, a2) ->
      Printf.sprintf "%sAEXP(%s, %s, %s)"
        (indent_string indent)
        (pretty_aop op)
        (pretty_aexp (indent + 1) a1)
        (pretty_aexp (indent + 1) a2)
  | VAR name -> Printf.sprintf "VAR(%s)" name
  | CHR_VAL value -> Printf.sprintf "VAL(%c)" value
  | INT_VAL value -> Printf.sprintf "VAL(%n)" value
  | DBL_VAL value -> Printf.sprintf "VAL(%f)" value
  | ITE (cond, a1, a2) ->
      Printf.sprintf "%sITE(\n%s,\n%s,\n%s\n%s)"
        (indent_string indent)
        (pretty_bexp (indent + 1) cond)
        (pretty_aexp (indent + 1) a1)
        (pretty_aexp (indent + 1) a2)
        (indent_string indent)
  | WRITE_EXPR expr ->
      Printf.sprintf "%sWRITE_EXPR(\n%s\n%s)"
        (indent_string indent)
        (pretty_aexp (indent + 1) expr)
        (indent_string indent)
  | CALL (name, args) ->
      Printf.sprintf "%sCALL(%s, [%s])"
        (indent_string indent)
        name
        (String.concat "; " (List.map (pretty_aexp 0) args))
  | ASSIGN (var, value) ->
      Printf.sprintf "%sASSIGN(%s, %d)"
        (indent_string indent)
        var value

and pretty_bexp indent = function
  | TRUE -> Printf.sprintf "%sTRUE" (indent_string indent)
  | FALSE -> Printf.sprintf "%sFALSE" (indent_string indent)
  | COMP (comp, a1, a2) ->
      Printf.sprintf "%sCOMP(%s, %s, %s)"
        (indent_string indent)
        (pretty_bcomp comp)
        (pretty_aexp 0 a1)
        (pretty_aexp 0 a2)
  | BEXP (op, b1, b2) ->
      Printf.sprintf "%sBEXP(%s, %s, %s)"
        (indent_string indent)
        (pretty_bop op)
        (pretty_bexp 0 b1)
        (pretty_bexp 0 b2)

let rec pretty_decl indent = function
  | FUNC (name, args, body, t) ->
      let args_str =
        args
        |> List.map (fun (arg_name, arg_type) -> Printf.sprintf "%s: %s" arg_name arg_type)
        |> String.concat ", "
      in
      Printf.sprintf "%sFUNC(%s, [%s] -> %s,\n%s\n%s)"
        (indent_string indent)
        name
        args_str
        t
        (pretty_aexp (indent + 1) body)
        (indent_string indent)
  | _ -> failwith "NOT DEFINED"

and pretty_prog indent = function
  | DEF_SEQ (decl, prog) ->
      Printf.sprintf "%sDEF_SEQ(\n%s,\n%s\n%s)"
        (indent_string indent)
        (pretty_decl (indent + 1) decl)
        (pretty_prog (indent + 1) prog)
        (indent_string indent)
  | MAIN expr ->
      Printf.sprintf "%sMAIN(\n%s\n%s)"
        (indent_string indent)
        (pretty_aexp (indent + 1) expr)
        (indent_string indent)

let pretty_print ast = pretty_prog 0 ast


let rec display_value v =
  match v with
  | StarVal(_) -> "StarValue()"
  | Empty -> "Empty"
  | Literal c -> "Literal('" ^ String.make 1 c ^ "')"
  | VSeq (v1, v2) -> "VSeq(" ^ display_value v1 ^ ", " ^ display_value v2 ^ ")"
  | Left v -> "Left(" ^ display_value v ^ ")"
  | Right v -> "Right(" ^ display_value v ^ ")"
  | Rec (s, v) -> Printf.sprintf "Record(%s: %s)" s (display_value v)
  | NTVal (v, n) -> Printf.sprintf "(%s){%d}" (display_value v) n

let rec display_regex r =
  match r with
  | Zero -> "0"
  | One -> "1"
  | Char c -> Printf.sprintf "%c" c
  | Any cset -> Printf.sprintf "Any{%s}"  (String.concat "," (CharSet.fold(fun c acc -> (String.make 1 c) :: acc) cset []))
  | Seq (r1, r2) -> 
      Printf.sprintf "%s%s" (display_regex r1) (display_regex r2)
  | Alt (r1, r2) -> 
      Printf.sprintf "(%s | %s)" (display_regex r1) (display_regex r2)
  | Star r -> Printf.sprintf "%s*" (display_regex r)
  | Recd (s, r) -> Printf.sprintf "(%s: %s)" s (display_regex r) 
  | NTimes (r, n) -> Printf.sprintf "(%s:{%d})" (display_regex r) n
  | Plus(r1) -> Printf.sprintf "%s+" (display_regex r1)
  | Opt(r1) -> Printf.sprintf "%s?" (display_regex r1)
  | CFun(_) -> Printf.sprintf "f"

let display_toks toks = 
  let toks_str = 
    List.fold_left (fun acc (label, value) ->
      acc ^ Printf.sprintf " (%s, %s), " label value
    ) "" toks
  in
  Printf.sprintf "[\n%s]\n" toks_str
  

let display_env ev = 
  List.iter (fun (x, s) -> Printf.printf "Found submatch (%s: %s)" x s) ev

let c_str (cs: char list) = 
  String.concat "" (List.map (String.make 1) cs)

let s_str (ss: string list) =
  String.concat "" ss

let b_str(b: bool) = 
  if b then "True" else "False"