open Ast

type tokens = (string * string) list
type 'a parser = tokens -> ('a * tokens) list

let map_parser (p: 'a parser) (f: 'a -> 'b) : 'b parser =
  fun inp -> 
    let res = p inp in
    List.map (fun (matched, unmatched) -> (f matched, unmatched)) res

let seq_parser (p: 'a parser) (q: 'b parser) : ('a * 'b) parser =
  fun inp ->
    let res_p = p inp in
    List.concat (
      List.map (fun (p_parsed, p_unparsed) ->
        let res_q = q p_unparsed in  (* Move this line inside the first mapping function *)
        List.map (fun (q_parsed, pq_unparsed) ->
          ((p_parsed, q_parsed), pq_unparsed)
        ) res_q
      ) res_p
    )
  

let alt_parser (p: 'a parser) (q: 'a parser) : 'a parser = 
  fun inp ->
    let res_p = p inp in
    let res_q = q inp in
    res_p @ res_q 


let create_parser (map : (string * 'a) list) : 'a parser = 
  fun toks ->
    match toks with
    | [] -> []
    | (_, tok_value) :: rest ->
        match List.assoc_opt tok_value map with
        | Some value -> [ (value, rest) ]
        | None -> []

  
let (++) = seq_parser
let (|~|) = alt_parser
let (>>=) = map_parser
let (!!) = fun tok value -> create_parser [(tok, value)]


let rec list_parser (p: 'a parser) (q: 'b parser) : 'a list parser = 
  fun inp ->
    let not_last = seq_parser p q in
    ((not_last ++ (list_parser p q) >>= fun ((x, _), xs) -> x::xs) 
    |~| (p >>= fun x -> [x])) inp


let _sc = !! ";" NONE
let _do = !! "do" NONE
let _skip = !! "skip" NONE
let _write = !! "write" NONE
let _while = !! "while" NONE
let _if = !! "if" NONE
let _then = !! "then" NONE
let _else = !! "else" NONE
let _assign = !! "=" NONE
let _lp = !! "(" NONE
let _rp = !! ")" NONE
let _lb = !! "{" NONE
let _rb = !! "}" NONE
let _comma = !! "," NONE
let _define = !! "def" NONE

let _add = (!! "+" ADD) |~| (!! "-" SUB)
let _mult = (!! "/" DIV) |~| (!! "%" MOD) |~| (!! "*" MULT)

let _bool = (!! "true" TRUE) |~| (!! "false" FALSE)
let _bcomp = 
  (!! "<=" LE) |~| 
  (!! ">=" GE) |~| 
  (!! "==" EQ) |~| 
  (!! "!=" NE) |~| 
  (!! ">" GT) |~| 
  (!! "<" LT)
let _bop =
  (!! "==" BEQ)
  |~| (!! "!=" BNE)
  |~| (!! "&&" CONJ)
  |~| (!! "||" DISJ)


let _id: string parser = 
  fun toks -> 
    match toks with 
    | ("i", name) :: rest -> [ (name, rest) ]
    | _ -> []

let _n: aexp parser =
  fun toks ->
    match toks with
    | ("n", num) :: rest -> ([ (VAL (int_of_string num), rest) ])
    | _ -> [] 


let rec _bexp: bexp parser = fun inp -> 
  ((_bterm |~| 
  ((_bterm ++ _bop ++ _bexp) >>= fun ((a, b), c) -> BEXP(b, a, c))) |~|
  ((_lp ++ _bexp ++ _rp) >>= fun ((_, b), _) -> b)) inp

and _bterm: bexp parser = fun inp ->
  ((_bool) |~|
  ((_aexp ++ _bcomp ++ _aexp) >>= fun ((a, b), c) -> COMP(b, a, c)) |~|
  ((_lp ++ _bexp ++ _rp) >>= fun ((_, b), _) -> b)) inp



  
and _aexp: aexp parser = fun inp ->
  (
    ((_if ++ _bexp ++ _then ++ _aexp ++ _else ++ _aexp)
      >>= fun (((((_, be), _), ib), _), tb) -> ITE(be, ib, tb)) |~|
    ((_acmd ++ _sc ++ _aexp) >>= fun ((e1, _), e2) -> SEQ(e1, e2)) |~| 
    _aite
  ) inp

and _acmd = fun inp -> 
  (
    ((_write ++ _aexp) >>= fun (_, e) -> WRITE_EXPR(e)) |~|
    _asum
  ) inp

and _asum = fun inp -> 
  (
    ((_aterm ++ _add ++ _aexp) >>= fun ((a, b), c) -> AEXP(b, a, c)) |~|
    _aterm
  ) inp

and _aterm: aexp parser = fun inp ->
  (
   ((_afinal ++ _mult ++ _aterm) >>= fun ((a, b), c) -> AEXP(b, a, c)) |~|
    _afinal 
  ) inp

and _afinal: aexp parser = fun inp ->
  (
    _n |~|
    (_id >>= fun s -> VAR(s)) |~|
    ((_lp ++ _aexp ++ _rp) >>= fun ((_, b), _) -> b) |~|
    ((_id ++ _lp ++ (list_parser _aexp _comma) ++ _rp) >>= fun (((name, _), args), _)-> CALL(name, args))
  ) inp

and _def: def parser = fun inp -> 
  (
    (_define ++ _id ++ _lp ++ (list_parser _id _comma) ++ _rp ++ _assign ++ _aexp)
      >>= fun ((((((_, s), _), a), _), _), bl) -> FUNC(s, a, bl)
  ) inp

and _prog = fun inp ->
  (
    (_aexp >>= fun e -> EXP(e))|~|
    ((_aexp ++ _sc ++ _prog) >>= fun ((e, _), p) -> EXP_SEQ(e, p))|~|
    ((_def ++ _sc ++ _prog) >>= fun ((d, _), p) -> DEF_SEQ(d, p))
  ) inp