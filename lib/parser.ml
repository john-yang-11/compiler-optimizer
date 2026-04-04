open Lexer

exception Error of string * int

type state = { toks : (token * int) array; mutable pos : int }

let make src =
  let toks = try Lexer.tokenize src with Lexer.Error (msg, line) -> raise (Error (msg, line)) in
  { toks = Array.of_list toks; pos = 0 }

let peek st = fst st.toks.(st.pos)
let peek2 st = if st.pos + 1 < Array.length st.toks then fst st.toks.(st.pos + 1) else EOF
let line st = snd st.toks.(st.pos)
let advance st = if st.pos < Array.length st.toks - 1 then st.pos <- st.pos + 1
let fail st msg = raise (Error (msg, line st))
let expect st tok what = if peek st = tok then advance st else fail st ("expected " ^ what)

let ident st =
  match peek st with
  | IDENT x ->
      advance st;
      x
  | _ -> fail st "expected identifier"

type infix = Logical_or | Logical_and | Arith of Ast.binop

let infix_of = function
  | OROR -> Some (1, Logical_or)
  | ANDAND -> Some (2, Logical_and)
  | EQEQ -> Some (3, Arith Ast.Eq)
  | NEQ -> Some (3, Arith Ast.Ne)
  | LT -> Some (4, Arith Ast.Lt)
  | LE -> Some (4, Arith Ast.Le)
  | GT -> Some (4, Arith Ast.Gt)
  | GE -> Some (4, Arith Ast.Ge)
  | PLUS -> Some (5, Arith Ast.Add)
  | MINUS -> Some (5, Arith Ast.Sub)
  | STAR -> Some (6, Arith Ast.Mul)
  | SLASH -> Some (6, Arith Ast.Div)
  | PERCENT -> Some (6, Arith Ast.Rem)
  | _ -> None

let rec expr st min_prec =
  let rec climb lhs =
    match infix_of (peek st) with
    | Some (prec, op) when prec >= min_prec ->
        advance st;
        let rhs = expr st (prec + 1) in
        let combined =
          match op with
          | Logical_or -> Ast.Or (lhs, rhs)
          | Logical_and -> Ast.And (lhs, rhs)
          | Arith b -> Ast.Binary (b, lhs, rhs)
        in
        climb combined
    | _ -> lhs
  in
  climb (unary st)

and unary st =
  match peek st with
  | MINUS ->
      advance st;
      Ast.Unary (Ast.Neg, unary st)
  | BANG ->
      advance st;
      Ast.Unary (Ast.Not, unary st)
  | _ -> primary st

and primary st =
  match peek st with
  | INT n ->
      advance st;
      Ast.Int n
  | TRUE ->
      advance st;
      Ast.Bool true
  | FALSE ->
      advance st;
      Ast.Bool false
  | IDENT x ->
      advance st;
      if peek st = LPAREN then (
        advance st;
        Ast.Call (x, args st))
      else Ast.Var x
  | LPAREN ->
      advance st;
      let e = expr st 1 in
      expect st RPAREN "')'";
      e
  | _ -> fail st "expected expression"

and args st =
  if peek st = RPAREN then (
    advance st;
    [])
  else
    let rec more acc =
      let e = expr st 1 in
      match peek st with
      | COMMA ->
          advance st;
          more (e :: acc)
      | RPAREN ->
          advance st;
          List.rev (e :: acc)
      | _ -> fail st "expected ',' or ')'"
    in
    more []

let rec stmt st =
  match peek st with
  | LET ->
      advance st;
      let x = ident st in
      expect st ASSIGN "'='";
      let e = expr st 1 in
      expect st SEMI "';'";
      Ast.Let (x, e)
  | IF ->
      advance st;
      if_rest st
  | WHILE ->
      advance st;
      expect st LPAREN "'('";
      let c = expr st 1 in
      expect st RPAREN "')'";
      Ast.While (c, block st)
  | RETURN ->
      advance st;
      if peek st = SEMI then (
        advance st;
        Ast.Return None)
      else
        let e = expr st 1 in
        expect st SEMI "';'";
        Ast.Return (Some e)
  | PRINT ->
      advance st;
      expect st LPAREN "'('";
      let e = expr st 1 in
      expect st RPAREN "')'";
      expect st SEMI "';'";
      Ast.Print e
  | BREAK ->
      advance st;
      expect st SEMI "';'";
      Ast.Break
  | CONTINUE ->
      advance st;
      expect st SEMI "';'";
      Ast.Continue
  | IDENT x when peek2 st = ASSIGN ->
      advance st;
      advance st;
      let e = expr st 1 in
      expect st SEMI "';'";
      Ast.Assign (x, e)
  | _ ->
      let e = expr st 1 in
      expect st SEMI "';'";
      Ast.Expr e

and if_rest st =
  expect st LPAREN "'('";
  let c = expr st 1 in
  expect st RPAREN "')'";
  let then_ = block st in
  if peek st = ELSE then (
    advance st;
    if peek st = IF then (
      advance st;
      Ast.If (c, then_, [ if_rest st ]))
    else Ast.If (c, then_, block st))
  else Ast.If (c, then_, [])

and block st =
  expect st LBRACE "'{'";
  let rec go acc =
    match peek st with
    | RBRACE ->
        advance st;
        List.rev acc
    | EOF -> fail st "unexpected end of input"
    | _ -> go (stmt st :: acc)
  in
  go []

let params st =
  expect st LPAREN "'('";
  if peek st = RPAREN then (
    advance st;
    [])
  else
    let rec more acc =
      let p = ident st in
      match peek st with
      | COMMA ->
          advance st;
          more (p :: acc)
      | RPAREN ->
          advance st;
          List.rev (p :: acc)
      | _ -> fail st "expected ',' or ')'"
    in
    more []

let func st =
  expect st FN "'fn'";
  let name = ident st in
  let params = params st in
  let body = block st in
  { Ast.name; params; body }

let program_of_string src =
  let st = make src in
  let rec go acc = if peek st = EOF then List.rev acc else go (func st :: acc) in
  go []

let expr_of_string src =
  let st = make src in
  let e = expr st 1 in
  if peek st <> EOF then fail st "unexpected trailing input";
  e
