type outcome = Finished | Failed of string | Timeout
type result = { output : string list; outcome : outcome; steps : int }

exception Runtime_error of string
exception Out_of_fuel

let default_fuel = 100_000_000
let of_bool b = if b then 1 else 0

let binop (op : Ast.binop) a b =
  match op with
  | Ast.Add -> a + b
  | Ast.Sub -> a - b
  | Ast.Mul -> a * b
  | Ast.Div -> if b = 0 then raise Division_by_zero else a / b
  | Ast.Rem -> if b = 0 then raise Division_by_zero else a mod b
  | Ast.Eq -> of_bool (a = b)
  | Ast.Ne -> of_bool (a <> b)
  | Ast.Lt -> of_bool (a < b)
  | Ast.Le -> of_bool (a <= b)
  | Ast.Gt -> of_bool (a > b)
  | Ast.Ge -> of_bool (a >= b)

let unop (op : Ast.unop) a = match op with Ast.Neg -> -a | Ast.Not -> of_bool (a = 0)

let is_comparison (op : Ast.binop) =
  match op with
  | Ast.Eq | Ast.Ne | Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge -> true
  | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div | Ast.Rem -> false

let is_commutative (op : Ast.binop) =
  match op with Ast.Add | Ast.Mul | Ast.Eq | Ast.Ne -> true | _ -> false

let negate_comparison (op : Ast.binop) =
  match op with
  | Ast.Eq -> Ast.Ne
  | Ast.Ne -> Ast.Eq
  | Ast.Lt -> Ast.Ge
  | Ast.Le -> Ast.Gt
  | Ast.Gt -> Ast.Le
  | Ast.Ge -> Ast.Lt
  | other -> other

let swap_comparison (op : Ast.binop) =
  match op with
  | Ast.Lt -> Ast.Gt
  | Ast.Gt -> Ast.Lt
  | Ast.Le -> Ast.Ge
  | Ast.Ge -> Ast.Le
  | other -> other

let render r =
  let body = String.concat "" (List.map (fun line -> line ^ "\n") r.output) in
  match r.outcome with
  | Finished -> body
  | Failed msg -> body ^ "runtime error: " ^ msg ^ "\n"
  | Timeout -> body ^ "timeout\n"
