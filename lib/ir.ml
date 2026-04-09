type operand = Reg of string | Imm of int

type instr =
  | Bin of string * Ast.binop * operand * operand
  | Un of string * Ast.unop * operand
  | Mov of string * operand
  | Call of string option * string * operand list
  | Print of operand

type terminator = Jmp of string | Br of operand * string * string | Ret of operand
type block = { label : string; instrs : instr list; term : terminator }
type func = { name : string; params : string list; blocks : block list }
type program = func list

let def = function
  | Bin (d, _, _, _) | Un (d, _, _) | Mov (d, _) | Call (Some d, _, _) -> Some d
  | Call (None, _, _) | Print _ -> None

let operands = function
  | Bin (_, _, a, b) -> [ a; b ]
  | Un (_, _, a) | Mov (_, a) | Print a -> [ a ]
  | Call (_, _, args) -> args

let regs_of ops = List.filter_map (function Reg r -> Some r | Imm _ -> None) ops
let uses i = regs_of (operands i)

let map_operands f = function
  | Bin (d, op, a, b) -> Bin (d, op, f a, f b)
  | Un (d, op, a) -> Un (d, op, f a)
  | Mov (d, a) -> Mov (d, f a)
  | Call (d, g, args) -> Call (d, g, List.map f args)
  | Print a -> Print (f a)

let term_operands = function Jmp _ -> [] | Br (c, _, _) -> [ c ] | Ret a -> [ a ]
let term_uses t = regs_of (term_operands t)

let map_term_operands f = function
  | Jmp l -> Jmp l
  | Br (c, t, e) -> Br (f c, t, e)
  | Ret a -> Ret (f a)

let successors = function Jmp l -> [ l ] | Br (_, t, e) -> [ t; e ] | Ret _ -> []

let map_targets f = function
  | Jmp l -> Jmp (f l)
  | Br (c, t, e) -> Br (c, f t, f e)
  | Ret a -> Ret a

let block_size b = List.length b.instrs + 1
let func_size f = List.fold_left (fun acc b -> acc + block_size b) 0 f.blocks
let program_size p = List.fold_left (fun acc f -> acc + func_size f) 0 p

let binop_name (op : Ast.binop) =
  match op with
  | Ast.Add -> "add"
  | Ast.Sub -> "sub"
  | Ast.Mul -> "mul"
  | Ast.Div -> "div"
  | Ast.Rem -> "rem"
  | Ast.Eq -> "eq"
  | Ast.Ne -> "ne"
  | Ast.Lt -> "lt"
  | Ast.Le -> "le"
  | Ast.Gt -> "gt"
  | Ast.Ge -> "ge"

let unop_name (op : Ast.unop) = match op with Ast.Neg -> "neg" | Ast.Not -> "not"
let string_of_operand = function Reg r -> "%" ^ r | Imm n -> string_of_int n
let string_of_args args = String.concat ", " (List.map string_of_operand args)

let string_of_instr = function
  | Bin (d, op, a, b) ->
      Printf.sprintf "%%%s = %s %s, %s" d (binop_name op) (string_of_operand a) (string_of_operand b)
  | Un (d, op, a) -> Printf.sprintf "%%%s = %s %s" d (unop_name op) (string_of_operand a)
  | Mov (d, a) -> Printf.sprintf "%%%s = mov %s" d (string_of_operand a)
  | Call (Some d, g, args) -> Printf.sprintf "%%%s = call @%s(%s)" d g (string_of_args args)
  | Call (None, g, args) -> Printf.sprintf "call @%s(%s)" g (string_of_args args)
  | Print a -> "print " ^ string_of_operand a

let string_of_term = function
  | Jmp l -> "jmp " ^ l
  | Br (c, t, e) -> Printf.sprintf "br %s, %s, %s" (string_of_operand c) t e
  | Ret a -> "ret " ^ string_of_operand a

let string_of_block b =
  let body = String.concat "" (List.map (fun i -> "  " ^ string_of_instr i ^ "\n") b.instrs) in
  b.label ^ ":\n" ^ body ^ "  " ^ string_of_term b.term ^ "\n"

let string_of_func f =
  let params = String.concat ", " (List.map (fun p -> "%" ^ p) f.params) in
  Printf.sprintf "define @%s(%s) {\n%s}\n" f.name params
    (String.concat "" (List.map string_of_block f.blocks))

let string_of_program p = String.concat "\n" (List.map string_of_func p)
