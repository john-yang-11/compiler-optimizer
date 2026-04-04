type binop = Add | Sub | Mul | Div | Rem | Eq | Ne | Lt | Le | Gt | Ge
type unop = Neg | Not

type expr =
  | Int of int
  | Bool of bool
  | Var of string
  | Binary of binop * expr * expr
  | Unary of unop * expr
  | And of expr * expr
  | Or of expr * expr
  | Call of string * expr list

type stmt =
  | Let of string * expr
  | Assign of string * expr
  | If of expr * stmt list * stmt list
  | While of expr * stmt list
  | Return of expr option
  | Print of expr
  | Expr of expr
  | Break
  | Continue

type func = { name : string; params : string list; body : stmt list }
type program = func list
