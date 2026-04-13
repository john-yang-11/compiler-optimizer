module SMap = Map.Make (String)

type value = Const of int | Varying

module Env = struct
  type t = value SMap.t option

  let bottom = None

  let equal a b =
    match (a, b) with
    | None, None -> true
    | Some x, Some y -> SMap.equal ( = ) x y
    | _ -> false

  let join a b =
    match (a, b) with
    | None, x | x, None -> x
    | Some x, Some y -> Some (SMap.union (fun _ u v -> Some (if u = v then u else Varying)) x y)
end

module Solver = Dataflow.Make (Env)

let value_of env = function
  | Ir.Imm n -> Const n
  | Ir.Reg r -> ( match SMap.find_opt r env with Some v -> v | None -> Varying)

let fold_bin op a b =
  match (a, b) with
  | Const x, Const y -> ( match Semantics.binop op x y with n -> Const n | exception Division_by_zero -> Varying)
  | _ -> Varying

let fold_un op = function Const x -> Const (Semantics.unop op x) | Varying -> Varying

let eval env (i : Ir.instr) =
  match i with
  | Ir.Bin (d, op, a, b) -> SMap.add d (fold_bin op (value_of env a) (value_of env b)) env
  | Ir.Un (d, op, a) -> SMap.add d (fold_un op (value_of env a)) env
  | Ir.Mov (d, a) -> SMap.add d (value_of env a) env
  | Ir.Call (Some d, _, _) -> SMap.add d Varying env
  | Ir.Call (None, _, _) | Ir.Print _ -> env

let transfer (b : Ir.block) fact =
  match fact with
  | None -> List.map (fun s -> (s, None)) (Ir.successors b.term)
  | Some env -> (
      let env = List.fold_left eval env b.instrs in
      match b.term with
      | Ir.Jmp l -> [ (l, Some env) ]
      | Ir.Ret _ -> []
      | Ir.Br (c, t, e) -> (
          match value_of env c with
          | Const n when n <> 0 -> [ (t, Some env); (e, None) ]
          | Const _ -> [ (t, None); (e, Some env) ]
          | Varying -> [ (t, Some env); (e, Some env) ]))

let subst env = function
  | Ir.Reg r as o -> ( match SMap.find_opt r env with Some (Const n) -> Ir.Imm n | _ -> o)
  | o -> o

let rewrite_block (b : Ir.block) fact =
  match fact with
  | None -> b
  | Some env ->
      let env, rev =
        List.fold_left
          (fun (env, acc) i ->
            let i = Ir.map_operands (subst env) i in
            let env = eval env i in
            let i =
              match i with
              | Ir.Bin (d, _, _, _) | Ir.Un (d, _, _) -> (
                  match SMap.find_opt d env with Some (Const n) -> Ir.Mov (d, Ir.Imm n) | _ -> i)
              | _ -> i
            in
            (env, i :: acc))
          (env, []) b.instrs
      in
      let term =
        match Ir.map_term_operands (subst env) b.term with
        | Ir.Br (Ir.Imm n, t, e) -> Ir.Jmp (if n <> 0 then t else e)
        | t -> t
      in
      { b with instrs = List.rev rev; term }

let run (f : Ir.func) =
  let init = Some (List.fold_left (fun m p -> SMap.add p Varying m) SMap.empty f.params) in
  let facts = Solver.forward f ~init ~transfer in
  { f with blocks = List.map (fun (b : Ir.block) -> rewrite_block b (Hashtbl.find facts b.label)) f.blocks }
