module SMap = Map.Make (String)

let produces_bool (i : Ir.instr) =
  match i with
  | Ir.Bin (_, op, _, _) -> Semantics.is_comparison op
  | Ir.Un (_, Ast.Not, _) -> true
  | _ -> false

let canonicalize (i : Ir.instr) =
  match i with
  | Ir.Bin (d, op, (Ir.Imm _ as a), (Ir.Reg _ as b)) when Semantics.is_commutative op -> Ir.Bin (d, op, b, a)
  | Ir.Bin (d, ((Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge) as op), (Ir.Imm _ as a), (Ir.Reg _ as b)) ->
      Ir.Bin (d, Semantics.swap_comparison op, b, a)
  | _ -> i

let rec simplify defs (i : Ir.instr) : Ir.instr =
  let def_of t = SMap.find_opt t defs in
  match canonicalize i with
  | Ir.Bin (d, (Ast.Add | Ast.Sub), x, Ir.Imm 0) -> Ir.Mov (d, x)
  | Ir.Bin (d, (Ast.Mul | Ast.Div), x, Ir.Imm 1) -> Ir.Mov (d, x)
  | Ir.Bin (d, Ast.Mul, _, Ir.Imm 0) -> Ir.Mov (d, Ir.Imm 0)
  | Ir.Bin (d, Ast.Rem, _, Ir.Imm (1 | -1)) -> Ir.Mov (d, Ir.Imm 0)
  | Ir.Bin (d, (Ast.Mul | Ast.Div), x, Ir.Imm -1) -> Ir.Un (d, Ast.Neg, x)
  | Ir.Bin (d, Ast.Sub, Ir.Imm 0, x) -> Ir.Un (d, Ast.Neg, x)
  | Ir.Bin (d, Ast.Sub, Ir.Reg x, Ir.Reg y) when x = y -> Ir.Mov (d, Ir.Imm 0)
  | Ir.Bin (d, (Ast.Eq | Ast.Le | Ast.Ge), Ir.Reg x, Ir.Reg y) when x = y -> Ir.Mov (d, Ir.Imm 1)
  | Ir.Bin (d, (Ast.Ne | Ast.Lt | Ast.Gt), Ir.Reg x, Ir.Reg y) when x = y -> Ir.Mov (d, Ir.Imm 0)
  | Ir.Bin (d, Ast.Sub, x, Ir.Imm c) when c <> min_int -> simplify defs (Ir.Bin (d, Ast.Add, x, Ir.Imm (-c)))
  | Ir.Bin (d, ((Ast.Add | Ast.Mul) as op), Ir.Reg t, Ir.Imm c2) as i -> (
      match def_of t with
      | Some (Ir.Bin (_, op', x, Ir.Imm c1)) when op' = op ->
          let c = if op = Ast.Add then c1 + c2 else c1 * c2 in
          simplify defs (Ir.Bin (d, op, x, Ir.Imm c))
      | _ -> i)
  | Ir.Bin (d, Ast.Ne, Ir.Reg t, Ir.Imm 0) as i -> (
      match def_of t with Some def when produces_bool def -> Ir.Mov (d, Ir.Reg t) | _ -> i)
  | Ir.Bin (d, Ast.Eq, Ir.Reg t, Ir.Imm 0) as i -> (
      match def_of t with
      | Some (Ir.Bin (_, op, a, b)) when Semantics.is_comparison op ->
          Ir.Bin (d, Semantics.negate_comparison op, a, b)
      | Some (Ir.Un (_, Ast.Not, a)) -> Ir.Bin (d, Ast.Ne, a, Ir.Imm 0)
      | _ -> i)
  | Ir.Un (d, Ast.Not, Ir.Reg t) as i -> (
      match def_of t with
      | Some (Ir.Bin (_, op, a, b)) when Semantics.is_comparison op ->
          Ir.Bin (d, Semantics.negate_comparison op, a, b)
      | _ -> i)
  | Ir.Un (d, Ast.Neg, Ir.Reg t) as i -> (
      match def_of t with Some (Ir.Un (_, Ast.Neg, x)) -> Ir.Mov (d, x) | _ -> i)
  | i -> i

let invalidate d defs = SMap.filter (fun k i -> k <> d && not (List.mem d (Ir.uses i))) defs

let record defs (i : Ir.instr) =
  match Ir.def i with
  | None -> defs
  | Some d -> (
      let defs = invalidate d defs in
      match i with
      | (Ir.Bin _ | Ir.Un _) when not (List.mem d (Ir.uses i)) -> SMap.add d i defs
      | _ -> defs)

let simplify_term defs (term : Ir.terminator) =
  match term with
  | Ir.Br (Ir.Reg t, l1, l2) -> (
      match SMap.find_opt t defs with
      | Some (Ir.Bin (_, Ast.Ne, x, Ir.Imm 0)) -> Ir.Br (x, l1, l2)
      | Some (Ir.Bin (_, Ast.Eq, x, Ir.Imm 0)) | Some (Ir.Un (_, Ast.Not, x)) -> Ir.Br (x, l2, l1)
      | _ -> term)
  | _ -> term

let run_block (b : Ir.block) =
  let defs, rev =
    List.fold_left
      (fun (defs, acc) i ->
        let i = simplify defs i in
        (record defs i, i :: acc))
      (SMap.empty, []) b.instrs
  in
  { b with instrs = List.rev rev; term = simplify_term defs b.term }

let run (f : Ir.func) = { f with blocks = List.map run_block f.blocks }
