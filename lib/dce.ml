module SSet = Set.Make (String)

module Live = struct
  type t = SSet.t

  let bottom = SSet.empty
  let equal = SSet.equal
  let join = SSet.union
end

module Solver = Dataflow.Make (Live)

let may_trap (i : Ir.instr) =
  match i with
  | Ir.Bin (_, (Ast.Div | Ast.Rem), _, Ir.Imm n) -> n = 0
  | Ir.Bin (_, (Ast.Div | Ast.Rem), _, Ir.Reg _) -> true
  | _ -> false

let is_dead live (i : Ir.instr) =
  match i with
  | Ir.Mov (d, Ir.Reg s) when d = s -> true
  | Ir.Bin (d, _, _, _) | Ir.Un (d, _, _) | Ir.Mov (d, _) -> (not (SSet.mem d live)) && not (may_trap i)
  | Ir.Call _ | Ir.Print _ -> false

let add_uses live regs = List.fold_left (fun s r -> SSet.add r s) live regs

let step live i =
  if is_dead live i then live
  else
    let live = match Ir.def i with Some d -> SSet.remove d live | None -> live in
    add_uses live (Ir.uses i)

let live_at_end (b : Ir.block) out = add_uses out (Ir.term_uses b.term)
let transfer (b : Ir.block) out = List.fold_right (fun i live -> step live i) b.instrs (live_at_end b out)

let rewrite_block (b : Ir.block) out =
  let _, kept =
    List.fold_right
      (fun i (live, acc) ->
        if is_dead live i then (live, acc)
        else
          let i =
            match i with
            | Ir.Call (Some d, g, args) when not (SSet.mem d live) -> Ir.Call (None, g, args)
            | _ -> i
          in
          (step live i, i :: acc))
      b.instrs
      (live_at_end b out, [])
  in
  { b with instrs = kept }

let run (f : Ir.func) =
  let f = Cfg.prune_unreachable f in
  let _, out = Solver.backward f ~transfer in
  { f with blocks = List.map (fun (b : Ir.block) -> rewrite_block b (Hashtbl.find out b.label)) f.blocks }
