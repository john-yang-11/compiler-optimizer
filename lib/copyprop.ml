module SMap = Map.Make (String)

module Copies = struct
  type t = string SMap.t option

  let bottom = None

  let equal a b =
    match (a, b) with
    | None, None -> true
    | Some x, Some y -> SMap.equal String.equal x y
    | _ -> false

  let join a b =
    match (a, b) with
    | None, x | x, None -> x
    | Some x, Some y ->
        Some
          (SMap.merge
             (fun _ u v -> match (u, v) with Some s, Some t when String.equal s t -> Some s | _ -> None)
             x y)
end

module Solver = Dataflow.Make (Copies)

let subst copies = function
  | Ir.Reg r as o -> ( match SMap.find_opt r copies with Some s -> Ir.Reg s | None -> o)
  | o -> o

let kill d copies = SMap.filter (fun k v -> k <> d && v <> d) copies

let step copies i =
  let i = Ir.map_operands (subst copies) i in
  let copies =
    match Ir.def i with
    | None -> copies
    | Some d -> (
        let copies = kill d copies in
        match i with Ir.Mov (_, Ir.Reg s) when s <> d -> SMap.add d s copies | _ -> copies)
  in
  (i, copies)

let transfer (b : Ir.block) fact =
  let out = Option.map (fun c -> List.fold_left (fun c i -> snd (step c i)) c b.instrs) fact in
  List.map (fun s -> (s, out)) (Ir.successors b.term)

let rewrite_block (b : Ir.block) fact =
  match fact with
  | None -> b
  | Some copies ->
      let copies, rev =
        List.fold_left
          (fun (c, acc) i ->
            let i, c = step c i in
            (c, i :: acc))
          (copies, []) b.instrs
      in
      { b with instrs = List.rev rev; term = Ir.map_term_operands (subst copies) b.term }

let run (f : Ir.func) =
  let facts = Solver.forward f ~init:(Some SMap.empty) ~transfer in
  { f with blocks = List.map (fun (b : Ir.block) -> rewrite_block b (Hashtbl.find facts b.label)) f.blocks }
