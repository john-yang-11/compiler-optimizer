let fold_branches (f : Ir.func) =
  let fold (b : Ir.block) =
    match b.term with
    | Ir.Br (Ir.Imm n, t, e) -> { b with term = Ir.Jmp (if n <> 0 then t else e) }
    | Ir.Br (_, t, e) when t = e -> { b with term = Ir.Jmp t }
    | _ -> b
  in
  { f with blocks = List.map fold f.blocks }

let thread_jumps (f : Ir.func) =
  let entry = (Cfg.entry f).label in
  let forward = Hashtbl.create 16 in
  List.iter
    (fun (b : Ir.block) ->
      match (b.instrs, b.term) with
      | [], Ir.Jmp t when b.label <> entry && t <> b.label -> Hashtbl.replace forward b.label t
      | _ -> ())
    f.blocks;
  let rec resolve seen l =
    match Hashtbl.find_opt forward l with
    | Some t when not (List.mem t seen) -> resolve (l :: seen) t
    | _ -> l
  in
  { f with blocks = List.map (fun (b : Ir.block) -> { b with term = Ir.map_targets (resolve []) b.term }) f.blocks }

let merge_once (f : Ir.func) =
  let entry = (Cfg.entry f).label in
  let incoming = Cfg.incoming_edges f in
  let table = Cfg.block_table f in
  let candidate =
    List.find_map
      (fun (a : Ir.block) ->
        match a.term with
        | Ir.Jmp t when t <> a.label && t <> entry && incoming t = 1 -> Some (a, Hashtbl.find table t)
        | _ -> None)
      f.blocks
  in
  match candidate with
  | None -> None
  | Some (a, b) ->
      let merged = { a with instrs = a.instrs @ b.instrs; term = b.term } in
      let blocks =
        List.filter_map
          (fun (x : Ir.block) ->
            if x.label = b.label then None else if x.label = a.label then Some merged else Some x)
          f.blocks
      in
      Some { f with blocks }

let rec merge_blocks f = match merge_once f with None -> f | Some f -> merge_blocks f

let rec run (f : Ir.func) =
  let f' = f |> fold_branches |> thread_jumps |> Cfg.prune_unreachable |> merge_blocks in
  if f' = f then f else run f'
