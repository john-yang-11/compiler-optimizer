module SSet = Set.Make (String)

let entry (f : Ir.func) =
  match f.blocks with b :: _ -> b | [] -> invalid_arg ("function @" ^ f.name ^ " has no blocks")

let successors (b : Ir.block) = Ir.successors b.term

let block_table (f : Ir.func) =
  let table = Hashtbl.create 16 in
  List.iter (fun (b : Ir.block) -> Hashtbl.replace table b.label b) f.blocks;
  table

let reverse_postorder (f : Ir.func) =
  let table = block_table f in
  let visited = Hashtbl.create 16 in
  let order = ref [] in
  let rec dfs l =
    if not (Hashtbl.mem visited l) then begin
      Hashtbl.replace visited l ();
      (match Hashtbl.find_opt table l with Some b -> List.iter dfs (successors b) | None -> ());
      order := l :: !order
    end
  in
  dfs (entry f).label;
  !order

let reachable f = SSet.of_list (reverse_postorder f)

let prune_unreachable (f : Ir.func) =
  let live = reachable f in
  { f with blocks = List.filter (fun (b : Ir.block) -> SSet.mem b.label live) f.blocks }

let predecessors (f : Ir.func) =
  let preds = Hashtbl.create 16 in
  List.iter (fun (b : Ir.block) -> Hashtbl.replace preds b.label []) f.blocks;
  List.iter
    (fun (b : Ir.block) ->
      List.iter
        (fun s ->
          let existing = Option.value ~default:[] (Hashtbl.find_opt preds s) in
          if not (List.mem b.label existing) then Hashtbl.replace preds s (b.label :: existing))
        (successors b))
    f.blocks;
  fun l -> Option.value ~default:[] (Hashtbl.find_opt preds l)

let incoming_edges (f : Ir.func) =
  let counts = Hashtbl.create 16 in
  List.iter
    (fun (b : Ir.block) ->
      List.iter
        (fun s -> Hashtbl.replace counts s (1 + Option.value ~default:0 (Hashtbl.find_opt counts s)))
        (successors b))
    f.blocks;
  fun l -> Option.value ~default:0 (Hashtbl.find_opt counts l)
