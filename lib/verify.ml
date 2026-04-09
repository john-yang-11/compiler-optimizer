exception Invalid of string

let fail fmt = Printf.ksprintf (fun s -> raise (Invalid s)) fmt

let func (f : Ir.func) =
  if f.blocks = [] then fail "@%s has no blocks" f.name;
  let entry = (Cfg.entry f).label in
  let labels = Hashtbl.create 16 in
  List.iter
    (fun (b : Ir.block) ->
      if Hashtbl.mem labels b.label then fail "@%s: duplicate label %s" f.name b.label;
      Hashtbl.replace labels b.label ())
    f.blocks;
  let defined = Hashtbl.create 32 in
  List.iter
    (fun p ->
      if Hashtbl.mem defined p then fail "@%s: duplicate parameter %%%s" f.name p;
      Hashtbl.replace defined p ())
    f.params;
  List.iter
    (fun (b : Ir.block) ->
      List.iter (fun i -> Option.iter (fun d -> Hashtbl.replace defined d ()) (Ir.def i)) b.instrs)
    f.blocks;
  List.iter
    (fun (b : Ir.block) ->
      List.iter
        (fun l ->
          if not (Hashtbl.mem labels l) then fail "@%s: %s branches to unknown label %s" f.name b.label l;
          if l = entry then fail "@%s: %s branches to the entry block" f.name b.label)
        (Ir.successors b.term);
      let check r =
        if not (Hashtbl.mem defined r) then fail "@%s: use of undefined register %%%s in %s" f.name r b.label
      in
      List.iter (fun i -> List.iter check (Ir.uses i)) b.instrs;
      List.iter check (Ir.term_uses b.term))
    f.blocks

let program (p : Ir.program) =
  let arities = Hashtbl.create 16 in
  List.iter
    (fun (f : Ir.func) ->
      if Hashtbl.mem arities f.name then fail "duplicate function @%s" f.name;
      Hashtbl.replace arities f.name (List.length f.params))
    p;
  List.iter
    (fun (f : Ir.func) ->
      func f;
      List.iter
        (fun (b : Ir.block) ->
          List.iter
            (function
              | Ir.Call (_, g, args) -> (
                  match Hashtbl.find_opt arities g with
                  | None -> fail "@%s: call to unknown function @%s" f.name g
                  | Some n when n <> List.length args -> fail "@%s: @%s called with wrong arity" f.name g
                  | Some _ -> ())
              | _ -> ())
            b.instrs)
        f.blocks)
    p
