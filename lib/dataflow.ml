module type FACT = sig
  type t

  val bottom : t
  val equal : t -> t -> bool
  val join : t -> t -> t
end

module Make (F : FACT) = struct
  type table = (string, F.t) Hashtbl.t

  let worklist labels =
    let queue = Queue.create () in
    let queued = Hashtbl.create 16 in
    let push l =
      if not (Hashtbl.mem queued l) then begin
        Hashtbl.replace queued l ();
        Queue.push l queue
      end
    in
    let pop () =
      let l = Queue.pop queue in
      Hashtbl.remove queued l;
      l
    in
    List.iter push labels;
    (push, pop, fun () -> Queue.is_empty queue)

  let forward (f : Ir.func) ~(init : F.t) ~(transfer : Ir.block -> F.t -> (string * F.t) list) : table =
    let blocks = Cfg.block_table f in
    let inn = Hashtbl.create 16 in
    List.iter (fun (b : Ir.block) -> Hashtbl.replace inn b.label F.bottom) f.blocks;
    Hashtbl.replace inn (Cfg.entry f).label init;
    let push, pop, is_empty = worklist (Cfg.reverse_postorder f) in
    while not (is_empty ()) do
      let l = pop () in
      let b = Hashtbl.find blocks l in
      List.iter
        (fun (succ, fact) ->
          let old = Hashtbl.find inn succ in
          let merged = F.join old fact in
          if not (F.equal old merged) then begin
            Hashtbl.replace inn succ merged;
            push succ
          end)
        (transfer b (Hashtbl.find inn l))
    done;
    inn

  let backward (f : Ir.func) ~(transfer : Ir.block -> F.t -> F.t) : table * table =
    let blocks = Cfg.block_table f in
    let preds = Cfg.predecessors f in
    let inn = Hashtbl.create 16 in
    let out = Hashtbl.create 16 in
    List.iter
      (fun (b : Ir.block) ->
        Hashtbl.replace inn b.label F.bottom;
        Hashtbl.replace out b.label F.bottom)
      f.blocks;
    let push, pop, is_empty = worklist (List.rev (Cfg.reverse_postorder f)) in
    while not (is_empty ()) do
      let l = pop () in
      let b = Hashtbl.find blocks l in
      let o = List.fold_left (fun acc s -> F.join acc (Hashtbl.find inn s)) F.bottom (Cfg.successors b) in
      Hashtbl.replace out l o;
      let i = transfer b o in
      if not (F.equal i (Hashtbl.find inn l)) then begin
        Hashtbl.replace inn l i;
        List.iter push (preds l)
      end
    done;
    (inn, out)
end
