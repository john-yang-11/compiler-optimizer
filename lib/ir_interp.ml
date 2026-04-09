open Semantics

let run ?(fuel = default_fuel) (prog : Ir.program) : result =
  let funcs = Hashtbl.create 16 in
  List.iter (fun (f : Ir.func) -> Hashtbl.replace funcs f.name (f, Cfg.block_table f)) prog;
  let output = ref [] in
  let steps = ref 0 in
  let tick () =
    incr steps;
    if !steps > fuel then raise Out_of_fuel
  in
  let rec call name args =
    match Hashtbl.find_opt funcs name with
    | None -> raise (Runtime_error ("unknown function " ^ name))
    | Some ((f : Ir.func), blocks) ->
        if List.length f.params <> List.length args then
          raise (Runtime_error ("arity mismatch calling " ^ name));
        let regs = Hashtbl.create 32 in
        List.iter2 (Hashtbl.replace regs) f.params args;
        let get = function
          | Ir.Imm n -> n
          | Ir.Reg r -> (
              match Hashtbl.find_opt regs r with
              | Some v -> v
              | None -> raise (Runtime_error ("read of undefined register %" ^ r)))
        in
        let set = Hashtbl.replace regs in
        let exec (i : Ir.instr) =
          tick ();
          match i with
          | Ir.Bin (d, op, a, b) ->
              let x = get a in
              let y = get b in
              set d (try binop op x y with Division_by_zero -> raise (Runtime_error "division by zero"))
          | Ir.Un (d, op, a) -> set d (unop op (get a))
          | Ir.Mov (d, a) -> set d (get a)
          | Ir.Call (d, g, ops) ->
              let v = call g (List.map get ops) in
              Option.iter (fun d -> set d v) d
          | Ir.Print a -> output := string_of_int (get a) :: !output
        in
        let find l =
          match Hashtbl.find_opt blocks l with
          | Some b -> b
          | None -> raise (Runtime_error ("branch to unknown label " ^ l))
        in
        let rec go (b : Ir.block) =
          List.iter exec b.instrs;
          tick ();
          match b.term with
          | Ir.Jmp l -> go (find l)
          | Ir.Br (c, t, e) -> go (find (if get c <> 0 then t else e))
          | Ir.Ret a -> get a
        in
        go (Cfg.entry f)
  in
  let outcome =
    match call "main" [] with
    | _ -> Finished
    | exception Runtime_error msg -> Failed msg
    | exception Out_of_fuel -> Timeout
    | exception Stack_overflow -> Failed "stack overflow"
  in
  { output = List.rev !output; outcome; steps = !steps }
