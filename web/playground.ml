open Optimizer
open Js_of_ocaml

let str s = Js.Unsafe.inject (Js.string s)
let int n = Js.Unsafe.inject n
let arr f xs = Js.Unsafe.inject (Js.array (Array.of_list (List.map f xs)))
let obj fields = Js.Unsafe.obj (Array.of_list fields)

let func_js (f : Ir.func) =
  obj
    [
      ("name", str f.name);
      ("params", arr str f.params);
      ( "blocks",
        arr
          (fun (b : Ir.block) ->
            obj
              [
                ("label", str b.label);
                ("instrs", arr (fun i -> str (Ir.string_of_instr i)) b.instrs);
                ("term", str (Ir.string_of_term b.term));
                ("succs", arr str (Ir.successors b.term));
              ])
          f.blocks );
    ]

let program_js p = arr func_js p

let result_js (r : Semantics.result) =
  obj
    [
      ("output", str (Semantics.render r));
      ("steps", int r.steps);
      ("ok", Js.Unsafe.inject (Js.bool (r.outcome = Semantics.Finished)));
    ]

let trace limit (prog : Ir.program) =
  let funcs = Hashtbl.create 16 in
  List.iter (fun (f : Ir.func) -> Hashtbl.replace funcs f.name (f, Cfg.block_table f)) prog;
  let events = ref [] and n = ref 0 in
  let ev fields =
    incr n;
    if !n > limit then raise Semantics.Out_of_fuel;
    events := obj fields :: !events
  in
  let rec call name args =
    let (f : Ir.func), blocks =
      match Hashtbl.find_opt funcs name with
      | Some x -> x
      | None -> raise (Semantics.Runtime_error ("unknown function " ^ name))
    in
    let regs = Hashtbl.create 32 in
    List.iter2 (Hashtbl.replace regs) f.params args;
    let get = function
      | Ir.Imm v -> v
      | Ir.Reg r -> (
          match Hashtbl.find_opt regs r with
          | Some v -> v
          | None -> raise (Semantics.Runtime_error ("read of undefined register %" ^ r)))
    in
    let rec go (b : Ir.block) =
      let at i k rest = ev ([ ("fn", str f.name); ("block", str b.label); ("idx", int i); ("k", str k) ] @ rest) in
      List.iteri
        (fun i (ins : Ir.instr) ->
          let set d v =
            Hashtbl.replace regs d v;
            at i "set" [ ("reg", str d); ("val", int v) ]
          in
          match ins with
          | Ir.Bin (d, op, a, c) ->
              let x = get a and y = get c in
              set d (try Semantics.binop op x y with Division_by_zero -> raise (Semantics.Runtime_error "division by zero"))
          | Ir.Un (d, op, a) -> set d (Semantics.unop op (get a))
          | Ir.Mov (d, a) -> set d (get a)
          | Ir.Print a ->
              let v = get a in
              at i "print" [ ("val", int v) ]
          | Ir.Call (d, g, ops) -> (
              let vs = List.map get ops in
              at i "call" [ ("callee", str g); ("args", arr int vs) ];
              let v = call g vs in
              match d with Some d -> set d v | None -> at i "return" [])
        )
        b.instrs;
      let t = List.length b.instrs in
      let find l =
        match Hashtbl.find_opt blocks l with
        | Some b -> b
        | None -> raise (Semantics.Runtime_error ("branch to unknown label " ^ l))
      in
      match b.term with
      | Ir.Jmp l ->
          at t "jump" [ ("to", str l) ];
          go (find l)
      | Ir.Br (c, l1, l2) ->
          let l = if get c <> 0 then l1 else l2 in
          at t "jump" [ ("to", str l) ];
          go (find l)
      | Ir.Ret a ->
          let v = get a in
          at t "ret" [ ("val", int v) ];
          v
    in
    go (Cfg.entry f)
  in
  let status =
    match call "main" [] with
    | _ -> "finished"
    | exception Semantics.Runtime_error msg -> "runtime error: " ^ msg
    | exception Semantics.Out_of_fuel -> Printf.sprintf "trace stopped after %d steps" limit
    | exception Stack_overflow -> "runtime error: stack overflow"
  in
  obj [ ("events", arr Fun.id (List.rev !events)); ("status", str status) ]

let fuel = 5_000_000

let compile src =
  try
    let ast = Parser.program_of_string (Js.to_string src) in
    let ir0 = Lower.program ast in
    let steps = ref [] in
    let on_pass ~round name prog = steps := (round, name, prog) :: !steps in
    let opt = (Pass_manager.run ~on_pass Pass_manager.o1 ir0).program in
    let step (round, name, prog) =
      obj [ ("round", int round); ("pass", str name); ("program", program_js prog); ("size", int (Ir.program_size prog)) ]
    in
    obj
      [
        ("ok", Js.Unsafe.inject Js._true);
        ("ir0", program_js ir0);
        ("size0", int (Ir.program_size ir0));
        ("steps", arr step (List.rev !steps));
        ("reference", result_js (Interp.run ~fuel ast));
        ("run0", result_js (Ir_interp.run ~fuel ir0));
        ("run1", result_js (Ir_interp.run ~fuel opt));
        ("trace0", trace 20_000 ir0);
        ("trace1", trace 20_000 opt);
      ]
  with
  | Parser.Error (msg, line) -> obj [ ("ok", Js.Unsafe.inject Js._false); ("error", str msg); ("line", int line) ]
  | Lower.Error msg | Verify.Invalid msg | Invalid_argument msg ->
      obj [ ("ok", Js.Unsafe.inject Js._false); ("error", str msg) ]

let () =
  Js.export "Optimizer"
    (obj
       [
         ("compile", Js.Unsafe.inject (Js.wrap_callback compile));
         ( "passes",
           arr
             (fun (p : Pass_manager.pass) -> obj [ ("name", str p.name); ("description", str p.description) ])
             Pass_manager.registry );
         ("examples", arr (fun (name, src) -> obj [ ("name", str name); ("source", str src) ]) Examples.all);
       ])
