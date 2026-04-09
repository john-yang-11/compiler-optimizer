exception Error of string

let fail fmt = Printf.ksprintf (fun s -> raise (Error s)) fmt

type loop_targets = { continue_to : string; break_to : string }

type state = {
  fname : string;
  arities : (string, int) Hashtbl.t;
  names : (string, int) Hashtbl.t;
  mutable blocks : Ir.block list;
  mutable label : string;
  mutable instrs : Ir.instr list;
  mutable next_temp : int;
  mutable next_label : int;
  mutable scopes : (string * string) list list;
  mutable loops : loop_targets list;
}

let emit st i = st.instrs <- i :: st.instrs

let fresh_temp st =
  let n = st.next_temp in
  st.next_temp <- n + 1;
  string_of_int n

let fresh_label st base =
  let n = st.next_label in
  st.next_label <- n + 1;
  base ^ string_of_int n

let close st term next =
  st.blocks <- { Ir.label = st.label; instrs = List.rev st.instrs; term } :: st.blocks;
  st.label <- next;
  st.instrs <- []

let declare st x =
  let reg =
    match Hashtbl.find_opt st.names x with
    | None ->
        Hashtbl.replace st.names x 1;
        x
    | Some k ->
        Hashtbl.replace st.names x (k + 1);
        Printf.sprintf "%s.%d" x k
  in
  (match st.scopes with
  | scope :: rest -> st.scopes <- ((x, reg) :: scope) :: rest
  | [] -> st.scopes <- [ [ (x, reg) ] ]);
  reg

let lookup st x =
  let rec go = function
    | [] -> fail "@%s: unbound variable %s" st.fname x
    | scope :: rest -> ( match List.assoc_opt x scope with Some r -> r | None -> go rest)
  in
  go st.scopes

let check_call st name nargs =
  match Hashtbl.find_opt st.arities name with
  | None -> fail "@%s: call to unknown function %s" st.fname name
  | Some n when n <> nargs -> fail "@%s: %s expects %d argument(s), got %d" st.fname name n nargs
  | Some _ -> ()

let rec expr st (e : Ast.expr) : Ir.operand =
  match e with
  | Ast.Int n -> Ir.Imm n
  | Ast.Bool b -> Ir.Imm (Semantics.of_bool b)
  | Ast.Var x -> Ir.Reg (lookup st x)
  | Ast.Binary (op, a, b) ->
      let x = expr st a in
      let y = expr st b in
      let t = fresh_temp st in
      emit st (Ir.Bin (t, op, x, y));
      Ir.Reg t
  | Ast.Unary (op, a) ->
      let x = expr st a in
      let t = fresh_temp st in
      emit st (Ir.Un (t, op, x));
      Ir.Reg t
  | Ast.Call (name, args) ->
      let ops = call_args st name args in
      let t = fresh_temp st in
      emit st (Ir.Call (Some t, name, ops));
      Ir.Reg t
  | Ast.And (a, b) -> short_circuit st ~is_and:true a b
  | Ast.Or (a, b) -> short_circuit st ~is_and:false a b

and call_args st name args =
  check_call st name (List.length args);
  List.rev (List.fold_left (fun acc a -> expr st a :: acc) [] args)

and short_circuit st ~is_and a b =
  let t = fresh_temp st in
  let lhs = expr st a in
  let prefix = if is_and then "and" else "or" in
  let rhs_label = fresh_label st (prefix ^ ".rhs") in
  let short_label = fresh_label st (prefix ^ ".short") in
  let end_label = fresh_label st (prefix ^ ".end") in
  let branch =
    if is_and then Ir.Br (lhs, rhs_label, short_label) else Ir.Br (lhs, short_label, rhs_label)
  in
  close st branch rhs_label;
  let rhs = expr st b in
  emit st (Ir.Bin (t, Ast.Ne, rhs, Ir.Imm 0));
  close st (Ir.Jmp end_label) short_label;
  emit st (Ir.Mov (t, Ir.Imm (if is_and then 0 else 1)));
  close st (Ir.Jmp end_label) end_label;
  Ir.Reg t

let rec stmt st (s : Ast.stmt) =
  match s with
  | Ast.Let (x, e) ->
      let v = expr st e in
      let r = declare st x in
      emit st (Ir.Mov (r, v))
  | Ast.Assign (x, e) ->
      let r = lookup st x in
      let v = expr st e in
      emit st (Ir.Mov (r, v))
  | Ast.Print e ->
      let v = expr st e in
      emit st (Ir.Print v)
  | Ast.Expr (Ast.Call (name, args)) ->
      let ops = call_args st name args in
      emit st (Ir.Call (None, name, ops))
  | Ast.Expr e -> ignore (expr st e)
  | Ast.If (c, then_, else_) ->
      let v = expr st c in
      let lt = fresh_label st "if.then" in
      let le = fresh_label st "if.else" in
      let lj = fresh_label st "if.end" in
      close st (Ir.Br (v, lt, le)) lt;
      block st then_;
      close st (Ir.Jmp lj) le;
      block st else_;
      close st (Ir.Jmp lj) lj
  | Ast.While (c, body) ->
      let lc = fresh_label st "while.cond" in
      let lb = fresh_label st "while.body" in
      let le = fresh_label st "while.end" in
      close st (Ir.Jmp lc) lc;
      let v = expr st c in
      close st (Ir.Br (v, lb, le)) lb;
      st.loops <- { continue_to = lc; break_to = le } :: st.loops;
      block st body;
      st.loops <- List.tl st.loops;
      close st (Ir.Jmp lc) le
  | Ast.Return e ->
      let v = match e with None -> Ir.Imm 0 | Some e -> expr st e in
      close st (Ir.Ret v) (fresh_label st "dead")
  | Ast.Break -> (
      match st.loops with
      | [] -> fail "@%s: break outside of a loop" st.fname
      | l :: _ -> close st (Ir.Jmp l.break_to) (fresh_label st "dead"))
  | Ast.Continue -> (
      match st.loops with
      | [] -> fail "@%s: continue outside of a loop" st.fname
      | l :: _ -> close st (Ir.Jmp l.continue_to) (fresh_label st "dead"))

and block st stmts =
  st.scopes <- [] :: st.scopes;
  List.iter (stmt st) stmts;
  st.scopes <- List.tl st.scopes

let func arities (f : Ast.func) : Ir.func =
  let st =
    {
      fname = f.name;
      arities;
      names = Hashtbl.create 16;
      blocks = [];
      label = "entry";
      instrs = [];
      next_temp = 0;
      next_label = 1;
      scopes = [ [] ];
      loops = [];
    }
  in
  let seen = Hashtbl.create 8 in
  List.iter
    (fun p ->
      if Hashtbl.mem seen p then fail "@%s: duplicate parameter %s" f.name p;
      Hashtbl.replace seen p ())
    f.params;
  let params = List.map (declare st) f.params in
  block st f.body;
  close st (Ir.Ret (Ir.Imm 0)) "";
  { Ir.name = f.name; params; blocks = List.rev st.blocks }

let program (p : Ast.program) : Ir.program =
  let arities = Hashtbl.create 16 in
  List.iter
    (fun (f : Ast.func) ->
      if Hashtbl.mem arities f.name then fail "duplicate function %s" f.name;
      Hashtbl.replace arities f.name (List.length f.params))
    p;
  (match Hashtbl.find_opt arities "main" with
  | None -> fail "no main function"
  | Some 0 -> ()
  | Some _ -> fail "main must take no parameters");
  List.map (func arities) p
