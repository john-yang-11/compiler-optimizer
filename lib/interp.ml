open Semantics

exception Return_value of int
exception Break_loop
exception Continue_loop

type env = { mutable scopes : (string, int ref) Hashtbl.t list }

let lookup env x =
  let rec go = function
    | [] -> raise (Runtime_error ("unbound variable " ^ x))
    | scope :: rest -> ( match Hashtbl.find_opt scope x with Some r -> r | None -> go rest)
  in
  go env.scopes

let run ?(fuel = default_fuel) (prog : Ast.program) : result =
  let funcs = Hashtbl.create 16 in
  List.iter (fun (f : Ast.func) -> Hashtbl.replace funcs f.name f) prog;
  let output = ref [] in
  let steps = ref 0 in
  let tick () =
    incr steps;
    if !steps > fuel then raise Out_of_fuel
  in
  let rec eval env (e : Ast.expr) =
    tick ();
    match e with
    | Ast.Int n -> n
    | Ast.Bool b -> of_bool b
    | Ast.Var x -> !(lookup env x)
    | Ast.Binary (op, a, b) -> (
        let x = eval env a in
        let y = eval env b in
        try binop op x y with Division_by_zero -> raise (Runtime_error "division by zero"))
    | Ast.Unary (op, a) -> unop op (eval env a)
    | Ast.And (a, b) -> if eval env a <> 0 then of_bool (eval env b <> 0) else 0
    | Ast.Or (a, b) -> if eval env a <> 0 then 1 else of_bool (eval env b <> 0)
    | Ast.Call (name, args) ->
        let values = List.rev (List.fold_left (fun acc a -> eval env a :: acc) [] args) in
        call name values
  and call name values =
    match Hashtbl.find_opt funcs name with
    | None -> raise (Runtime_error ("unknown function " ^ name))
    | Some f -> (
        if List.length f.params <> List.length values then
          raise (Runtime_error ("arity mismatch calling " ^ name));
        let frame = Hashtbl.create 8 in
        List.iter2 (fun p v -> Hashtbl.replace frame p (ref v)) f.params values;
        let env = { scopes = [ frame ] } in
        match exec_block env f.body with () -> 0 | exception Return_value v -> v)
  and exec env (s : Ast.stmt) =
    tick ();
    match s with
    | Ast.Let (x, e) ->
        let v = eval env e in
        Hashtbl.replace (List.hd env.scopes) x (ref v)
    | Ast.Assign (x, e) ->
        let v = eval env e in
        lookup env x := v
    | Ast.If (c, then_, else_) -> if eval env c <> 0 then exec_block env then_ else exec_block env else_
    | Ast.While (c, body) ->
        let rec loop () =
          if eval env c <> 0 then
            match exec_block env body with
            | () -> loop ()
            | exception Break_loop -> ()
            | exception Continue_loop -> loop ()
        in
        loop ()
    | Ast.Return e -> raise (Return_value (match e with None -> 0 | Some e -> eval env e))
    | Ast.Print e ->
        let v = eval env e in
        output := string_of_int v :: !output
    | Ast.Expr e -> ignore (eval env e)
    | Ast.Break -> raise Break_loop
    | Ast.Continue -> raise Continue_loop
  and exec_block env stmts =
    env.scopes <- Hashtbl.create 8 :: env.scopes;
    match List.iter (exec env) stmts with
    | () -> env.scopes <- List.tl env.scopes
    | exception ex ->
        env.scopes <- List.tl env.scopes;
        raise ex
  in
  let outcome =
    match call "main" [] with
    | _ -> Finished
    | exception Runtime_error msg -> Failed msg
    | exception Out_of_fuel -> Timeout
    | exception Stack_overflow -> Failed "stack overflow"
  in
  { output = List.rev !output; outcome; steps = !steps }
