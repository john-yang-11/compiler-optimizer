let prelude =
  {|@.fmt = private unnamed_addr constant [6 x i8] c"%lld\0A\00"
@.divmsg = private unnamed_addr constant [32 x i8] c"runtime error: division by zero\00"

declare i32 @printf(ptr, ...)
declare i32 @puts(ptr)
declare void @exit(i32)

define internal i64 @rt.div(i64 %a, i64 %b) {
entry:
  %z = icmp eq i64 %b, 0
  br i1 %z, label %trap, label %ok
trap:
  call i32 @puts(ptr @.divmsg)
  call void @exit(i32 1)
  unreachable
ok:
  %q = sdiv i64 %a, %b
  ret i64 %q
}

define internal i64 @rt.rem(i64 %a, i64 %b) {
entry:
  %z = icmp eq i64 %b, 0
  br i1 %z, label %trap, label %ok
trap:
  call i32 @puts(ptr @.divmsg)
  call void @exit(i32 1)
  unreachable
ok:
  %r = srem i64 %a, %b
  ret i64 %r
}

|}

let slot r = "%r." ^ r
let label l = "bb." ^ l
let symbol name = "@fn." ^ name

let icmp (op : Ast.binop) =
  match op with
  | Ast.Eq -> "eq"
  | Ast.Ne -> "ne"
  | Ast.Lt -> "slt"
  | Ast.Le -> "sle"
  | Ast.Gt -> "sgt"
  | Ast.Ge -> "sge"
  | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div | Ast.Rem -> invalid_arg "icmp"

let registers (f : Ir.func) =
  let seen = Hashtbl.create 32 in
  let order = ref [] in
  let add r =
    if not (Hashtbl.mem seen r) then begin
      Hashtbl.replace seen r ();
      order := r :: !order
    end
  in
  List.iter add f.params;
  List.iter
    (fun (b : Ir.block) ->
      List.iter
        (fun i ->
          Option.iter add (Ir.def i);
          List.iter add (Ir.uses i))
        b.instrs;
      List.iter add (Ir.term_uses b.term))
    f.blocks;
  List.rev !order

let emit_func buf (f : Ir.func) =
  let counter = ref 0 in
  let fresh () =
    incr counter;
    Printf.sprintf "%%v%d" !counter
  in
  let line fmt =
    Printf.ksprintf
      (fun s ->
        Buffer.add_string buf "  ";
        Buffer.add_string buf s;
        Buffer.add_char buf '\n')
      fmt
  in
  let value = function
    | Ir.Imm n -> string_of_int n
    | Ir.Reg r ->
        let v = fresh () in
        line "%s = load i64, ptr %s" v (slot r);
        v
  in
  let values ops = List.rev (List.fold_left (fun acc o -> value o :: acc) [] ops) in
  let store d v = line "store i64 %s, ptr %s" v (slot d) in
  let params = String.concat ", " (List.map (fun p -> "i64 %p." ^ p) f.params) in
  Printf.bprintf buf "define i64 %s(%s) {\nprologue:\n" (symbol f.name) params;
  List.iter (fun r -> line "%s = alloca i64" (slot r)) (registers f);
  List.iter (fun p -> line "store i64 %%p.%s, ptr %s" p (slot p)) f.params;
  line "br label %%%s" (label (Cfg.entry f).label);
  List.iter
    (fun (b : Ir.block) ->
      Printf.bprintf buf "%s:\n" (label b.label);
      List.iter
        (fun (i : Ir.instr) ->
          match i with
          | Ir.Bin (d, op, a, c) ->
              let x = value a in
              let y = value c in
              let v = fresh () in
              (match op with
              | Ast.Add -> line "%s = add i64 %s, %s" v x y
              | Ast.Sub -> line "%s = sub i64 %s, %s" v x y
              | Ast.Mul -> line "%s = mul i64 %s, %s" v x y
              | Ast.Div -> line "%s = call i64 @rt.div(i64 %s, i64 %s)" v x y
              | Ast.Rem -> line "%s = call i64 @rt.rem(i64 %s, i64 %s)" v x y
              | cmp ->
                  let flag = fresh () in
                  line "%s = icmp %s i64 %s, %s" flag (icmp cmp) x y;
                  line "%s = zext i1 %s to i64" v flag);
              store d v
          | Ir.Un (d, Ast.Neg, a) ->
              let x = value a in
              let v = fresh () in
              line "%s = sub i64 0, %s" v x;
              store d v
          | Ir.Un (d, Ast.Not, a) ->
              let x = value a in
              let flag = fresh () in
              let v = fresh () in
              line "%s = icmp eq i64 %s, 0" flag x;
              line "%s = zext i1 %s to i64" v flag;
              store d v
          | Ir.Mov (d, a) -> store d (value a)
          | Ir.Call (d, g, ops) -> (
              let args = String.concat ", " (List.map (fun v -> "i64 " ^ v) (values ops)) in
              let call = Printf.sprintf "call i64 %s(%s)" (symbol g) args in
              match d with
              | Some d ->
                  let v = fresh () in
                  line "%s = %s" v call;
                  store d v
              | None -> line "%s" call)
          | Ir.Print a ->
              let x = value a in
              line "call i32 (ptr, ...) @printf(ptr @.fmt, i64 %s)" x)
        b.instrs;
      match b.term with
      | Ir.Jmp l -> line "br label %%%s" (label l)
      | Ir.Br (c, t, e) ->
          let x = value c in
          let flag = fresh () in
          line "%s = icmp ne i64 %s, 0" flag x;
          line "br i1 %s, label %%%s, label %%%s" flag (label t) (label e)
      | Ir.Ret a -> line "ret i64 %s" (value a))
    f.blocks;
  Buffer.add_string buf "}\n\n"

let program (p : Ir.program) =
  let buf = Buffer.create 8192 in
  Buffer.add_string buf prelude;
  List.iter (emit_func buf) p;
  Printf.bprintf buf "define i32 @main() {\nentry:\n  %%r = call i64 %s()\n  ret i32 0\n}\n" (symbol "main");
  Buffer.contents buf
