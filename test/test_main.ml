open OUnit2
open Optimizer

let read_file path = In_channel.with_open_bin path In_channel.input_all
let compile src = Lower.program (Parser.program_of_string src)
let optimize prog = (Pass_manager.run Pass_manager.o1 prog).program
let run_ir prog = Semantics.render (Ir_interp.run prog)
let main_of (prog : Ir.program) = List.find (fun (f : Ir.func) -> f.name = "main") prog
let instrs_of (f : Ir.func) = List.concat_map (fun (b : Ir.block) -> b.instrs) f.blocks
let block label instrs term = { Ir.label; instrs; term }
let func ?(params = []) name blocks = { Ir.name; params; blocks }
let only_block (f : Ir.func) = match f.blocks with [ b ] -> b | _ -> assert_failure "expected one block"
let show_instrs is = String.concat "; " (List.map Ir.string_of_instr is)
let assert_instrs expected actual = assert_equal ~printer:show_instrs expected actual

let parse_expr_test src expected =
  src >:: fun _ -> assert_equal expected (Parser.expr_of_string src)

let parser_tests =
  let open Ast in
  "parser"
  >::: [
         parse_expr_test "1 + 2 * 3 - 4" (Binary (Sub, Binary (Add, Int 1, Binary (Mul, Int 2, Int 3)), Int 4));
         parse_expr_test "a || b && !c" (Or (Var "a", And (Var "b", Unary (Not, Var "c"))));
         parse_expr_test "-x * 2 < y == 1"
           (Binary (Eq, Binary (Lt, Binary (Mul, Unary (Neg, Var "x"), Int 2), Var "y"), Int 1));
         parse_expr_test "f(1, g(2), x)" (Call ("f", [ Int 1; Call ("g", [ Int 2 ]); Var "x" ]));
         parse_expr_test "10 % 3 / 2" (Binary (Div, Binary (Rem, Int 10, Int 3), Int 2));
         ( "comments and else-if" >:: fun _ ->
           let src = "// header\nfn main() {\n  if (1) { print(1); } else if (0) { print(2); } // trailing\n}" in
           match Parser.program_of_string src with
           | [ { body = [ If (Int 1, [ Print (Int 1) ], [ If (Int 0, [ Print (Int 2) ], []) ]) ]; _ } ] -> ()
           | _ -> assert_failure "unexpected parse" );
         ( "error reports line" >:: fun _ ->
           match Parser.program_of_string "fn main() {\n  print(1)\n}" with
           | _ -> assert_failure "expected a syntax error"
           | exception Parser.Error (_, line) -> assert_equal ~printer:string_of_int 3 line );
         ( "bad character" >:: fun _ ->
           match Parser.program_of_string "fn main() { let x = 1 # 2; }" with
           | _ -> assert_failure "expected a lexer error"
           | exception Parser.Error (_, 1) -> () );
       ]

let rejects name src =
  name >:: fun _ ->
  match compile src with _ -> assert_failure "expected a compile error" | exception Lower.Error _ -> ()

let lowering_tests =
  "lowering"
  >::: [
         rejects "unbound variable" "fn main() { print(x); }";
         rejects "unknown function" "fn main() { print(f(1)); }";
         rejects "arity mismatch" "fn f(a, b) { return a; } fn main() { print(f(1)); }";
         rejects "break outside loop" "fn main() { break; }";
         rejects "continue outside loop" "fn main() { if (1) { continue; } }";
         rejects "missing main" "fn f() { return 1; }";
         rejects "main with parameters" "fn main(x) { print(x); }";
         rejects "duplicate function" "fn main() { } fn main() { }";
         rejects "duplicate parameter" "fn f(a, a) { return a; } fn main() { }";
         rejects "out of scope" "fn main() { if (1) { let y = 2; } print(y); }";
         ( "shadowed variables get distinct registers" >:: fun _ ->
           let prog = compile "fn main() { let x = 1; if (1) { let x = 2; print(x); } print(x); }" in
           let defs = List.filter_map Ir.def (instrs_of (main_of prog)) in
           assert_bool "x" (List.mem "x" defs);
           assert_bool "x.1" (List.mem "x.1" defs) );
         ( "lowered IR verifies" >:: fun _ ->
           Verify.program (compile "fn f(n) { while (n > 0 && n != 5) { n = n - 1; } return n; } fn main() { print(f(9)); }") );
       ]

let constprop_tests =
  "constprop"
  >::: [
         ( "folds straight-line arithmetic" >:: fun _ ->
           let prog = optimize (compile "fn main() { let a = 2; let b = a * 3 + 1; print(b); }") in
           assert_instrs [ Ir.Print (Ir.Imm 7) ] (only_block (main_of prog)).instrs );
         ( "folds a known branch" >:: fun _ ->
           let prog = optimize (compile "fn main() { let x = 5; if (x > 3) { print(1); } else { print(2); } }") in
           let b = only_block (main_of prog) in
           assert_instrs [ Ir.Print (Ir.Imm 1) ] b.instrs );
         ( "does not fold a loop-carried value" >:: fun _ ->
           let prog = optimize (compile "fn main() { let i = 0; while (i < 3) { i = i + 1; } print(i); }") in
           assert_bool "loop kept"
             (List.exists (fun (b : Ir.block) -> match b.term with Ir.Br _ -> true | _ -> false) (main_of prog).blocks);
           assert_equal "3\n" (run_ir prog) );
         ( "proves values through infeasible edges" >:: fun _ ->
           let src =
             "fn main() { let x = 1; let i = 0; while (i < 10) { if (x == 1) { x = 1; } else { x = 2; } i = i + 1; } print(x); }"
           in
           let prog = optimize (compile src) in
           assert_bool "print 1"
             (List.mem (Ir.Print (Ir.Imm 1)) (instrs_of (main_of prog))) );
         ( "never folds division by zero" >:: fun _ ->
           let f = func "main" [ block "entry" [ Ir.Bin ("a", Ast.Div, Ir.Imm 1, Ir.Imm 0) ] (Ir.Ret (Ir.Reg "a")) ] in
           assert_instrs [ Ir.Bin ("a", Ast.Div, Ir.Imm 1, Ir.Imm 0) ] (only_block (Constprop.run f)).instrs );
         ( "parameters are unknown" >:: fun _ ->
           let f =
             func ~params:[ "p" ] "f"
               [ block "entry" [ Ir.Bin ("a", Ast.Add, Ir.Reg "p", Ir.Imm 1) ] (Ir.Ret (Ir.Reg "a")) ]
           in
           assert_equal f (Constprop.run f) );
       ]

let copyprop_tests =
  "copyprop"
  >::: [
         ( "forwards a copy" >:: fun _ ->
           let f =
             func ~params:[ "p" ] "f"
               [
                 block "entry"
                   [ Ir.Mov ("a", Ir.Reg "p"); Ir.Bin ("b", Ast.Add, Ir.Reg "a", Ir.Imm 1) ]
                   (Ir.Ret (Ir.Reg "a"));
               ]
           in
           let b = only_block (Copyprop.run f) in
           assert_instrs [ Ir.Mov ("a", Ir.Reg "p"); Ir.Bin ("b", Ast.Add, Ir.Reg "p", Ir.Imm 1) ] b.instrs;
           assert_equal (Ir.Ret (Ir.Reg "p")) b.term );
         ( "redefinition kills the copy" >:: fun _ ->
           let instrs =
             [ Ir.Mov ("a", Ir.Reg "p"); Ir.Bin ("p", Ast.Add, Ir.Reg "p", Ir.Imm 1); Ir.Print (Ir.Reg "a") ]
           in
           let f = func ~params:[ "p" ] "f" [ block "entry" instrs (Ir.Ret (Ir.Imm 0)) ] in
           assert_instrs instrs (only_block (Copyprop.run f)).instrs );
         ( "copies must agree at joins" >:: fun _ ->
           let f =
             func ~params:[ "p"; "q" ] "f"
               [
                 block "entry" [] (Ir.Br (Ir.Reg "p", "l", "r"));
                 block "l" [ Ir.Mov ("a", Ir.Reg "p") ] (Ir.Jmp "join");
                 block "r" [ Ir.Mov ("a", Ir.Reg "q") ] (Ir.Jmp "join");
                 block "join" [ Ir.Print (Ir.Reg "a") ] (Ir.Ret (Ir.Imm 0));
               ]
           in
           let out = Copyprop.run f in
           let join = List.find (fun (b : Ir.block) -> b.label = "join") out.blocks in
           assert_instrs [ Ir.Print (Ir.Reg "a") ] join.instrs );
       ]

let combine instrs = (Instcombine.run_block (block "entry" instrs (Ir.Ret (Ir.Imm 0)))).instrs

let instcombine_tests =
  "instcombine"
  >::: [
         ( "additive identity" >:: fun _ ->
           assert_instrs [ Ir.Mov ("a", Ir.Reg "x") ] (combine [ Ir.Bin ("a", Ast.Add, Ir.Reg "x", Ir.Imm 0) ]) );
         ( "multiply by zero" >:: fun _ ->
           assert_instrs [ Ir.Mov ("a", Ir.Imm 0) ] (combine [ Ir.Bin ("a", Ast.Mul, Ir.Imm 0, Ir.Reg "x") ]) );
         ( "x - x" >:: fun _ ->
           assert_instrs [ Ir.Mov ("a", Ir.Imm 0) ] (combine [ Ir.Bin ("a", Ast.Sub, Ir.Reg "x", Ir.Reg "x") ]) );
         ( "subtract constant becomes add" >:: fun _ ->
           assert_instrs
             [ Ir.Bin ("a", Ast.Add, Ir.Reg "x", Ir.Imm (-5)) ]
             (combine [ Ir.Bin ("a", Ast.Sub, Ir.Reg "x", Ir.Imm 5) ]) );
         ( "constants move right" >:: fun _ ->
           assert_instrs
             [ Ir.Bin ("a", Ast.Mul, Ir.Reg "x", Ir.Imm 3); Ir.Bin ("b", Ast.Gt, Ir.Reg "x", Ir.Imm 3) ]
             (combine [ Ir.Bin ("a", Ast.Mul, Ir.Imm 3, Ir.Reg "x"); Ir.Bin ("b", Ast.Lt, Ir.Imm 3, Ir.Reg "x") ]) );
         ( "reassociates constants" >:: fun _ ->
           assert_instrs
             [ Ir.Bin ("a", Ast.Add, Ir.Reg "x", Ir.Imm 1); Ir.Bin ("b", Ast.Add, Ir.Reg "x", Ir.Imm 3) ]
             (combine [ Ir.Bin ("a", Ast.Add, Ir.Reg "x", Ir.Imm 1); Ir.Bin ("b", Ast.Add, Ir.Reg "a", Ir.Imm 2) ]) );
         ( "reassociation respects redefinition" >:: fun _ ->
           let instrs =
             [
               Ir.Bin ("a", Ast.Add, Ir.Reg "x", Ir.Imm 1);
               Ir.Mov ("x", Ir.Imm 7);
               Ir.Bin ("b", Ast.Add, Ir.Reg "a", Ir.Imm 2);
             ]
           in
           assert_instrs instrs (combine instrs) );
         ( "not of a comparison" >:: fun _ ->
           assert_instrs
             [ Ir.Bin ("c", Ast.Lt, Ir.Reg "x", Ir.Reg "y"); Ir.Bin ("d", Ast.Ge, Ir.Reg "x", Ir.Reg "y") ]
             (combine [ Ir.Bin ("c", Ast.Lt, Ir.Reg "x", Ir.Reg "y"); Ir.Un ("d", Ast.Not, Ir.Reg "c") ]) );
         ( "branch on not swaps targets" >:: fun _ ->
           let b = block "entry" [ Ir.Un ("c", Ast.Not, Ir.Reg "x") ] (Ir.Br (Ir.Reg "c", "t", "f")) in
           assert_equal (Ir.Br (Ir.Reg "x", "f", "t")) (Instcombine.run_block b).term );
         ( "branch on ne zero" >:: fun _ ->
           let b = block "entry" [ Ir.Bin ("c", Ast.Ne, Ir.Reg "x", Ir.Imm 0) ] (Ir.Br (Ir.Reg "c", "t", "f")) in
           assert_equal (Ir.Br (Ir.Reg "x", "t", "f")) (Instcombine.run_block b).term );
       ]

let dce_tests =
  "dce"
  >::: [
         ( "removes unused chains" >:: fun _ ->
           let f =
             func ~params:[ "p" ] "f"
               [
                 block "entry"
                   [ Ir.Bin ("a", Ast.Add, Ir.Reg "p", Ir.Imm 1); Ir.Bin ("b", Ast.Mul, Ir.Reg "a", Ir.Imm 2) ]
                   (Ir.Ret (Ir.Reg "p"));
               ]
           in
           assert_instrs [] (only_block (Dce.run f)).instrs );
         ( "keeps divisions that may trap" >:: fun _ ->
           let instrs =
             [ Ir.Bin ("a", Ast.Div, Ir.Reg "p", Ir.Reg "q"); Ir.Bin ("b", Ast.Div, Ir.Reg "p", Ir.Imm 2) ]
           in
           let f = func ~params:[ "p"; "q" ] "f" [ block "entry" instrs (Ir.Ret (Ir.Imm 0)) ] in
           assert_instrs [ Ir.Bin ("a", Ast.Div, Ir.Reg "p", Ir.Reg "q") ] (only_block (Dce.run f)).instrs );
         ( "drops unused call results but keeps the call" >:: fun _ ->
           let f =
             func "main"
               [ block "entry" [ Ir.Call (Some "r", "g", []) ] (Ir.Ret (Ir.Imm 0)) ]
           in
           assert_instrs [ Ir.Call (None, "g", []) ] (only_block (Dce.run f)).instrs );
         ( "removes dead loop-carried counters" >:: fun _ ->
           let src =
             "fn main() { let j = 0; let i = 0; while (i < 10) { j = j + 1; i = i + 1; } print(i); }"
           in
           let prog = optimize (compile src) in
           assert_bool "j eliminated"
             (not (List.exists (fun i -> Ir.def i = Some "j" || List.mem "j" (Ir.uses i)) (instrs_of (main_of prog))));
           assert_equal "10\n" (run_ir prog) );
         ( "preserves runtime errors" >:: fun _ ->
           let prog = optimize (compile "fn main() { let x = 1 / 0; print(5); }") in
           assert_equal "runtime error: division by zero\n" (run_ir prog) );
       ]

let simplifycfg_tests =
  "simplifycfg"
  >::: [
         ( "drops unreachable blocks" >:: fun _ ->
           let f =
             func "main"
               [ block "entry" [] (Ir.Ret (Ir.Imm 0)); block "dead" [ Ir.Print (Ir.Imm 1) ] (Ir.Ret (Ir.Imm 1)) ]
           in
           assert_equal 1 (List.length (Simplifycfg.run f).blocks) );
         ( "threads empty blocks" >:: fun _ ->
           let f =
             func ~params:[ "p" ] "f"
               [
                 block "entry" [] (Ir.Br (Ir.Reg "p", "a", "b"));
                 block "a" [] (Ir.Jmp "c");
                 block "b" [ Ir.Print (Ir.Imm 1) ] (Ir.Jmp "c");
                 block "c" [] (Ir.Ret (Ir.Imm 0));
               ]
           in
           let out = Simplifycfg.run f in
           assert_equal [ "entry"; "b"; "c" ] (List.map (fun (b : Ir.block) -> b.label) out.blocks);
           assert_equal (Ir.Br (Ir.Reg "p", "c", "b")) (Cfg.entry out).term );
         ( "merges straight-line chains" >:: fun _ ->
           let f =
             func "main"
               [
                 block "entry" [ Ir.Print (Ir.Imm 1) ] (Ir.Jmp "x");
                 block "x" [ Ir.Print (Ir.Imm 2) ] (Ir.Jmp "y");
                 block "y" [ Ir.Print (Ir.Imm 3) ] (Ir.Ret (Ir.Imm 0));
               ]
           in
           assert_instrs
             [ Ir.Print (Ir.Imm 1); Ir.Print (Ir.Imm 2); Ir.Print (Ir.Imm 3) ]
             (only_block (Simplifycfg.run f)).instrs );
         ( "folds constant branches" >:: fun _ ->
           let f =
             func "main"
               [
                 block "entry" [] (Ir.Br (Ir.Imm 0, "t", "e"));
                 block "t" [ Ir.Print (Ir.Imm 1) ] (Ir.Ret (Ir.Imm 0));
                 block "e" [ Ir.Print (Ir.Imm 2) ] (Ir.Ret (Ir.Imm 0));
               ]
           in
           assert_instrs [ Ir.Print (Ir.Imm 2) ] (only_block (Simplifycfg.run f)).instrs );
         ( "keeps infinite loops intact" >:: fun _ ->
           let f =
             func "main"
               [ block "entry" [] (Ir.Jmp "a"); block "a" [] (Ir.Jmp "b"); block "b" [] (Ir.Jmp "a") ]
           in
           Verify.func (Simplifycfg.run f) );
       ]

let invalid name f =
  name >:: fun _ ->
  match Verify.program [ f ] with _ -> assert_failure "expected verifier error" | exception Verify.Invalid _ -> ()

let verify_tests =
  "verify"
  >::: [
         invalid "undefined register" (func "main" [ block "entry" [ Ir.Print (Ir.Reg "nope") ] (Ir.Ret (Ir.Imm 0)) ]);
         invalid "unknown label" (func "main" [ block "entry" [] (Ir.Jmp "missing") ]);
         invalid "branch to entry"
           (func "main" [ block "entry" [] (Ir.Jmp "a"); block "a" [] (Ir.Jmp "entry") ]);
         invalid "duplicate label"
           (func "main" [ block "entry" [] (Ir.Jmp "a"); block "a" [] (Ir.Jmp "a"); block "a" [] (Ir.Ret (Ir.Imm 0)) ]);
         invalid "call arity" (func "main" [ block "entry" [ Ir.Call (None, "main", [ Ir.Imm 1 ]) ] (Ir.Ret (Ir.Imm 0)) ]);
       ]

let pipeline_tests =
  "pipeline"
  >::: [
         ( "reaches a fixpoint" >:: fun _ ->
           let prog = optimize (compile (read_file "programs/24_perfect_numbers.mini")) in
           assert_equal prog (optimize prog) );
         ( "unknown pass is rejected" >:: fun _ ->
           assert_raises (Invalid_argument "unknown pass 'gvn' (available: simplifycfg, constprop, copyprop, instcombine, dce)")
             (fun () -> Pass_manager.run [ "gvn" ] []) );
         ( "reports removed instructions" >:: fun _ ->
           let prog = compile "fn main() { let a = 1 + 2; print(a); }" in
           let r = Pass_manager.run Pass_manager.o1 prog in
           let removed = List.fold_left (fun acc (s : Pass_manager.report) -> acc + s.removed) 0 r.reports in
           assert_equal ~printer:string_of_int (Ir.program_size prog - Ir.program_size r.program) removed );
       ]

let llvm_tests =
  "llvm"
  >::: [
         ( "emits a module" >:: fun _ ->
           let ll = Llvm_emit.program (optimize (compile "fn sq(x) { return x * x; } fn main() { print(sq(7) / 2); }")) in
           let contains s =
             let n = String.length s in
             let rec go i = i + n <= String.length ll && (String.sub ll i n = s || go (i + 1)) in
             assert_bool s (go 0)
           in
           contains "define i64 @fn.sq(i64 %p.x)";
           contains "define i64 @fn.main()";
           contains "define i32 @main()";
           contains "call i64 @rt.div";
           contains "@printf" );
       ]

let program_dir = "programs"

let programs () =
  Sys.readdir program_dir |> Array.to_list
  |> List.filter (fun f -> Filename.check_suffix f ".mini")
  |> List.sort compare

let differential file =
  let path = Filename.concat program_dir file in
  let src = read_file path in
  let expected = read_file (Filename.chop_suffix path ".mini" ^ ".expected") in
  let check actual = assert_equal ~printer:Fun.id expected actual in
  let single pass = pass >:: fun _ -> check (run_ir (Pass_manager.run [ pass ] (compile src)).program) in
  Filename.chop_suffix file ".mini"
  >::: [
         ("reference" >:: fun _ -> check (Semantics.render (Interp.run (Parser.program_of_string src))));
         ( "O0" >:: fun _ ->
           let prog = compile src in
           Verify.program prog;
           check (run_ir prog) );
         ( "O1" >:: fun _ ->
           let prog = compile src in
           let opt = optimize prog in
           check (run_ir opt);
           assert_bool "optimizer grew the program" (Ir.program_size opt <= Ir.program_size prog) );
       ]
       @ List.map single Pass_manager.names

let () =
  run_test_tt_main
    ("compiler-optimizer"
    >::: [
           parser_tests;
           lowering_tests;
           constprop_tests;
           copyprop_tests;
           instcombine_tests;
           dce_tests;
           simplifycfg_tests;
           verify_tests;
           pipeline_tests;
           llvm_tests;
           "programs" >::: List.map differential (programs ());
         ])
