open Optimizer

let read_file path = In_channel.with_open_bin path In_channel.input_all

type options = {
  mutable level : int;
  mutable passes : string list option;
  mutable stats : bool;
  mutable positional : string list;
}

let usage () =
  prerr_string
    "usage: optc <command> [options] <file>\n\n\
     commands:\n\
    \  interp <file>     run the source-level reference interpreter\n\
    \  run <file>        compile, optimize and execute the IR\n\
    \  ir <file>         print the IR after optimization\n\
    \  llvm <file>       emit textual LLVM IR\n\
    \  bench <dir>       compare unoptimized and optimized IR for every .mini in a directory\n\
    \  passes            list the available passes\n\n\
     options:\n\
    \  -O0 | -O1         optimization level (default -O1)\n\
    \  --passes=a,b,c    run a custom pipeline instead of -O1\n\
    \  --stats           print per-pass statistics to stderr\n";
  exit 2

let parse_options args =
  let o = { level = 1; passes = None; stats = false; positional = [] } in
  List.iter
    (fun a ->
      if a = "-O0" then o.level <- 0
      else if a = "-O1" then o.level <- 1
      else if a = "--stats" then o.stats <- true
      else if String.starts_with ~prefix:"--passes=" a then
        let spec = String.sub a 9 (String.length a - 9) in
        o.passes <- Some (List.filter (fun s -> s <> "") (String.split_on_char ',' spec))
      else if String.length a > 1 && a.[0] = '-' then (
        Printf.eprintf "unknown option %s\n" a;
        usage ())
      else o.positional <- o.positional @ [ a ])
    args;
  o

let pipeline o =
  match o.passes with Some ps -> ps | None -> if o.level = 0 then [] else Pass_manager.o1

let optimize o prog =
  let result = Pass_manager.run (pipeline o) prog in
  if o.stats then Pass_manager.print_report stderr ~before:(Ir.program_size prog) result;
  result.program

let single_path o = match o.positional with [ p ] -> p | _ -> usage ()

let compile_file path =
  let ast = Parser.program_of_string (read_file path) in
  (ast, Lower.program ast)

let report (r : Semantics.result) =
  List.iter print_endline r.output;
  match r.outcome with
  | Semantics.Finished -> ()
  | Semantics.Failed msg ->
      Printf.eprintf "runtime error: %s\n" msg;
      exit 1
  | Semantics.Timeout ->
      prerr_endline "error: step limit exceeded";
      exit 1

let cmd_interp o =
  let ast, _ = compile_file (single_path o) in
  report (Interp.run ast)

let cmd_run o =
  let _, ir = compile_file (single_path o) in
  report (Ir_interp.run (optimize o ir))

let cmd_ir o =
  let _, ir = compile_file (single_path o) in
  print_string (Ir.string_of_program (optimize o ir))

let cmd_llvm o =
  let _, ir = compile_file (single_path o) in
  print_string (Llvm_emit.program (optimize o ir))

let cmd_passes () =
  List.iter (fun (p : Pass_manager.pass) -> Printf.printf "%-12s %s\n" p.name p.description) Pass_manager.registry

let cmd_bench o =
  let dir = single_path o in
  let files =
    Sys.readdir dir |> Array.to_list |> List.filter (fun f -> Filename.check_suffix f ".mini") |> List.sort compare
  in
  let passes = match o.passes with Some ps -> ps | None -> Pass_manager.o1 in
  Printf.printf "%-28s %6s %6s %7s %11s %11s %7s  %s\n" "program" "-O0" "-O1" "static" "dyn -O0" "dyn -O1"
    "dynamic" "output";
  let s0 = ref 0 and s1 = ref 0 and d0 = ref 0 and d1 = ref 0 and failures = ref 0 in
  List.iter
    (fun file ->
      let ast, ir = compile_file (Filename.concat dir file) in
      let reference = Semantics.render (Interp.run ast) in
      let opt = (Pass_manager.run passes ir).program in
      let r0 = Ir_interp.run ir in
      let r1 = Ir_interp.run opt in
      let ok = Semantics.render r0 = reference && Semantics.render r1 = reference in
      if not ok then incr failures;
      let a = Ir.program_size ir and b = Ir.program_size opt in
      s0 := !s0 + a;
      s1 := !s1 + b;
      d0 := !d0 + r0.steps;
      d1 := !d1 + r1.steps;
      Printf.printf "%-28s %6d %6d %6.1f%% %11d %11d %6.1f%%  %s\n" (Filename.chop_suffix file ".mini") a b
        (Pass_manager.percent a b) r0.steps r1.steps
        (Pass_manager.percent r0.steps r1.steps)
        (if ok then "match" else "MISMATCH"))
    files;
  Printf.printf "%s\n" (String.make 96 '-');
  Printf.printf "%-28s %6d %6d %6.1f%% %11d %11d %6.1f%%  %d/%d match\n"
    (Printf.sprintf "total (%d programs)" (List.length files))
    !s0 !s1 (Pass_manager.percent !s0 !s1) !d0 !d1 (Pass_manager.percent !d0 !d1)
    (List.length files - !failures)
    (List.length files);
  if !failures > 0 then exit 1

let () =
  match List.tl (Array.to_list Sys.argv) with
  | [] -> usage ()
  | cmd :: rest -> (
      let o = parse_options rest in
      try
        match cmd with
        | "interp" -> cmd_interp o
        | "run" -> cmd_run o
        | "ir" -> cmd_ir o
        | "llvm" -> cmd_llvm o
        | "bench" -> cmd_bench o
        | "passes" -> cmd_passes ()
        | _ -> usage ()
      with
      | Parser.Error (msg, line) ->
          Printf.eprintf "syntax error on line %d: %s\n" line msg;
          exit 1
      | Lower.Error msg ->
          Printf.eprintf "error: %s\n" msg;
          exit 1
      | Verify.Invalid msg ->
          Printf.eprintf "internal error: invalid IR after %s\n" msg;
          exit 3
      | Invalid_argument msg | Sys_error msg ->
          Printf.eprintf "error: %s\n" msg;
          exit 1)
