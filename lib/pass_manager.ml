type pass = { name : string; description : string; run : Ir.func -> Ir.func }

let registry =
  [
    {
      name = "simplifycfg";
      description = "fold constant branches, thread empty blocks, drop unreachable code, merge straight-line blocks";
      run = Simplifycfg.run;
    };
    {
      name = "constprop";
      description = "sparse conditional constant propagation over the CFG";
      run = Constprop.run;
    };
    { name = "copyprop"; description = "global copy propagation using available-copies analysis"; run = Copyprop.run };
    {
      name = "instcombine";
      description = "algebraic identities, constant reassociation, compare and branch canonicalization";
      run = Instcombine.run;
    };
    { name = "dce"; description = "liveness-based dead instruction elimination"; run = Dce.run };
  ]

let names = List.map (fun p -> p.name) registry

let find name =
  match List.find_opt (fun p -> p.name = name) registry with
  | Some p -> p
  | None -> invalid_arg (Printf.sprintf "unknown pass '%s' (available: %s)" name (String.concat ", " names))

let o1 = [ "simplifycfg"; "constprop"; "copyprop"; "instcombine"; "dce"; "simplifycfg" ]

type report = { pass_name : string; mutable invocations : int; mutable removed : int }
type result = { program : Ir.program; reports : report list; rounds : int }

let dedup xs = List.fold_left (fun acc x -> if List.mem x acc then acc else acc @ [ x ]) [] xs

let run ?(verify = true) ?(max_rounds = 16) pipeline (prog : Ir.program) =
  let passes = List.map find pipeline in
  let reports = List.map (fun n -> { pass_name = n; invocations = 0; removed = 0 }) (dedup pipeline) in
  let report_for n = List.find (fun r -> r.pass_name = n) reports in
  let apply prog (p : pass) =
    let before = Ir.program_size prog in
    let prog' = List.map p.run prog in
    if verify then (try Verify.program prog' with Verify.Invalid msg -> raise (Verify.Invalid (p.name ^ ": " ^ msg)));
    let r = report_for p.name in
    r.invocations <- r.invocations + 1;
    r.removed <- r.removed + (before - Ir.program_size prog');
    prog'
  in
  let rec loop prog round =
    let prog' = List.fold_left apply prog passes in
    if prog' = prog || round >= max_rounds then (prog', round) else loop prog' (round + 1)
  in
  let program, rounds = loop prog 1 in
  { program; reports; rounds }

let percent before after =
  if before = 0 then 0.0 else 100.0 *. float_of_int (before - after) /. float_of_int before

let print_report oc ~before r =
  let after = Ir.program_size r.program in
  Printf.fprintf oc "pipeline reached a fixpoint after %d round(s)\n" r.rounds;
  List.iter
    (fun s -> Printf.fprintf oc "  %-12s runs=%-3d removed=%d\n" s.pass_name s.invocations s.removed)
    r.reports;
  Printf.fprintf oc "  instructions: %d -> %d (%.1f%% fewer)\n" before after (percent before after)
