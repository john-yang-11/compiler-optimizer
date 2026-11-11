let () =
  print_string "let all = [\n";
  Array.iteri
    (fun i path ->
      if i > 0 then
        Printf.printf "  (%S, %S);\n"
          (Filename.chop_suffix (Filename.basename path) ".mini")
          (In_channel.with_open_bin path In_channel.input_all))
    Sys.argv;
  print_string "]\n"
