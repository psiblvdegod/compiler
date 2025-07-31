(* psiblvdegod, 2025, under MIT License *)

(* console application for 'compiler' library *)
(* takes two arguments: source path and destination path *)
(* compiles contents of the source and writes it to the destination *)
let () =
  if Array.length Sys.argv <> 3 then (
    Printf.eprintf "Error: 2 arguments expected\n";
    exit 1)
  else
    let input_channel = Sys.argv.(1) |> open_in in
    let source =
      in_channel_length input_channel |> really_input_string input_channel
    in
    let output_channel = Sys.argv.(2) |> open_out in
    match Compiler.App.compile source with
    | Error err -> Printf.eprintf "%s\n" err
    | Ok result ->
        output_string output_channel result;
        close_in input_channel;
        close_out output_channel
