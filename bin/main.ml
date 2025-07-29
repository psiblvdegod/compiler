let () =
  if Array.length Sys.argv != 3 then
    Sys.command "echo \"Error: 2 arguments expected\"" |> ignore
  else
    let input_channel = Sys.argv.(1) |> open_in in
    let source =
      in_channel_length input_channel |> really_input_string input_channel
    in
    let output_channel = Sys.argv.(2) |> open_out in
    match Compiler.App.compile source with
    | Error err -> Sys.command (Printf.sprintf "echo %s" err) |> ignore
    | Ok result ->
        output_string output_channel result;
        close_in input_channel;
        close_out output_channel
