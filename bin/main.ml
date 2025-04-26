open Smol

let () =
  Printexc.record_backtrace true;
  if Array.length Sys.argv < 2
  then print_endline "Usage: program <filename>"
  else (
    let filename = Sys.argv.(1) in
    try
      let lexbuf = Lexing.from_channel (In_channel.open_text filename) in
      let ast = Parser.program Lexer.tokenize lexbuf in
      (* Format.printf "%a@." Tast.pp_texpr texpr; *)
      let _, texpr = Infer.type_infer ast in
      (* Format.printf "%a@." Tast.pp_texpr texpr; *)
      let value = Eval.eval [] texpr in
      print_endline (Value.string_of_value value)
    with
    | Sys_error err -> Printf.printf "Error: %s\n" err
    | exn ->
      Printf.printf
        "Unhandled exception: %s\n%s\n"
        (Printexc.to_string exn)
        (Printexc.get_backtrace ()))
;;
