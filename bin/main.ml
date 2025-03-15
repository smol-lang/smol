open Smol
open Ast

let () =
  Printexc.record_backtrace true;
  if Array.length Sys.argv < 2
  then print_endline "Usage: program <filename>"
  else (
    let filename = Sys.argv.(1) in
    try
      let lexbuf = Lexing.from_channel (In_channel.open_text filename) in
      let parsed_ast = Parser.program Lexer.tokenize lexbuf in
      Format.printf "%s\n" (show_expr parsed_ast)
    with
    | Sys_error err -> Printf.printf "Error: %s\n" err
    | exn ->
      Printf.printf
        "Unhandled exception: %s\n%s\n"
        (Printexc.to_string exn)
        (Printexc.get_backtrace ()))
;;
