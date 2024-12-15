open Lexer.Lex
open Defs.Fun
open Core.Parser
open Codegen
open Lexer.Formatter

let () =
  let dir = "./lib/fun_programs" in
  let files = Sys.readdir dir in

  Array.iter (fun file ->
    if Filename.check_suffix file ".fun" then begin
      try
        Printf.printf "BEGIN COMPILATION OF %s\n" file;
        let filepath = Filename.concat dir file in

        let ic = open_in filepath in
        let n = in_channel_length ic in
        let s = really_input_string ic n in
        close_in ic;

        (* Parse the input to get the AST *)
        let prog = lexing_simp s fun_toks in
        print_endline (display_toks prog);
        let asts = _prog prog in
        let ast = 
          match List.find_opt (fun (_, rest) -> rest = []) asts with
          | Some (a, _) -> a
          | None -> failwith "Error: No valid AST found with an empty rest" in

        (* Generate LLVM IR code *)
        let class_name = Filename.remove_extension file in
        let test_dir = "out" in
        let ll_file = Printf.sprintf "%s/%s.ll" test_dir class_name in

        (* Generate LLVM IR using your compiler function *)
        let code = compile ast in

        (* Write the LLVM IR code to a file *)
        let oc = open_out ll_file in
        output_string oc code;
        close_out oc;

        (* Run the LLVM IR file using the lli interpreter *)
        let cmd_lli = Printf.sprintf "lli %s" ll_file in
        let exit_code = Sys.command cmd_lli in

        if exit_code <> 0 then
          Printf.printf "Error: lli failed to execute %s\n" ll_file
      with
        | Failure msg -> Printf.printf "Caught failure: %s\n" msg
        | _ -> Printf.printf "An unknown error occurred\n"
    end
  ) files