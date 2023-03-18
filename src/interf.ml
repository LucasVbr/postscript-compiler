(* Interface with parser *)

exception ParseLexError of exn * (string * int * int * string * string)

let parse_file infile = 
  let lexbuf = Lexing.from_channel (open_in infile) in
  try 
    Parser.start Lexer.token lexbuf
  with exn ->
    begin
      let curr = lexbuf.Lexing.lex_curr_p in
      let line = curr.Lexing.pos_lnum in
      let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
      let tok = Lexing.lexeme lexbuf in
      let tail = Lexer.ruleTail "" lexbuf in
      raise (ParseLexError (exn,(infile, line,cnum,tok,tail)))
    end
;;

let print_parse_error (filename, line,cnum,tok,tail) =
  print_string ("Parsing error in file: " ^ filename ^ 
                      " on line: " ^ (string_of_int line) ^ 
                      " column: " ^ (string_of_int cnum) ^
                      " token: "  ^ tok ^
                      "\nrest: "  ^ tail ^ "\n")
;;

let parse infile = 
  try parse_file infile
  with ParseLexError (e, r) -> 
    print_parse_error r;
    failwith "Stopped execution."
;;

let run_test infile outfile =
  let inprog = parse infile in
  if Typing.tp_prog inprog
  then
    (let genprog = Gen.gen_prog inprog in
     let outf = open_out outfile in
     let instrs = Instrs.string_of_instr genprog in
     output_string outf instrs;
     close_out outf;
     print_string ("generated " ^ outfile ^ "\n")
    )
  else
     print_string ("compilation aborted because of typing error")


    
