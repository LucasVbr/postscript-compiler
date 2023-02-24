(* Main function and target of compilation in Makefile *)

let main () =
  let help_message = "Run with:\n comp <input_filename> <output_filename>\n" in  
  if (Array.length (Sys.argv)) != 3
  then print_string help_message
  else Interf.run_test Sys.argv.(1) Sys.argv.(2)
;;

main();;

