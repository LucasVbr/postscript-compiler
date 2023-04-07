#load "lang.cmo";;
#load "parser.cmo" ;;
#load "lexer.cmo" ;;
#load "typing.cmo";;
#load "instrs.cmo";;
#load "gen.cmo";;
#load "interf.cmo";;

open Interf;;
open Lang;;
open Instrs;;

(* For using the parser:

- Evaluate this file (use.ml)
- parse "../Tests/progsimple.c" ;;

* For code generation:

- Evaluate this file (use.ml)
- run_test "../Tests/rectangles.c" "../Tests/rectangles.ps";;
*)


