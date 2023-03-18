(* Compilation functions *)

open Lang
open Instrs
open Typing

(* ************************************************************ *)
(* **** Compilation of expressions / statements            **** *)
(* ************************************************************ *)

  
let gen_prog (Prog (fundecls, fundefns)) =
  ISeq []
    
  
