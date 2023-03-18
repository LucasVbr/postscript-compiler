(* Typechecking of source programs *)

open Lang

(* Environments *)

type environment = 
    {localvars: (vname * tp) list; 
     funbind: fundecl list   
    }


let tp_prog (Prog (fundecls, fundefns)) = true