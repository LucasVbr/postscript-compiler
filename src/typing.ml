(* Typechecking of source programs *)

open Lang

(* Environments *)
type environment = {
    localvars: (vname * tp) list; 
    funbind: fundecl list   
}

let find_var (var: vname) (env: environment) =
    let rec aux local_vars = 
        match local_vars with
        | [] -> failwith "Variable inconnue"
        | tete::reste -> 
            let (name, _) = tete in
            if name = var
            then tete
            else aux reste
    in aux (env.localvars)
;;

(* let rec tp_expr (expression: expr) (env: environment) =
    match expression with
    | Const(const) -> 
        match const with
        | BoolV(_) -> BoolT
        | FloatV(_) -> FloatT
        | IntV(_) -> IntT
        | LitV(_) -> LitT
        | StringV(_) -> StringT
    | VarE(var) -> tp_expr (find_var var env) env
    | BinOp(op, expr1, expr2) -> 
    | CondE(expr1, expr2, expr3) -> 
    | CallE(functionName, exprList) ->  *)
;;

let tp_prog (Prog (fundecls, fundefns)) = true