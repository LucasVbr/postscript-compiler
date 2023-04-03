(* Typechecking of source programs *)

open Lang

(* Environments *)
type environment = {
    localvars: (vname * tp) list; 
    funbind: fundecl list   
}

(* Recherche dans l'environement *)
type 'a option = None| Some of 'a;;

let lookup_env (name: vname) (env: environment) =
    let rec aux = function
    | [] -> None
    | (k, v)::reste ->
        if k = name then Some(v)
        else aux(reste)
    in aux(env.localvars)
;;

(* Ajoute dans l'environement *)
let rec add_env (key, valeur) env =
    let paire = (key, valeur)
    in {localvars: paire::(env.localvars); funbind: (env.funbind)}
;;

(* Enlève de l'environement *)
let rec remove_env key env =
    let aux (liste) = 
    match liste with
    | [] -> []
    | (k, v)::reste when k = key -> remove_assoc key reste
    | a::reste -> a::(remove_assoc key reste)
    in {
        localvars: (aux(env.localvars));
        funbind: (env.funbind)
    }
;;

(* Typage d'une expression *)
let rec tp_expr (env: environment) (expression: expr) =
    match expression with
    | Const(const) -> (
      match const with
      | BoolV _ -> BoolT
      | FloatV _ -> FloatT
      | IntV _ -> IntT
      | LitV _ -> LitT
      | StringV _ -> StringT
      )
    | VarE(var) -> (
      let var_value = (lookup_env var env) in
      match var_value with
      | Some(value) -> value
      | None -> raise (Failure "Unknown var")
      )
    | BinOp(op, expr1, expr2) ->
        let tp_expr1 = tp_expr env expr1
        and tp_expr2 = tp_expr env expr2 in
        if tp_expr1 = tp_expr2
        then match op with
        | BArith(barith) -> (
            match barith with
            | BAadd -> IntT
            | BAsub -> IntT
            | BAmul -> IntT
            | BAdiv -> IntT
            | BAmod ->  IntT
            | BAfadd -> FloatT 
            | BAfsub -> FloatT
            | BAfmul -> FloatT
            | BAfdiv -> FloatT
        )
        | BBool(_) -> BoolT
        | BCompar(_) -> BoolT
        else raise (Failure "Invalid type of Binary operation")
    | CondE(expr1, expr2, expr3) ->
        let tp_expr1 = tp_expr env expr1
        and tp_expr2 = tp_expr env expr2
        and tp_expr3 = tp_expr env expr3
        in if tp_expr1 = BoolT && tp_expr2 = tp_expr3
        then tp_expr2
        else raise (Failure "Invalid type of Conditionnal Expression")
    | CallE(name, list_expr) -> VoidT (* TODO *)
;;
 

let rec tp_cmd (env: environment) (cmd: com) =
    match cmd with
    | Skip -> VoidT
    | Exit -> VoidT
    | Assign(name, expr) -> VoidT (* TODO ajouter dans env *)
    | Seq(cmd1, cmd2) ->
        let tp_cmd1 = (tp_cmd env cmd1)
        and tp_cmd2 = (tp_cmd env cmd2)
        in if tp_cmd1 = Void
        then tp_cmd2
        else raise (Failure "Invalid type Sequence")
    | CondC(expr1, cmd1, cmd2) ->
        let tp_expr1 = tp_expr env expr1
        and tp_cmd1 = tp_cmd env cmd1
        and tp_cmd2 = tp_cmd env cmd2
        in
        if tp_expr1 = BoolT && tp_cmd1 = tp_cmd2
        then tp_cmd1
        else raise (Failure "Invalid type of Conditionnal Command")
    | Loop(cmd1) ->
        let _ = (tp_cmd env cmd1)
        in VoidT
    | CallC(name, list_expr) -> VoidT (* TODO Faire l'appel *)
    | Return(expr) -> (tp_expr env expr)
  
;;

let tp_prog (Prog (fundecls, fundefns)) = true