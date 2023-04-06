(* Typechecking of source programs *)

open Lang;;

(* Environments *)
type environment = {
    localvars: (vname * tp) list; 
    funbind: fundecl list   
};;

(* Recherche dans l'environement *)
type 'a option = None| Some of 'a;;

let findEnv_var (name: vname) (env: environment) =
    let rec aux = function
    | [] -> None
    | (k, v)::reste ->
        if k = name then Some(v)
        else aux(reste)
    in aux(env.localvars)
;;
(* val findEnv_var :
   Lang.vname -> environment
   -> Lang.tp option = <fun>
*)

let findEnv_fun (name: fname) (env: environment) =
    let rec aux = function
    | [] -> None
    | Fundecl(tp, fname, vars)::reste ->
        let f = (tp, fname, vars) in
        if fname = name then Some(f)
        else aux(reste)
    in aux(env.funbind)
;;
(* val findEnv_fun :
  Lang.fname -> environment
  -> (Lang.tp * Lang.fname * Lang.vardecl list) option = <fun>
*)

(* Ajout dans l'environnement *)
let addEnv_var (var_decl: vardecl) (env: environment) =
    let Vardecl(var_decl_tp, var_decl_name) = var_decl
    in {
    localvars=(var_decl_name, var_decl_tp)::(env.localvars);
    funbind=env.funbind
};;
(* val addEnv_var :
   Lang.vardecl -> environment
   -> environment = <fun>
*)

let addEnv_fun (fun_decl: fundecl) (env: environment) = {
    localvars=(env.localvars);
    funbind=(fun_decl::(env.funbind))
};;
(* val addEnv_fun :
   Lang.fundecl -> environment
   -> environment = <fun>
*)

(* Typage d'une declaration de variable *)
let tp_vardecl (vdecl: vardecl) = 
    let Vardecl(vdecl_tp, _) = vdecl
    in vdecl_tp
;;
(* val tp_vardecl : Lang.vardecl -> Lang.tp = <fun> *)

(* ----- Typage d'une expression ----- *)
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
      let var_value = (findEnv_var var env) in
      match var_value with
      | Some(value) -> value
      | None -> raise (Failure "Unknown var")
      )
    | BinOp(op, expr1, expr2) ->
        let tp_expr1 = tp_expr env expr1
        and tp_expr2 = tp_expr env expr2 in
        if tp_expr1 = tp_expr2
        then (
            match op with
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
            | BBool(_) ->
                if tp_expr1 = BoolT && tp_expr2 = BoolT
                then BoolT
                else raise (Failure "Invalid Boolean operation")
            | BCompar(_) -> BoolT
        )
        else raise (Failure "Invalid type of Binary operation")
    | CondE(expr1, expr2, expr3) ->
        let tp_expr1 = tp_expr env expr1
        and tp_expr2 = tp_expr env expr2
        and tp_expr3 = tp_expr env expr3
        in if tp_expr1 = BoolT && tp_expr2 = tp_expr3
        then tp_expr2
        else raise (Failure "Invalid type of Conditionnal Expression")
    | CallE(name, list_expr) -> 
        match (findEnv_fun name env) with
        | Some(func) -> (
            let (func_tp, func_name, func_list_vardecl) = func in
            if (List.map (tp_expr env) list_expr) = (List.map tp_vardecl func_list_vardecl)
            then func_tp
            else raise (Failure "Invalid arguments")
        )
        | None -> raise (Failure "Unknown function in the environment")
;;
(* val tp_expr :
   environment -> Lang.expr
   -> Lang.tp = <fun>
*)

(* - tp_expr: TESTS - *)
let test_tp_expr =
    let function_to_test = tp_expr {
        localvars=[
            ("i", IntT);
            ("f", FloatT);
            ("b", BoolT);
            ("l", LitT);
            ("s", StringT);
        ];
        funbind=[
            Fundecl(BoolT, "fun1", [Vardecl(IntT, "a"); Vardecl(FloatT, "b")])
        ]
    }
    and input_values = [
        Const(BoolV(true));
        Const(FloatV(10.98));
        Const(IntV(10));
        Const(LitV("l"));
        Const(StringV("Hello"));

        VarE("b");
        VarE("f");
        VarE("i");
        VarE("l");
        VarE("s");
        
        BinOp(BArith(BAadd), Const(IntV(5)), VarE("i"));
        BinOp(BArith(BAsub), Const(IntV(5)), VarE("i"));
        BinOp(BArith(BAmul), Const(IntV(5)), VarE("i"));
        BinOp(BArith(BAdiv), Const(IntV(5)), VarE("i"));
        BinOp(BArith(BAmod), Const(IntV(5)), VarE("i"));

        BinOp(BArith(BAfadd), Const(FloatV(5.)), VarE("f"));
        BinOp(BArith(BAfsub), Const(FloatV(5.)), VarE("f"));
        BinOp(BArith(BAfmul), Const(FloatV(5.)), VarE("f"));
        BinOp(BArith(BAfdiv), Const(FloatV(5.)), VarE("f"));
        
        BinOp(BCompar(BCeq), VarE("i"), VarE("i"));
        BinOp(BCompar(BCge), VarE("i"), VarE("i"));
        BinOp(BCompar(BCgt), VarE("i"), VarE("i"));
        BinOp(BCompar(BCle), VarE("i"), VarE("i"));
        BinOp(BCompar(BClt), VarE("i"), VarE("i"));
        BinOp(BCompar(BCne), VarE("i"), VarE("i"));

        BinOp(BBool(BBand), VarE("b"), VarE("b"));
        BinOp(BBool(BBor), VarE("b"), VarE("b"));

        (* (4 >= 5) ? 7 : 8 -> IntT *)
        CondE(
            BinOp(BCompar(BCge), Const(IntV(4)), Const(IntV(5))),
            Const(IntV(7)),
            Const(IntV(8))
        );

        CallE("fun1", [VarE("i"); VarE("f")]);
        (* CallE("fun2", [VarE("i"); VarE("f")]); *)
        (* CallE("fun1", [VarE("b"); VarE("f")]); *)
    ] and expected_result = [
        BoolT; FloatT; IntT; LitT; StringT;
        BoolT; FloatT; IntT; LitT; StringT;
        IntT; IntT; IntT; IntT; IntT;
        FloatT; FloatT; FloatT; FloatT;
        BoolT; BoolT; BoolT; BoolT; BoolT; BoolT;
        BoolT; BoolT;

        IntT;
        BoolT;
    ] in
    (List.map function_to_test input_values) = expected_result
;;

(* ----- Typage d'une commande ----- *)
let rec tp_cmd (env: environment) (cmd: com) =
    match cmd with
    | Skip -> VoidT
    | Exit -> VoidT
    | Assign(name, expr) -> VoidT
    | Seq(cmd1, cmd2) ->
        let tp_cmd1 = (tp_cmd env cmd1)
        and tp_cmd2 = (tp_cmd env cmd2)
        in if tp_cmd1 = VoidT
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
        let _ = (tp_cmd env cmd1) in VoidT
    | CallC(name, list_expr) -> tp_expr env (CallE(name, list_expr)) (* TODO => Question: Il faut renvoyer le type ??? *)
    | Return(expr) -> (tp_expr env expr)
;;
(* val tp_cmd :
   environment -> Lang.com
   -> Lang.tp = <fun>
*)

(* - tp_cmd: TESTS - *)
let test_tp_cmd =
    let function_to_test = tp_cmd {
        localvars=[
            ("i", IntT);
            ("f", FloatT);
        ];
        funbind=[
            Fundecl(BoolT, "fun1", [Vardecl(IntT, "a"); Vardecl(FloatT, "b")])
        ]
    }
    and input_values = [
        Skip;
        Exit;
        Assign("test", Const(IntV(5)));
        Seq(Skip, Exit);
        Seq(Skip, Return(Const(FloatV(5.))));

        CondC(
            BinOp(BCompar(BCge), Const(IntV(4)), Const(IntV(5))),
            Skip, Skip
        );
        Loop(Exit);

        CallC("fun1", [VarE("i"); VarE("f")]);
        Return(Const(IntV(5)));
    ]
    and expected_result = [
        VoidT;
        VoidT;
        VoidT;
        VoidT; FloatT;
        VoidT;
        VoidT;
        BoolT;
        IntT;
    ] in
    (List.map function_to_test input_values) = expected_result
;;

(* ----- Typage d'une définition de fonction ----- *)
let rec count_same_var_name (name: string) (var_decl_list: vardecl list) =
    match var_decl_list with
    | [] -> 0
    | Vardecl(var_tp, var_name)::reste ->
        (count_same_var_name name reste)
        + if name = var_name then 1 else 0
;;
(* val count_same_var_decl : string -> Lang.vardecl list -> int = <fun> *)

let rec valid_vardecl_list (var_decl_list: vardecl list) =
    match var_decl_list with
    | [] -> true
    | Vardecl(var_tp, var_name)::reste ->
        if (count_same_var_name var_name reste) = 0
        then valid_vardecl_list reste
        else false
;;
(* val valid_vardecl_list : Lang.vardecl list -> bool = <fun> *)

let tp_fundefn (env: environment) (fun_def: fundefn) =
    let Fundefn(fun_decl, cmd) = fun_def in
    let Fundecl(fun_decl_tp,_,var_decl_list) = fun_decl in
    let env1 = (addEnv_fun fun_decl env) in (* Besoin d'effacer dans l'env si recurence ? *)
    let _ = tp_cmd env1 cmd
    in if valid_vardecl_list var_decl_list && (tp_cmd env1 cmd) = fun_decl_tp
    then fun_decl_tp
    else raise (Failure "Invalid function definition")
;;
(* val tp_fundefn :
   environment -> Lang.fundefn
   -> Lang.tp = <fun>
*)

(* - tp_fundefn: TESTS - *)
let test_tp_fundefn =
    let function_to_test = tp_fundefn {
        localvars=[];
        funbind=[]
    }
    and input_values = [
        Fundefn(
            Fundecl(BoolT, "fun1", [Vardecl(IntT, "a"); Vardecl(FloatT, "b")]),
            Return(BinOp(BCompar(BCeq), Const(IntV(4)), Const(IntV(5))))
        )
    ]
    and expected_result = [
        BoolT
    ] in
    (List.map function_to_test input_values) = expected_result
;;

(* let tp_stmt fundecls = ;; *)

let tp_prog (Prog (fundecls, fundefns)) = true;; (* try (tp_fundefn fundefns) && (tp_stmt fundecls) with _ -> false *)