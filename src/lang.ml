(* Definition of source language data structures *)

(* variable names *)
type vname = string

(* function names *)
type fname = string

(* binary arithmetic operators *)
type barith = BAadd | BAsub | BAmul | BAdiv | BAmod (* integer *)
            | BAfadd | BAfsub | BAfmul | BAfdiv (* float *)

(* binary boolean operators: and, or *)
type bbool = BBand | BBor

(* binary comparison operators: =, >=, >, <=, <, != *)
type bcompar = BCeq | BCge | BCgt | BCle | BClt | BCne

(* binary operators, combining all of the above *)
type binop =
  BArith of barith
| BBool of bbool
| BCompar of bcompar
    
type value =
  BoolV of bool
| FloatV of float
| IntV of int
| LitV of string
| StringV of string

(* Expresssions *)
type expr = 
    Const of value                        (* constant *)
  | VarE of vname                         (* variable *)
  | BinOp of binop * expr *  expr         (* binary operation *)
  | CondE of expr * expr * expr           (* conditional expr *)
  | CallE of fname * (expr list)          (* call expression *)

(* Commands *)
type com =
    Skip                                  (* no operation *)
  | Exit                                  (* exit from loop *)
  | Assign of vname * expr                (* assign expression to var *)
  | Seq of com * com                      (* sequence of statements *)
  | CondC of expr * com * com             (* conditional com *)
  | Loop of com                           (* loop until exit *)
  | CallC of fname * (expr list)          (* call statement *)
  | Return of expr                        (* return from call *)

(* Types *)
type tp = BoolT | FloatT | IntT | LitT | StringT | VoidT

(* variable / parameter declaration *)
type vardecl = Vardecl of tp * vname

(* function declaration: return type; parameter declarations *)
type fundecl = Fundecl of tp * fname * (vardecl list)

(* function definition: function declaration; function body *)
type fundefn = Fundefn of fundecl * com

type prog = Prog of (fundecl list) * (fundefn list) 
    
