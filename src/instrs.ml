(* Datatypes for Postscript instructions *)


open Lang

type instr =
  IVal of value
| IVar of int
| IOper of string
| IBloc of instr
| ISeq of instr list
| ILoop of instr
| IDef of fname * instr

let string_of_instr instr = ""
