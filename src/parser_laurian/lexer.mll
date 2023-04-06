{
  open Lexing
  open Parser
  open Lang
  exception Lexerror

  let pos lexbuf = (lexeme_start lexbuf, lexeme_end lexbuf)

  let advance_line_pos pos =
    { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum; }

  let advance_line lexbuf =
    lexbuf.lex_curr_p <- advance_line_pos lexbuf.lex_curr_p

}

let includeline = '#' [^ '\n']* '\n'
let num     = ['1'-'9']['0'-'9']*
let num_virgule     = ['1'-'9']['0'-'9']*'.'(['1'-'9']['0'-'9']*)?
let alph =           ['a'-'z''A'-'Z']
let literal = '/'alph(alph|'-')*
let str = '\"'alph(alph|'-'|'_'|num)*'\"'
let blancs  = [' ''\t']+
let comment = '/' '*' (blancs|alph|num|num_virgule|'>'|'<')* '*' '/'

rule token = parse
 blancs
    { token lexbuf }    (* white space: recursive call of lexer *)
|'\n'
    {advance_line lexbuf; token lexbuf }    (* white space: recursive call of lexer *)
| includeline
    { advance_line lexbuf; token lexbuf }    (* C include directives --> ignore *)
| comment
    { token lexbuf }    (* comment --> ignore *)


| "int" {TP(IntT)}
| "bool" {TP(BoolT)}
| "string" {TP(StringT)}
| "float" {TP(FloatT)}
| "void" {TP(VoidT)}
| "lit" {TP(LitT)}   (* Potentiellement list *) 

| '('  { LPAREN }
| ')'  { RPAREN }
| '{'  { LBRACE }
| '}'  { RBRACE }
| "=="  { BCEQ }
| '='  { EQ }
| '+'  { PLUS }
| '-'  { MINUS }
| '*'  { TIMES }
| '/'  { DIV }
| '%'  { MOD }
| '/'  { DIV }
| "<="  { BCLE }
| ">="  { BCGE }
| ">"  { BCGT }
| "<"  { BCLT }
| "!="  { BCNE }
| "and" {BLAND}
| "or" {BLOR}
| "," {COMMA}
| ";" {SEMICOLON}
| ":" {COLON}
| "?" {QMARK}


| "return" {RETURN}

| "if" {IF}
| "else" {ELSE}
| "while" {WHILE}
| "for" {FOR}

| "true" {BCONSTANT(true)}
| "false" {BCONSTANT(false)}
| num_virgule as s {FLOATCONSTANT(float_of_string s)}
| num as s {INTCONSTANT(int_of_string s)}
| literal as l    { LITCONSTANT l }
| str as s    { STRINGCONSTANT s }


| eof          {EOF}

| alph alph* as i  {IDENTIFIER i} 

| _  {Printf.printf "ERROR: unrecogized symbol '%s'\n" (Lexing.lexeme lexbuf);
      raise Lexerror }

and
    ruleTail acc = parse
| eof { acc }
| _* as str { ruleTail (acc ^ str) lexbuf }
