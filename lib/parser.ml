type token =
  | IDENTIFIER of (string)
  | LITCONSTANT of (string)
  | STRINGCONSTANT of (string)
  | TP of (Lang.tp)
  | BCONSTANT of (bool)
  | INTCONSTANT of (int)
  | FLOATCONSTANT of (float)
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | MOD
  | FPLUS
  | FMINUS
  | FTIMES
  | FDIV
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | EQ
  | COMMA
  | SEMICOLON
  | COLON
  | QMARK
  | IF
  | ELSE
  | WHILE
  | FOR
  | RETURN
  | BCEQ
  | BCGE
  | BCGT
  | BCLE
  | BCLT
  | BCNE
  | BLAND
  | BLOR
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Lang
# 47 "parser.ml"
let yytransl_const = [|
  264 (* PLUS *);
  265 (* MINUS *);
  266 (* TIMES *);
  267 (* DIV *);
  268 (* MOD *);
  269 (* FPLUS *);
  270 (* FMINUS *);
  271 (* FTIMES *);
  272 (* FDIV *);
  273 (* LPAREN *);
  274 (* RPAREN *);
  275 (* LBRACE *);
  276 (* RBRACE *);
  277 (* EQ *);
  278 (* COMMA *);
  279 (* SEMICOLON *);
  280 (* COLON *);
  281 (* QMARK *);
  282 (* IF *);
  283 (* ELSE *);
  284 (* WHILE *);
  285 (* FOR *);
  286 (* RETURN *);
  287 (* BCEQ *);
  288 (* BCGE *);
  289 (* BCGT *);
  290 (* BCLE *);
  291 (* BCLT *);
  292 (* BCNE *);
  293 (* BLAND *);
  294 (* BLOR *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* IDENTIFIER *);
  258 (* LITCONSTANT *);
  259 (* STRINGCONSTANT *);
  260 (* TP *);
  261 (* BCONSTANT *);
  262 (* INTCONSTANT *);
  263 (* FLOATCONSTANT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\004\000\003\000\006\000\
\006\000\007\000\007\000\008\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\011\000\011\000\012\000\012\000\
\013\000\013\000\014\000\015\000\015\000\015\000\015\000\015\000\
\016\000\016\000\017\000\017\000\017\000\017\000\018\000\018\000\
\019\000\020\000\020\000\020\000\020\000\020\000\021\000\021\000\
\021\000\022\000\022\000\023\000\023\000\010\000\024\000\024\000\
\024\000\024\000\024\000\025\000\005\000\005\000\030\000\030\000\
\026\000\026\000\033\000\032\000\031\000\027\000\027\000\028\000\
\029\000\000\000"

let yylen = "\002\000\
\001\000\002\000\001\000\003\000\002\000\004\000\005\000\000\000\
\001\000\001\000\003\000\002\000\001\000\001\000\001\000\001\000\
\001\000\001\000\003\000\007\000\001\000\004\000\000\000\001\000\
\001\000\003\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\003\000\001\000\001\000\001\000\001\000\001\000\003\000\
\001\000\001\000\003\000\003\000\003\000\003\000\001\000\003\000\
\003\000\001\000\003\000\001\000\003\000\001\000\001\000\001\000\
\001\000\001\000\001\000\003\000\000\000\001\000\001\000\002\000\
\002\000\002\000\004\000\003\000\001\000\003\000\005\000\003\000\
\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\074\000\000\000\000\000\003\000\000\000\
\000\000\005\000\000\000\002\000\000\000\004\000\000\000\000\000\
\000\000\000\000\000\000\000\000\069\000\055\000\056\000\057\000\
\058\000\059\000\000\000\063\000\000\000\000\000\000\000\000\000\
\000\000\010\000\000\000\000\000\000\000\000\000\017\000\018\000\
\014\000\015\000\016\000\000\000\021\000\000\000\027\000\033\000\
\000\000\000\000\042\000\000\000\000\000\000\000\000\000\000\000\
\000\000\006\000\064\000\065\000\066\000\012\000\007\000\000\000\
\025\000\000\000\000\000\068\000\060\000\000\000\000\000\000\000\
\028\000\029\000\030\000\031\000\032\000\000\000\035\000\036\000\
\037\000\038\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\072\000\073\000\011\000\067\000\000\000\
\000\000\019\000\000\000\000\000\034\000\000\000\046\000\044\000\
\045\000\043\000\000\000\000\000\000\000\000\000\026\000\022\000\
\000\000\071\000\000\000\000\000\020\000"

let yydgoto = "\002\000\
\004\000\005\000\006\000\007\000\020\000\032\000\033\000\034\000\
\045\000\065\000\047\000\066\000\067\000\048\000\078\000\049\000\
\083\000\050\000\051\000\052\000\053\000\054\000\055\000\021\000\
\022\000\023\000\024\000\025\000\026\000\027\000\028\000\029\000\
\030\000"

let yysindex = "\003\000\
\011\255\000\000\016\255\000\000\011\255\251\254\000\000\031\255\
\026\255\000\000\180\255\000\000\051\255\000\000\014\255\180\255\
\005\255\005\255\005\255\060\255\000\000\000\000\000\000\000\000\
\000\000\000\000\180\255\000\000\058\255\061\255\082\255\069\255\
\066\255\000\000\005\255\005\255\072\255\076\255\000\000\000\000\
\000\000\000\000\000\000\005\255\000\000\180\255\000\000\000\000\
\057\255\012\255\000\000\042\255\253\254\052\255\056\255\180\255\
\074\255\000\000\000\000\000\000\000\000\000\000\000\000\051\255\
\000\000\086\255\084\255\000\000\000\000\005\255\247\254\094\255\
\000\000\000\000\000\000\000\000\000\000\005\255\000\000\000\000\
\000\000\000\000\005\255\005\255\005\255\005\255\005\255\005\255\
\005\255\005\255\005\255\000\000\000\000\000\000\000\000\005\255\
\104\255\000\000\005\255\180\255\000\000\057\255\000\000\000\000\
\000\000\000\000\042\255\042\255\253\254\052\255\000\000\000\000\
\101\255\000\000\005\255\108\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\127\000\000\000\000\000\000\000\
\000\000\000\000\110\255\000\000\113\255\000\000\000\000\110\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\112\255\000\000\000\000\000\000\000\000\000\000\
\117\255\000\000\118\255\000\000\000\000\028\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\077\255\141\255\000\000\167\255\245\255\041\000\093\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\126\255\000\000\000\000\118\255\000\000\004\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\115\255\000\000\000\000\
\000\000\000\000\193\255\219\255\015\000\067\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\149\000\150\000\140\000\000\000\000\000\094\000\
\000\000\239\255\000\000\087\000\000\000\083\000\000\000\079\000\
\000\000\000\000\033\000\238\255\080\000\089\000\000\000\213\255\
\000\000\000\000\000\000\000\000\000\000\000\000\155\000\000\000\
\000\000"

let yytablesize = 379
let yytable = "\046\000\
\056\000\057\000\072\000\001\000\070\000\038\000\039\000\040\000\
\098\000\041\000\042\000\043\000\092\000\011\000\003\000\099\000\
\008\000\012\000\068\000\079\000\080\000\044\000\070\000\070\000\
\081\000\082\000\071\000\088\000\013\000\070\000\035\000\070\000\
\089\000\070\000\036\000\013\000\013\000\013\000\013\000\013\000\
\013\000\013\000\013\000\013\000\011\000\013\000\013\000\013\000\
\014\000\013\000\013\000\013\000\013\000\013\000\031\000\013\000\
\114\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
\013\000\013\000\073\000\074\000\075\000\107\000\108\000\076\000\
\077\000\084\000\085\000\086\000\087\000\039\000\111\000\058\000\
\060\000\113\000\062\000\061\000\039\000\039\000\063\000\064\000\
\090\000\039\000\039\000\069\000\070\000\091\000\039\000\039\000\
\093\000\116\000\039\000\039\000\039\000\039\000\039\000\095\000\
\039\000\096\000\039\000\039\000\039\000\039\000\039\000\039\000\
\039\000\039\000\039\000\040\000\103\000\104\000\105\000\106\000\
\100\000\112\000\040\000\040\000\115\000\117\000\001\000\040\000\
\040\000\061\000\008\000\062\000\040\000\040\000\009\000\023\000\
\040\000\040\000\040\000\040\000\040\000\041\000\040\000\024\000\
\040\000\040\000\040\000\040\000\040\000\040\000\040\000\040\000\
\040\000\009\000\010\000\037\000\097\000\094\000\041\000\041\000\
\101\000\102\000\041\000\041\000\041\000\041\000\041\000\047\000\
\041\000\109\000\041\000\041\000\041\000\041\000\041\000\041\000\
\041\000\041\000\041\000\110\000\015\000\059\000\000\000\000\000\
\047\000\047\000\000\000\000\000\047\000\047\000\047\000\047\000\
\047\000\048\000\047\000\000\000\047\000\047\000\016\000\000\000\
\000\000\000\000\047\000\047\000\047\000\017\000\000\000\018\000\
\000\000\019\000\048\000\048\000\000\000\000\000\048\000\048\000\
\048\000\048\000\048\000\049\000\048\000\000\000\048\000\048\000\
\000\000\000\000\000\000\000\000\048\000\048\000\048\000\000\000\
\000\000\000\000\000\000\000\000\049\000\049\000\000\000\000\000\
\049\000\049\000\049\000\049\000\049\000\050\000\049\000\000\000\
\049\000\049\000\000\000\000\000\000\000\000\000\049\000\049\000\
\049\000\000\000\000\000\000\000\000\000\000\000\050\000\050\000\
\000\000\000\000\050\000\050\000\050\000\050\000\050\000\051\000\
\050\000\000\000\050\000\000\000\000\000\000\000\000\000\000\000\
\000\000\050\000\050\000\000\000\000\000\000\000\000\000\000\000\
\051\000\051\000\000\000\000\000\051\000\051\000\051\000\051\000\
\051\000\052\000\051\000\000\000\051\000\000\000\000\000\000\000\
\000\000\000\000\000\000\051\000\051\000\000\000\000\000\000\000\
\000\000\000\000\052\000\052\000\000\000\000\000\052\000\052\000\
\052\000\052\000\052\000\053\000\052\000\000\000\052\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\052\000\000\000\
\000\000\000\000\000\000\000\000\053\000\053\000\000\000\000\000\
\053\000\053\000\053\000\053\000\053\000\054\000\053\000\000\000\
\053\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\053\000\000\000\000\000\000\000\000\000\000\000\054\000\054\000\
\000\000\000\000\054\000\054\000\054\000\054\000\054\000\000\000\
\054\000\000\000\054\000"

let yycheck = "\017\000\
\018\000\019\000\046\000\001\000\001\001\001\001\002\001\003\001\
\018\001\005\001\006\001\007\001\056\000\019\001\004\001\025\001\
\001\001\023\001\036\000\008\001\009\001\017\001\019\001\020\001\
\013\001\014\001\044\000\031\001\001\001\026\001\017\001\028\001\
\036\001\030\001\021\001\008\001\009\001\010\001\011\001\012\001\
\013\001\014\001\015\001\016\001\019\001\018\001\019\001\017\001\
\023\001\022\001\023\001\024\001\025\001\026\001\004\001\028\001\
\100\000\030\001\031\001\032\001\033\001\034\001\035\001\036\001\
\037\001\038\001\010\001\011\001\012\001\088\000\089\000\015\001\
\016\001\032\001\033\001\034\001\035\001\001\001\096\000\020\001\
\023\001\099\000\001\001\023\001\008\001\009\001\018\001\022\001\
\037\001\013\001\014\001\020\001\017\001\038\001\018\001\019\001\
\023\001\115\000\022\001\023\001\024\001\025\001\026\001\018\001\
\028\001\022\001\030\001\031\001\032\001\033\001\034\001\035\001\
\036\001\037\001\038\001\001\001\084\000\085\000\086\000\087\000\
\027\001\018\001\008\001\009\001\024\001\018\001\000\000\013\001\
\014\001\020\001\018\001\020\001\018\001\019\001\018\001\018\001\
\022\001\023\001\024\001\025\001\026\001\001\001\028\001\018\001\
\030\001\031\001\032\001\033\001\034\001\035\001\036\001\037\001\
\038\001\005\000\005\000\016\000\070\000\064\000\018\001\019\001\
\078\000\083\000\022\001\023\001\024\001\025\001\026\001\001\001\
\028\001\090\000\030\001\031\001\032\001\033\001\034\001\035\001\
\036\001\037\001\038\001\091\000\001\001\027\000\255\255\255\255\
\018\001\019\001\255\255\255\255\022\001\023\001\024\001\025\001\
\026\001\001\001\028\001\255\255\030\001\031\001\019\001\255\255\
\255\255\255\255\036\001\037\001\038\001\026\001\255\255\028\001\
\255\255\030\001\018\001\019\001\255\255\255\255\022\001\023\001\
\024\001\025\001\026\001\001\001\028\001\255\255\030\001\031\001\
\255\255\255\255\255\255\255\255\036\001\037\001\038\001\255\255\
\255\255\255\255\255\255\255\255\018\001\019\001\255\255\255\255\
\022\001\023\001\024\001\025\001\026\001\001\001\028\001\255\255\
\030\001\031\001\255\255\255\255\255\255\255\255\036\001\037\001\
\038\001\255\255\255\255\255\255\255\255\255\255\018\001\019\001\
\255\255\255\255\022\001\023\001\024\001\025\001\026\001\001\001\
\028\001\255\255\030\001\255\255\255\255\255\255\255\255\255\255\
\255\255\037\001\038\001\255\255\255\255\255\255\255\255\255\255\
\018\001\019\001\255\255\255\255\022\001\023\001\024\001\025\001\
\026\001\001\001\028\001\255\255\030\001\255\255\255\255\255\255\
\255\255\255\255\255\255\037\001\038\001\255\255\255\255\255\255\
\255\255\255\255\018\001\019\001\255\255\255\255\022\001\023\001\
\024\001\025\001\026\001\001\001\028\001\255\255\030\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\038\001\255\255\
\255\255\255\255\255\255\255\255\018\001\019\001\255\255\255\255\
\022\001\023\001\024\001\025\001\026\001\001\001\028\001\255\255\
\030\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\038\001\255\255\255\255\255\255\255\255\255\255\018\001\019\001\
\255\255\255\255\022\001\023\001\024\001\025\001\026\001\255\255\
\028\001\255\255\030\001"

let yynames_const = "\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  MOD\000\
  FPLUS\000\
  FMINUS\000\
  FTIMES\000\
  FDIV\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  EQ\000\
  COMMA\000\
  SEMICOLON\000\
  COLON\000\
  QMARK\000\
  IF\000\
  ELSE\000\
  WHILE\000\
  FOR\000\
  RETURN\000\
  BCEQ\000\
  BCGE\000\
  BCGT\000\
  BCLE\000\
  BCLT\000\
  BCNE\000\
  BLAND\000\
  BLOR\000\
  EOF\000\
  "

let yynames_block = "\
  IDENTIFIER\000\
  LITCONSTANT\000\
  STRINGCONSTANT\000\
  TP\000\
  BCONSTANT\000\
  INTCONSTANT\000\
  FLOATCONSTANT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fundecl_or_fundefn_list) in
    Obj.repr(
# 25 "parser.mly"
                               (  let (fdcs, fdfs) = _1 in Prog (fdcs, fdfs) )
# 335 "parser.ml"
               : Lang.prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'fundecl) in
    Obj.repr(
# 29 "parser.mly"
                    ( ([_1], []) )
# 342 "parser.ml"
               : 'fundecl_or_fundefn_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fundefn) in
    Obj.repr(
# 30 "parser.mly"
          ( ([], [_1]) )
# 349 "parser.ml"
               : 'fundecl_or_fundefn_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'fundecl_or_fundefn_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'fundecl) in
    Obj.repr(
# 31 "parser.mly"
                                            ( let (fdcs, fdfs) = _1 in (fdcs @ [_2], fdfs) )
# 357 "parser.ml"
               : 'fundecl_or_fundefn_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'fundecl_or_fundefn_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fundefn) in
    Obj.repr(
# 32 "parser.mly"
                                  ( let (fdcs, fdfs) = _1 in (fdcs, fdfs @ [_2]) )
# 365 "parser.ml"
               : 'fundecl_or_fundefn_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'fundecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'block_item_list_opt) in
    Obj.repr(
# 38 "parser.mly"
  ( Fundefn(_1, _3) )
# 373 "parser.ml"
               : 'fundefn))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : Lang.tp) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'vardecl_comma_list_opt) in
    Obj.repr(
# 42 "parser.mly"
  ( Fundecl(_1, _2, _4) )
# 382 "parser.ml"
               : 'fundecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 47 "parser.mly"
  ( [] )
# 388 "parser.ml"
               : 'vardecl_comma_list_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vardecl_comma_list) in
    Obj.repr(
# 49 "parser.mly"
    ( _1 )
# 395 "parser.ml"
               : 'vardecl_comma_list_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vardecl) in
    Obj.repr(
# 55 "parser.mly"
    ( [_1] )
# 402 "parser.ml"
               : 'vardecl_comma_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vardecl_comma_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vardecl) in
    Obj.repr(
# 57 "parser.mly"
    ( _1 @ [_3] )
# 410 "parser.ml"
               : 'vardecl_comma_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Lang.tp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 61 "parser.mly"
    ( Vardecl(_1, _2) )
# 418 "parser.ml"
               : 'vardecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 70 "parser.mly"
    ( VarE(_1) )
# 425 "parser.ml"
               : 'primary_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 72 "parser.mly"
    ( Const(BoolV _1) )
# 432 "parser.ml"
               : 'primary_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 74 "parser.mly"
    ( Const(IntV _1) )
# 439 "parser.ml"
               : 'primary_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 76 "parser.mly"
    ( Const(FloatV _1) )
# 446 "parser.ml"
               : 'primary_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 78 "parser.mly"
    ( Const(LitV _1) )
# 453 "parser.ml"
               : 'primary_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 80 "parser.mly"
    ( Const(StringV _1) )
# 460 "parser.ml"
               : 'primary_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 82 "parser.mly"
    ( _2 )
# 467 "parser.ml"
               : 'primary_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'expression) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'expression) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 84 "parser.mly"
    ( CondE (_2, _4, _6) )
# 476 "parser.ml"
               : 'primary_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'primary_expression) in
    Obj.repr(
# 90 "parser.mly"
      ( _1 )
# 483 "parser.ml"
               : 'postfix_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'argument_expression_list_opt) in
    Obj.repr(
# 92 "parser.mly"
      ( CallE(_1, _3) )
# 491 "parser.ml"
               : 'postfix_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 97 "parser.mly"
    ( [] )
# 497 "parser.ml"
               : 'argument_expression_list_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'argument_expression_list) in
    Obj.repr(
# 99 "parser.mly"
      ( _1 )
# 504 "parser.ml"
               : 'argument_expression_list_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 104 "parser.mly"
      ( [_1] )
# 511 "parser.ml"
               : 'argument_expression_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'argument_expression_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 106 "parser.mly"
      ( _1 @ [_3] )
# 519 "parser.ml"
               : 'argument_expression_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'postfix_expression) in
    Obj.repr(
# 111 "parser.mly"
      ( _1 )
# 526 "parser.ml"
               : 'unary_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 117 "parser.mly"
          ( BAmul )
# 532 "parser.ml"
               : 'multiplicative_op))
; (fun __caml_parser_env ->
    Obj.repr(
# 118 "parser.mly"
          ( BAdiv )
# 538 "parser.ml"
               : 'multiplicative_op))
; (fun __caml_parser_env ->
    Obj.repr(
# 119 "parser.mly"
          ( BAmod )
# 544 "parser.ml"
               : 'multiplicative_op))
; (fun __caml_parser_env ->
    Obj.repr(
# 120 "parser.mly"
           ( BAfmul )
# 550 "parser.ml"
               : 'multiplicative_op))
; (fun __caml_parser_env ->
    Obj.repr(
# 121 "parser.mly"
           ( BAfdiv )
# 556 "parser.ml"
               : 'multiplicative_op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'unary_expression) in
    Obj.repr(
# 126 "parser.mly"
      ( _1 )
# 563 "parser.ml"
               : 'multiplicative_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'multiplicative_expression) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'multiplicative_op) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'unary_expression) in
    Obj.repr(
# 128 "parser.mly"
    ( BinOp(BArith _2, _1, _3) )
# 572 "parser.ml"
               : 'multiplicative_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 133 "parser.mly"
         ( BAadd )
# 578 "parser.ml"
               : 'additive_op))
; (fun __caml_parser_env ->
    Obj.repr(
# 134 "parser.mly"
         ( BAsub )
# 584 "parser.ml"
               : 'additive_op))
; (fun __caml_parser_env ->
    Obj.repr(
# 135 "parser.mly"
         ( BAfadd )
# 590 "parser.ml"
               : 'additive_op))
; (fun __caml_parser_env ->
    Obj.repr(
# 136 "parser.mly"
         ( BAfsub )
# 596 "parser.ml"
               : 'additive_op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'multiplicative_expression) in
    Obj.repr(
# 141 "parser.mly"
    ( _1 )
# 603 "parser.ml"
               : 'additive_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'additive_expression) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'additive_op) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'multiplicative_expression) in
    Obj.repr(
# 143 "parser.mly"
    ( BinOp(BArith _2, _1, _3) )
# 612 "parser.ml"
               : 'additive_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'additive_expression) in
    Obj.repr(
# 148 "parser.mly"
    ( _1 )
# 619 "parser.ml"
               : 'shift_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'shift_expression) in
    Obj.repr(
# 153 "parser.mly"
    ( _1 )
# 626 "parser.ml"
               : 'relational_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'relational_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'shift_expression) in
    Obj.repr(
# 155 "parser.mly"
    ( BinOp(BCompar BClt, _1, _3) )
# 634 "parser.ml"
               : 'relational_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'relational_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'shift_expression) in
    Obj.repr(
# 157 "parser.mly"
    ( BinOp(BCompar BCgt, _1, _3) )
# 642 "parser.ml"
               : 'relational_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'relational_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'shift_expression) in
    Obj.repr(
# 159 "parser.mly"
    ( BinOp(BCompar BCle, _1, _3) )
# 650 "parser.ml"
               : 'relational_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'relational_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'shift_expression) in
    Obj.repr(
# 161 "parser.mly"
    ( BinOp(BCompar BCge, _1, _3) )
# 658 "parser.ml"
               : 'relational_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'relational_expression) in
    Obj.repr(
# 166 "parser.mly"
    ( _1 )
# 665 "parser.ml"
               : 'equality_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'equality_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'relational_expression) in
    Obj.repr(
# 168 "parser.mly"
    ( BinOp(BCompar BCeq, _1, _3) )
# 673 "parser.ml"
               : 'equality_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'equality_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'relational_expression) in
    Obj.repr(
# 170 "parser.mly"
    ( BinOp(BCompar BCne, _1, _3) )
# 681 "parser.ml"
               : 'equality_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'equality_expression) in
    Obj.repr(
# 175 "parser.mly"
    ( _1 )
# 688 "parser.ml"
               : 'logical_AND_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'logical_AND_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'equality_expression) in
    Obj.repr(
# 177 "parser.mly"
    ( BinOp(BBool BBand, _1, _3) )
# 696 "parser.ml"
               : 'logical_AND_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'logical_AND_expression) in
    Obj.repr(
# 182 "parser.mly"
    ( _1 )
# 703 "parser.ml"
               : 'logical_OR_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'logical_OR_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'logical_AND_expression) in
    Obj.repr(
# 184 "parser.mly"
    ( BinOp(BBool BBor, _1, _3) )
# 711 "parser.ml"
               : 'logical_OR_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'logical_OR_expression) in
    Obj.repr(
# 192 "parser.mly"
    ( _1 )
# 718 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'compound_statement) in
    Obj.repr(
# 200 "parser.mly"
                       ( _1 )
# 725 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expression_statement) in
    Obj.repr(
# 201 "parser.mly"
                         ( _1 )
# 732 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'selection_statement) in
    Obj.repr(
# 202 "parser.mly"
                         ( _1 )
# 739 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'iteration_statement) in
    Obj.repr(
# 203 "parser.mly"
                        ( _1 )
# 746 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'jump_statement) in
    Obj.repr(
# 204 "parser.mly"
                   ( _1 )
# 753 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'block_item_list_opt) in
    Obj.repr(
# 211 "parser.mly"
    ( _2 )
# 760 "parser.ml"
               : 'compound_statement))
; (fun __caml_parser_env ->
    Obj.repr(
# 216 "parser.mly"
  ( Skip)
# 766 "parser.ml"
               : 'block_item_list_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'block_item_list) in
    Obj.repr(
# 218 "parser.mly"
    ( _1 )
# 773 "parser.ml"
               : 'block_item_list_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'block_item) in
    Obj.repr(
# 223 "parser.mly"
    ( _1 )
# 780 "parser.ml"
               : 'block_item_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'block_item_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'block_item) in
    Obj.repr(
# 225 "parser.mly"
    ( Seq (_1, _2) )
# 788 "parser.ml"
               : 'block_item_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'assignment) in
    Obj.repr(
# 231 "parser.mly"
                         ( _1 )
# 795 "parser.ml"
               : 'expression_statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'call_statement) in
    Obj.repr(
# 232 "parser.mly"
                             ( _1 )
# 802 "parser.ml"
               : 'expression_statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'argument_expression_list_opt) in
    Obj.repr(
# 237 "parser.mly"
    ( CallC(_1, _3) )
# 810 "parser.ml"
               : 'call_statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 245 "parser.mly"
  ( Assign(_1, _3) )
# 818 "parser.ml"
               : 'assignment))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 255 "parser.mly"
                       ( _1 )
# 825 "parser.ml"
               : 'block_item))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 261 "parser.mly"
  ( CondC(_2, _3, Skip) )
# 833 "parser.ml"
               : 'selection_statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'statement) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 263 "parser.mly"
    ( CondC(_2, _3, _5) )
# 842 "parser.ml"
               : 'selection_statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 269 "parser.mly"
  ( Loop(Seq(CondC(_2, Skip, Exit), _3)) )
# 850 "parser.ml"
               : 'iteration_statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 274 "parser.mly"
    ( Return _2 )
# 857 "parser.ml"
               : 'jump_statement))
(* Entry start *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let start (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Lang.prog)
