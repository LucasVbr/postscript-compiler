%{
open Lang
%}

%token <string> IDENTIFIER
%token <string> LITCONSTANT
%token <string> STRINGCONSTANT
%token <Lang.tp> TP
%token <bool> BCONSTANT
%token <int> INTCONSTANT
%token <float> FLOATCONSTANT
%token PLUS MINUS TIMES DIV MOD FPLUS FMINUS FTIMES FDIV 
%token LPAREN RPAREN LBRACE RBRACE
%token EQ COMMA SEMICOLON COLON QMARK
%token IF ELSE WHILE FOR RETURN BCEQ BCGE BCGT BCLE BCLT BCNE BLAND BLOR
%token EOF

%left BLOR
%left BLAND
%left BCEQ BCNE
%left BCGE BCGT BCLE BCLT
%left PLUS MINUS FPLUS FMINUS
%left TIMES DIV FDIV FTIMES MOD
%left LPAREN RPAREN LBRACE RBRACE

%right IF ELSE
%right WHILE FOR
%right RETURN

%start start
%type <Lang.prog> start

%%

start: fundefn {  Prog ([], [$1]) }
  ;

fundefn:  /* Compound-statement dans la doc --> 6.8.2 */
  /* d'apres la doc, on peut lui mettre --> LBRACE RBRACE {Skip} */
  |fundecl LBRACE block_item_list_opt RBRACE { Fundefn($1, $3) }
  // |fundecl LBRACE statement RBRACE { Fundefn($1, $3) }
  
;

fundecl: TP IDENTIFIER LPAREN vardecl_comma_list_opt RPAREN 
  { Fundecl($1, $2, $4) }
;

vardecl_comma_list_opt:
  |TP IDENTIFIER COMMA vardecl_comma_list_opt {[Vardecl($1, $2)] @ $4} /* dans celle là c'est récursif*/ 
  |TP IDENTIFIER {[Vardecl($1, $2)]}
  |/* empty */{ [] }
;


constant: 
  |LITCONSTANT{LitV($1) }
  |STRINGCONSTANT {StringV($1) }
  |BCONSTANT {BoolV($1) }
  |INTCONSTANT {IntV($1) }
  |FLOATCONSTANT {FloatV($1) }
;

/*expression_opt:
 |{Skip}
 |expression {$1}
;
*/

expression: /* A.2.1 ---> 6.5.1 */
  |constant {Const($1)}
  |IDENTIFIER {VarE($1)}
  |LPAREN expression RPAREN {$2}
  |multiplicative_expression {$1}
  |additive_expression {$1}
  |relational_expression {$1}
  |equality_expression {$1}
  |logical_and_expression {$1} 
  |conditional_expression {$1}
;

/*les calculs*/
multiplicative_expression: /* 6.5.5 */
  |expression TIMES expression {BinOp(BArith(BAmul), $1, $3)}
  |expression FTIMES expression {BinOp(BArith(BAfmul), $1, $3)}
  |expression DIV expression {BinOp(BArith(BAdiv), $1, $3)}
  |expression FDIV expression {BinOp(BArith(BAfdiv), $1, $3)}
  |expression MOD expression {BinOp(BArith(BAmod), $1, $3)}
;

additive_expression: /* 6.5.6 */
  |expression PLUS expression {BinOp(BArith(BAadd), $1, $3)}
  |expression MINUS expression {BinOp(BArith(BAsub), $1, $3)}
  |expression FPLUS expression {BinOp(BArith(BAfadd), $1, $3)}
  |expression FMINUS expression {BinOp(BArith(BAfsub), $1, $3)}
;

/*les comparaisons*/
relational_expression: /* 6.5.8 */
  |expression BCLE expression {BinOp(BCompar(BCle), $1, $3)}
  |expression BCGE expression {BinOp(BCompar(BCge), $1, $3)}
  |expression BCGT expression {BinOp(BCompar(BCgt), $1, $3)}
  |expression BCLT expression {BinOp(BCompar(BClt), $1, $3)}
;
equality_expression: /* 6.5.9 */
  |expression BCEQ expression {BinOp(BCompar(BCeq), $1, $3)}
  |expression BCNE expression {BinOp(BCompar(BCne), $1, $3)}
;

/*les operateurs booleens */ 
logical_and_expression:
  |expression BLAND expression {BinOp(BBool(BBand), $1, $3)} /* 6.5.13 */
  |expression BLOR expression {BinOp(BBool(BBor), $1, $3)} /* 6.5.14 */
;


conditional_expression: /* 6.5.16 */
  |expression QMARK expression COLON expression {CondE($1, $3, $5)}
;


/*///////////// FIN DES EXPRESSIONS ///////////////////*/

/*///////////// DEBUT DES STATEMENTS //////////////////*/

statement: /* A.2.3 ----> 6.8 */
  |LBRACE block_item_list_opt RBRACE {$2} /* peut etre fundefn plutot vu que c'est le vrai compound_statement*/
  |select_statement {$1} /*if et else*/
  |iteration_statement {$1} /*while et for*/
  |jump_statement SEMICOLON {$1} /*return et break*/
  |assignation SEMICOLON {$1}
  // Faut faire l'appel à des foncttions avec CondC( fname * (expr_list ) )
;

block_item_list_opt: /* 6.8.2 */
  |statement {$1}
  |block_item_list_opt statement {Seq($1,$2)} /* pas sur du tout */
  | {Skip}
;



block_item: /* 6.8.2 */
  /* declaration de variable / function (A.2.2 dans la doc) */
  |statement {$1}
;


select_statement:  /* 6.8.4 */
  |IF LPAREN expression RPAREN statement {CondC ($3, $5, Skip) }
  |IF LPAREN expression RPAREN statement ELSE statement {CondC ($3, $5, $7) }
;

iteration_statement: /* 6.8.5 */
  |WHILE LPAREN expression RPAREN statement {Loop (Seq (CondC ( $3, Skip, Exit), $5))}
  /*|FOR LPAREN expression_opt SEMICOLON expression_opt SEMICOLON expression_opt RPAREN statement {Loop (Seq (CondC ( $5, Skip, Exit), $9))} */ /*Absolument pas sur*/
;

jump_statement: /* 6.8.6 */
  |RETURN expression {Return($2)}
;

assignation: /* 6.5.17 */ /* dans la doc ça fait parti des expressions mais pg */
  |IDENTIFIER EQ expression {Assign($1, $3)}
;



/*
  Dans ce qu'il y a marqué dans lang.ml, il manque 
  
  expr : CallE of fname * (expr list) ( call expression )

  et 

  com : CallC of fname * (expr list)  ( call statement )
*/



/*
  Demander au prof : 


 ---Comment on différencie "==" et "="

 ---Type LitT (peut etre list ?)

 --- Je vois pas comment déclarer une variable (utiliser Vardecl qui demande un tp * vname) 
     qui renvoie un type vardecl avec Assign qui demande vname * expr ( et qui renvoie un com) 

     pour pouvoir prendre en compte "int x = 15;" par exemple


 ---Comment ça marche pour appeler une fonction ou expressionn CallE et CallC ?


 ---|block_item_list_opt {$1} // peut etre fundefn plutot vu que c'est le vrai compound_statement

     : d'apres la doc, fundefn c'est le compound_statement, mais il a pas le optionnel 

     associativité des opérateurs  ? (dans le pdf TP2 : introduction à YACC,  partie 4 --> développons la grammaire ---> tirer 2)
*/
