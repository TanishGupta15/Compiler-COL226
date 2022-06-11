open ASTree
datatype return_type = I | B | Err;
val hashTable: (string, return_type) HashTable.hash_table = HashTable.mkTable (HashString.hashString, op=) (100, Fail "not found");

fun insert_hashtable (x, y) = 
    let 
        val doesnt_matter = HashTable.insert(hashTable) (x,y);
    in
        x
    end;

fun insert_dec(ASTree.node(DE, x, y)) = 
    let
        fun insert_variableList (ASTree.node(VL, z, t)) = 
            let 
                fun get_id_name (ASTree.idnode(ID, a, b))  = a | get_id_name (_) = raise Fail "Not an ID";
                fun ast_to_returntype(ASTree.node(ASTree.INT, d, e)) = I | ast_to_returntype(ASTree.node(ASTree.BOOL, d, e)) = B | ast_to_returntype(_) = raise Fail "Not an INT or BOOL";
                val nomatter = insert_hashtable (get_id_name(z), ast_to_returntype(y)) ;
            in
                if(t = ASTree.empty) then t
                else insert_variableList(t)
            end
        |  insert_variableList (_) = raise Fail "Not a Variable List";
        val matter =  insert_variableList(x);
    in
       ASTree.node(DE, x, y)
    end
| insert_dec(_) = raise Fail "Not a declaration Seq";

fun typeofvar (ASTree.node(ASTree.INT, x, y)) : return_type = I
      | typeofvar (ASTree.node(ASTree.BOOL, x, y)) = B
      | typeofvar (ASTree.idnode(ASTree.ID, x, y)) = 
        if(HashTable.inDomain hashTable x = true) then
            if(HashTable.lookup hashTable x = I) then I else B
        else Err
      | typeofvar (_) = Err;

fun typeofexp (ASTree.node(ASTree.TT, x, y)) = B
      | typeofexp (ASTree.node(ASTree.FF, x, y)) = B
      | typeofexp (ASTree.node(ASTree.NUM, x, y)) = I
      | typeofexp (ASTree.node(ASTree.Var, x, y)) = typeofvar(x)
      | typeofexp (ASTree.node(ASTree.GT, x, y)) = if((typeofexp(x) = B andalso typeofexp(y) = B) orelse (typeofexp(x) = I andalso typeofexp(y) = I))  then B else Err
      | typeofexp (ASTree.node(ASTree.LT, x, y)) = if((typeofexp(x) = B andalso typeofexp(y) = B) orelse (typeofexp(x) = I andalso typeofexp(y) = I))  then B else Err
      | typeofexp (ASTree.node(ASTree.LEQ, x, y)) = if((typeofexp(x) = B andalso typeofexp(y) = B) orelse (typeofexp(x) = I andalso typeofexp(y) = I))  then B else Err
      | typeofexp (ASTree.node(ASTree.EQ, x, y)) = if((typeofexp(x) = B andalso typeofexp(y) = B) orelse (typeofexp(x) = I andalso typeofexp(y) = I))  then B else Err
      | typeofexp (ASTree.node(ASTree.GEQ, x, y)) = if((typeofexp(x) = B andalso typeofexp(y) = B) orelse (typeofexp(x) = I andalso typeofexp(y) = I))  then B else Err
      | typeofexp (ASTree.node(ASTree.NEQ, x, y)) = if((typeofexp(x) = B andalso typeofexp(y) = B) orelse (typeofexp(x) = I andalso typeofexp(y) = I))  then B else Err
      | typeofexp (ASTree.node(ASTree.NOT, x, y)) = if(typeofexp(x) = B) then B else Err
      | typeofexp (ASTree.node(ASTree.NEGATION, x, y)) = if(typeofexp(x) = I) then I else Err
      | typeofexp (ASTree.node(ASTree.PLUS, x, y)) = if(typeofexp(x) = I andalso typeofexp(y) = I) then I else Err
      | typeofexp (ASTree.node(ASTree.MINUS, x, y)) = if(typeofexp(x) = I andalso typeofexp(y) = I) then I else Err
      | typeofexp (ASTree.node(ASTree.TIMES, x, y)) = if(typeofexp(x) = I andalso typeofexp(y) = I) then I else Err
      | typeofexp (ASTree.node(ASTree.AND, x, y)) = if(typeofexp(x) = B andalso typeofexp(y) = B) then B else Err
      | typeofexp (ASTree.node(ASTree.OR, x, y)) = if(typeofexp(x) = B andalso typeofexp(y) = B) then B else Err
      | typeofexp (ASTree.node(ASTree.DIV, x, y)) = if(typeofexp(x) = I andalso typeofexp(y) = I) then I else Err
      | typeofexp (ASTree.node(ASTree.MOD, x, y)) = if(typeofexp(x) = I andalso typeofexp(y) = I) then I else Err
      | typeofexp (_) = Err;

fun check_type (ASTree.node(ASTree.PROG, x, y)) = check_type(y)
    | check_type (ASTree.node(ASTree.BLK, x, y)) = check_type(y)
    | check_type (ASTree.empty) = true
    | check_type (ASTree.node(ASTree.SEQ, x, y)) = check_type(x) andalso check_type(y)
    | check_type (ASTree.node(ASTree.SET, x, y)) = (typeofvar(x) = typeofexp(y))
    | check_type (ASTree.node3(ASTree.ITE, x, y, z)) = (typeofexp(x) = B andalso check_type(y) andalso check_type(z))
    | check_type (ASTree.node(ASTree.WH, x, y)) = (typeofexp(x) = B andalso check_type(y))
    | check_type (ASTree.node(ASTree.READ, x, y)) = (y = ASTree.empty)
    | check_type (ASTree.node(ASTree.WRITE, x, y)) = (typeofexp(x) = I)
    | check_type (_) = false;

fun main(program : AST, type_chk) = if type_chk then program else empty;
%%

%name while_ast (* This is the name of the parser, make sure it agrees with %header in the lex file*)

%term (*This is the set of terminals*)
TERMINATOR | DEFINE | ASSIGNA | ASSIGNB | PROGRAM | VAR of string| 
INT| BOOL | LPAREN | RPAREN | LBRACE | RBRACE |Read | Write |
IF | THEN | ELSE | ENDIF | WHILE | DO | ENDWH | 
OR | AND | TRUE | FALSE | NOT | LT | GT | GEQ | EQ | LEQ | NEQ | PLUS | MINUS | MULT | DIV | MOD | Negation | IDENTIFIER of string| EOF | NUMERAL of int| COMMA


%nonterm (*This is the set of nonterminals*)
program of ASTree.AST | BLOCK of ASTree.AST| DeclarationSeq of ASTree.AST| Declaration of ASTree.AST| dt of ASTree.AST| VariableList of ASTree.AST| CommandSeq of ASTree.AST| command of ASTree.AST| Expression of ASTree.AST| Variable of ASTree.AST | Number of ASTree.AST


(*Uptil here were the required declarations, we can also declare some optional declarations*)

%pos int 

%eop EOF
%noshift EOF

%start program

%left OR
%left AND
%left LT GT GEQ EQ LEQ NEQ (*This is a design choice*)
%left PLUS MINUS
%left MULT DIV MOD
%right NOT
%right Negation

%verbose

%%

(* All rules are in the form of grammar. Firstly productions are written, along with the return value when that production is applied. *)

(* The output ast will look like the Binary trees to string form as built in assign 2*)

program : PROGRAM Variable DEFINE BLOCK (main(node(PROG, Variable, BLOCK), check_type(node(PROG, Variable, BLOCK))))
BLOCK : DeclarationSeq LBRACE CommandSeq RBRACE (ASTree.node(BLK, DeclarationSeq, CommandSeq))
DeclarationSeq : (*no declarations*) (ASTree.node(DES, empty, empty))
                             |  Declaration DeclarationSeq (ASTree.node(DES, Declaration, DeclarationSeq))
Declaration : VAR VariableList ASSIGNB dt TERMINATOR (insert_dec(ASTree.node(DE, VariableList, dt)))
dt : INT (ASTree.node(INT, empty, empty))
    | BOOL (ASTree.node(BOOL, empty,empty))
VariableList : Variable COMMA VariableList (ASTree.node(VL, Variable, VariableList))
                        | Variable (ASTree.node(VL, Variable, empty)) 

CommandSeq : command TERMINATOR CommandSeq (ASTree.node(SEQ, command, CommandSeq))
                            | (*no more commands*) (ASTree.node(SEQ, empty, empty))
command : Variable ASSIGNA Expression  (ASTree.node(SET, Variable, Expression))
                    | Read Variable (ASTree.node(READ, Variable, empty))
                    | Write Expression (ASTree.node(WRITE, Expression, empty))
                    | IF Expression THEN LBRACE CommandSeq RBRACE ELSE LBRACE CommandSeq RBRACE ENDIF (ASTree.node3(ITE, Expression, CommandSeq1, CommandSeq2))
                    | WHILE Expression DO LBRACE CommandSeq RBRACE ENDWH (ASTree.node(WH, Expression, CommandSeq))

Expression : 
            TRUE (ASTree.node(ASTree.TT, empty, empty))
            | FALSE (ASTree.node(ASTree.FF, empty, empty))
            | Number (ASTree.node(ASTree.NUM, Number, empty))
            | Variable (ASTree.node(ASTree.Var, Variable, empty)) 
            | Expression GT Expression %prec GT (ASTree.node(ASTree.GT, Expression1, Expression2))
            | Expression LT Expression %prec LT (ASTree.node(ASTree.LT, Expression1, Expression2))
            | Expression LEQ Expression %prec LEQ (ASTree.node(ASTree.LEQ, Expression1, Expression2))
            | Expression EQ Expression %prec EQ (ASTree.node(ASTree.EQ, Expression1, Expression2))
            | Expression GEQ Expression %prec GEQ (ASTree.node(ASTree.GEQ, Expression1, Expression2))
            | Expression NEQ Expression %prec NEQ (ASTree.node(ASTree.NEQ, Expression1, Expression2))
            | Expression AND Expression %prec AND (ASTree.node(ASTree.AND, Expression1, Expression2))
            | Expression OR Expression %prec OR (ASTree.node(ASTree.OR, Expression1, Expression2))
            | LPAREN Expression RPAREN (Expression)
            | NOT Expression %prec NOT (ASTree.node(ASTree.NOT, Expression, empty))
            | Negation Expression %prec Negation (ASTree.node(ASTree.NEGATION, Expression, empty))
            | Expression PLUS Expression  %prec PLUS (ASTree.node(ASTree.PLUS, Expression1, Expression2))
            | Expression MINUS Expression  %prec MINUS (ASTree.node(ASTree.MINUS, Expression1, Expression2))
            | Expression MULT Expression  %prec MULT (ASTree.node(ASTree.TIMES, Expression1, Expression2))
            | Expression DIV Expression  %prec DIV (ASTree.node(ASTree.DIV, Expression1, Expression2))
            | Expression MOD Expression  %prec MOD (ASTree.node(ASTree.MOD, Expression1, Expression2))

Variable : IDENTIFIER (ASTree.idnode(ASTree.ID, IDENTIFIER, empty))
Number: NUMERAL (ASTree.intnode(ASTree.NUM, NUMERAL, empty))
