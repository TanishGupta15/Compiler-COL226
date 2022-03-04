# COL226 Assignment-3


## Context-free Grammar

program : "program" Variable "::" BLOCK 


BLOCK : DeclarationSeq "{" CommandSeq "}" 


DeclarationSeq : epsilon |  Declaration DeclarationSeq
  
Declaration : "var" VariableList ":" dt ";"

dt : INT | BOOL 

VariableList : Variable "," VariableList | Variable 

CommandSeq : command ";" CommandSeq | epsilon
                            
command : Variable ":=" Expression | "read" Variable | "write" Expression | "if" Expression "then" CommandSeq "else" CommandSeq "endif" | "while" Expression "do" CommandSeq "endwh"


Expression : 
            "tt" 
            | "ff"
            | Number
            | Variable
            | Expression ">" Expression | Expression "<" Expression | Expression "<=" Expression
            | Expression "=" Expression | Expression ">=" Expression
            | Expression "<>" Expression| Expression "&&" Expression 
            | Expression "||" Expression | "{" Expression "}"
            | "!" Expression | "~" Expression
            | Expression "+" Expression  
            | Expression "-" Expression
            | Expression "*" Expression 
            | Expression "/" Expression 
            | Expression "%" Expression
            
Variable : IDENTIFIER

Number : Digit Number | Digit

Digit: '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'

IDENTIFIER : Letter Mix

Mix : Letter | Digit | Letter Mix | Digit Mix

Letter : UpperCase | LowerCase


UpperCase : “A” | “B” | “C” | “D” | “E” | “F” | “G” | “H” |
“I” | “J” | “K” | “L” | “M” | “N” | “O” | “P” | “Q” |
“R” | “S” | “T” | “U” | “V ” | “W” | “X” | “Y ” | “Z”


LowerCase : “a” | “b” | “c” | “d” | “e” | “f” | “g” | “h” |
“i” | “j” | “k” | “l” | “m” | “n” | “o” | “p” | “q” |
“r” | “s” | “t” | “u” | “v” | “w” | “x” | “y” | “z”


## AST datatype definition

datatype node_data = LT | LEQ | EQ | GT | GEQ | NEQ | PROG | INT | BOOL | TT | FF | NOT | AND | OR | PLUS | MINUS | TIMES | DIV | MOD | BLK | DES | VL | SEQ | SET | READ | WRITE | ITE | WH | IEXP | BEXP | NEGATION | Var | DE | ID | NUM;


datatype AST = empty | node of node_data*AST*AST | node3 of node_data*AST*AST*AST | idnode of node_data * string* AST | intnode of node_data * int * AST;


The datatype AST can either be:
  1. An empty tree
  2. A node which has value of type node_data and has only 2 children (Most nodes will fall under this category).
  3. A node which has value of type node_data and has 3 children (Special node for If then else block). This node is called node3.
  4. A node whose 2nd element needs to be a string. This node is used for Identifier, and is called idnode.
  5. A node whose 2nd element needs to be an integer. This node is used for Numerals, and is called intnode.

## Syntax directed translation


program : PROGRAM Variable DEFINE BLOCK (main(node(PROG, Variable, BLOCK), check_type(node(PROG, Variable, BLOCK))))


BLOCK : DeclarationSeq LBRACE CommandSeq RBRACE (node(BLK, DeclarationSeq, CommandSeq))


DeclarationSeq : (*no declarations*) (node(DES, empty, empty)) |  Declaration DeclarationSeq (node(DES, Declaration, DeclarationSeq))

Declaration : VAR VariableList ASSIGNB dt TERMINATOR (insert_dec(node(DE, VariableList, dt)))

dt : INT (node(INT, empty, empty))
    | BOOL (node(BOOL, empty,empty))
    
VariableList : Variable COMMA VariableList (node(VL, Variable, VariableList))
                        | Variable (node(VL, Variable, empty)) 
                        

CommandSeq : command TERMINATOR CommandSeq (node(SEQ, command, CommandSeq))
                            | (*no more commands*) (node(SEQ, empty, empty))
                            
command : Variable ASSIGNA Expression  (node(SET, Variable, Expression))
                    | Read Variable (node(READ, Variable, empty))
                    | Write Expression (node(WRITE, Expression, empty))
                    | IF Expression THEN CommandSeq ELSE CommandSeq ENDIF (node3(ITE, Expression, CommandSeq1, CommandSeq2))
                    | WHILE Expression DO CommandSeq ENDWH (node(WH, Expression, CommandSeq))
                    

Expression : 
            TRUE (node(TT, empty, empty))
            | FALSE (node(FF, empty, empty))
            | Number (node(NUM, Number, empty))
            | Variable (node(Var, Variable, empty)) 
            | Expression GT Expression %prec GT (node(GT, Expression1, Expression2))
            | Expression LT Expression %prec LT (node(LT, Expression1, Expression2))
            | Expression LEQ Expression %prec LEQ (node(LEQ, Expression1, Expression2))
            | Expression EQ Expression %prec EQ (node(EQ, Expression1, Expression2))
            | Expression GEQ Expression %prec GEQ (node(GEQ, Expression1, Expression2))
            | Expression NEQ Expression %prec NEQ (node(NEQ, Expression1, Expression2))
            | Expression AND Expression %prec NEQ (node(AND, Expression1, Expression2))
            | Expression OR Expression %prec NEQ (node(OR, Expression1, Expression2))
            | LPAREN Expression RPAREN (Expression)
            | NOT Expression %prec NOT (node(NOT, Expression, empty))
            | Negation Expression %prec Negation (node(NEGATION, Expression, empty))
            | Expression PLUS Expression  %prec PLUS (node(PLUS, Expression1, Expression2))
            | Expression MINUS Expression  %prec MINUS (node(MINUS, Expression1, Expression2))
            | Expression MULT Expression  %prec MULT (node(TIMES, Expression1, Expression2))
            | Expression DIV Expression  %prec DIV (node(DIV, Expression1, Expression2))
            | Expression MOD Expression  %prec MOD (node(MOD, Expression1, Expression2))
            

Variable : IDENTIFIER (idnode(ID, IDENTIFIER, empty))


Number: NUMERAL (intnode(NUM, NUMERAL, empty))
    

## Other Design Decisions

  For this assignment, I have built an AST for a given program. 
  
  1. I have implemented a symbol Table and made sure to check types. If there is a datatype mismatch during command sequences, the final AST returned is empty. (Type checking has been implemented as discussed with Prof Arun Kumar in the lecture)
  2. I have included a makefile which contains the commands that need to be run on the terminal to compile lex and yacc files. Before running the parser, make sure to run the make command.
  3. To run the parser (after running the make command!), just type (parseFile "$(name_of_file.txt)" ;) in the repl environment, where $(name_of_file.txt) is replaced with the filename. SO, an example of this will look like: parseFile "test1.txt";
  4. I have tested the function on all the given test files. Some of the files intentionally contain errors to test parsing exhaustively.


## Other Implementation Decisions

  The final output is in the form of nodes (something similar to the Integer Binary Tree Built in Assignment 2).


## Acknowledgements

For learning ML-Lex and ML-YACC, I used the following resources: 

  http://www.rogerprice.org/ug/ug.pdf
  
  https://www.smlnj.org/doc/ML-Lex/manual.html
  
  https://www.smlnj.org/doc/CM/Old/index.html
  
  https://www.cs.princeton.edu/~appel/modern/ml/ml-yacc/manual.html
  
  https://www.smlnj.org/doc/ML-Yacc/mlyacc007.html
  
To learn about referencing and deferencing in SML, I followed http://rigaux.org/language-study/syntax-across-languages-per-language/SML.html.

I used https://www.smlnj.org/doc/smlnj-lib/Util/str-HashTable.html to learn about HashTables in SML, and used them to build the Symbol Table.

## Authors

Tanish Gupta - 2020CS10397
