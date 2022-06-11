# The VMC machine for the ASTs of WHILE Language (A Toy Language)

## Objective


The objective of this assignment is to execute the ASTs on a VMC machine by first transforming each program as
a sequence of operations in post-fix form.


## Implementation details

The assignment directory contains the following files:

1. AST.sml => This file contains the datatype AST. This is implemented using a structure (which is called ASTree). This structured is then opened in different files, as and when needed.
2. while_ast.lex => This is the lexer file, written in ML-Lex. This is same as submitted in assignment 3. Upon running the command 

    
        $ ml-lex while_ast.lex
  
  while_ast.lex.sml file is generated.
  
3. white_ast.yacc => This is the yacc file, written in ML-YACC. The datatype definition of AST, which was previously included in this file in assignment 3 is moved to AST.sml file now. This is done to conveniently link the same datatype in other files too.
4. while_ast.cm => This file in the SMLNJ Compilation Manager (CM). 
5. load-while.sml => This file "joins" the different structures of lex and yacc file. Originally, this file also contained the logic of parsing Function, which has now been moved to compiler.sml for convenience.
6. compiler.sml => This file contains the user interaction part. It implements a structure "While_AST" that implements a function called main.

        main : string -> ((stackDataType FunStack.Stack) * (int Array.array) * (stackDataType FunStack.Stack)) 

7. VMC.sml => This file is the crux of this assignment. This file contains various functions and implementations. Firstly, the FunStack structure of STACK signature is implemented. A symbol Table is then defined which stores the address of memory location of various variables. A function called postfix is implemented, which takes in an AST and outputs a Stack, with all commands in postfix form. During this, the symbol table is also filled using auxiliary functions.

        postfix : (AST * stackDataType FunStack.Stack) -> stackDataType FunStack.Stack
     
Finally, this file contains the signature VMC that defines the function "rules". The structure Vmc of this signature implements the function rules.

      rules : (('a FunStack.Stack) * (int Array.array) * ('a FunStack.Stack)) -> (('a FunStack.Stack) * (int Array.array) * ('a FunStack.Stack))
      
Some auxiliary functions have been defined next to help evaluate the whole of the program. The function helpExecute calls itself recursively unless the stack C becomes empty. The function evaluate calls this function helpExecute. The toString function helps to visualize the stacks and the memory in a better way at each instant.

       toString: (('a FunStack.Stack) * (int Array.array) * ('a FunStack.Stack)) -> string
       
       
The main directory also contains a folder by the name of TestCases which includes the test cases on which the assignment was tested. Some testcases intentionally contain errors to test the code exhaustively.

       
## Using the compiler

The following commands need to be run to use this code:
    
      $ sml
      - CM.make "while_ast.cm";
      - While_AST.main "$(name_of_test_file)";
      
Firsly, we enter the REPL environment of SML by typing sml. Then we run the compilation manager file, which helps in executing all the files. After this, we can directly call the main function defined in the While_AST structure. 

To run the evaluate function for an AST, use

    - Vmc.evaluate(filename);

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

## Other Design Decisions

1. To conveniently use the same datatypes in YACC file as well as in the VMC.sml file, I had to make significant changes in the YACC file. So, the YACC file submitted now is somewhat different than originally submittes (Mainly in the topmost section, though).
2. Some "rules" were modified and others were added to implement the structure correctly. As an example, there was no rule to deal with unary operators, which has to be then added.

## Other Implementation Decisions

  The final output is a 3-Tuple. Presently, I am not commenting the code which helps in visualizing the stacks and memory at every instant. This can be easily commented.


## Acknowledgements

For learning ML-Lex and ML-YACC, I used the following resources: 

  http://www.rogerprice.org/ug/ug.pdf
  
  https://www.smlnj.org/doc/ML-Lex/manual.html
  
  https://www.smlnj.org/doc/CM/Old/index.html
  
  https://www.cs.princeton.edu/~appel/modern/ml/ml-yacc/manual.html
  
  https://www.smlnj.org/doc/ML-Yacc/mlyacc007.html
  
To learn about referencing and deferencing in SML, I followed http://rigaux.org/language-study/syntax-across-languages-per-language/SML.html.

I used https://www.smlnj.org/doc/smlnj-lib/Util/str-HashTable.html to learn about HashTables in SML, and used them to build the Symbol Table.

## For IITD Students

This compiler was made as a part of an assignment for COL226, II Semester, 2021-22 under Prof S. Arun Kumar. Copy at your own risk :)

## Authors

Tanish Gupta - 2020CS10397
