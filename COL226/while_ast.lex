structure Tokens = Tokens

    type pos = int 
    type svalue = Tokens.svalue

    type ('a, 'b) token = ('a, 'b) Tokens.token
    type lexresult = (svalue, pos) token

    exception badChar
    val pos = ref 1 
    val characters_inline = ref 1
    
    val output = ref "["
    val eof = fn fileName => 
                                                 Tokens.EOF(!pos, !characters_inline);
    val error: string*int*int -> unit = fn (e, l1, l2) => TextIO.output(TextIO.stdOut,"lex:line " ^ Int.toString l1 ^ " l2=" ^ Int.toString l2 ^ ": "^ e ^"\n")

%%

%header (functor WhileLexFun(structure Tokens: while_ast_TOKENS));

identifier = [A-Za-z][A-Za-z0-9]*;
linebreak = [\ \t\n];
numeral = [0-9][0-9]*;
sign = [+~];
%%

";"     =>  (characters_inline := 1 ; pos := (!pos) + 1;  output := !output ^ "TERMINATOR ; , " ; Tokens.TERMINATOR(!pos, !characters_inline));
{linebreak} => (characters_inline := !characters_inline + 1; lex());
"::" => (characters_inline := !characters_inline + 2; output := !output ^ "DEFINE ::, " ; Tokens.DEFINE(!pos, !characters_inline));
":" => (characters_inline := !characters_inline + 1; output := !output ^ "Assignment :, " ; Tokens.ASSIGNB(!pos, !characters_inline));
"program" => (characters_inline := !characters_inline + 7; output := !output ^ "Program program, "; Tokens.PROGRAM(!pos, !characters_inline));
"var" => (characters_inline := !characters_inline + 3; output := !output ^ "var var, "; Tokens.VAR(yytext, !pos, !characters_inline));
"int" => (characters_inline := !characters_inline + 3; output := !output ^ "DataType int, "; Tokens.INT(!pos, !characters_inline));
"bool" => (characters_inline := !characters_inline + 4; output := !output ^ "DataType bool, "; Tokens.BOOL(!pos, !characters_inline));
"{" => (characters_inline := !characters_inline + 1; output := !output ^ "Lparen {, "; Tokens.LBRACE(!pos, !characters_inline));
"}" => (characters_inline := !characters_inline + 1; output := !output ^ "Rparen }, "; Tokens.RBRACE( !pos, !characters_inline));
"(" => (characters_inline := !characters_inline + 1; output := !output ^ "Lparen (, "; Tokens.LPAREN(!pos, !characters_inline));
")" => (characters_inline := !characters_inline + 1; output := !output ^ "Rparen ), "; Tokens.RPAREN(!pos, !characters_inline));
":=" => (characters_inline := !characters_inline + 2; output := !output ^ "Assignment := ," ; Tokens.ASSIGNA(!pos, !characters_inline));
"read" => (characters_inline := !characters_inline + 4; output := !output ^ "Read read, "; Tokens.Read(!pos, !characters_inline));
"write" => (characters_inline := !characters_inline + 5; output := !output ^ "Write write, "; Tokens.Write(!pos, !characters_inline));
"if" => (characters_inline := !characters_inline + 2; output := !output ^ "If if, "; Tokens.IF(!pos, !characters_inline));
"then" => (characters_inline := !characters_inline + 4; output := !output ^ "Then then, "; Tokens.THEN(!pos, !characters_inline));
"else" => (characters_inline := !characters_inline + 4; output := !output ^ "Else else, "; Tokens.ELSE(!pos, !characters_inline));
"endif" => (characters_inline := !characters_inline + 5; output := !output ^ "Endif endif, "; Tokens.ENDIF(!pos, !characters_inline));
"while" => (characters_inline := !characters_inline + 5; output := !output ^ "While while, "; Tokens.WHILE(!pos, !characters_inline));
"do" => (characters_inline := !characters_inline + 2; output := !output ^ "Do do, "; Tokens.DO(!pos, !characters_inline));
"endwh" => (characters_inline := !characters_inline + 5; output := !output ^ "Endwh endwh, "; Tokens.ENDWH(!pos, !characters_inline));
"||" => (characters_inline := !characters_inline + 1; output := !output ^ "OR ||, "; Tokens.OR(!pos, !characters_inline));
"&&" => (characters_inline := !characters_inline + 1; output := !output ^ "AND &&, "; Tokens.AND(!pos, !characters_inline));
"tt" => (characters_inline := !characters_inline + 2; output := !output ^ "TRUE tt, "; Tokens.TRUE(!pos, !characters_inline));
"ff" => (characters_inline := !characters_inline + 2; output := !output ^ "FALSE ff, "; Tokens.FALSE(!pos, !characters_inline));
"!" => (characters_inline := !characters_inline + 1; output := !output ^ "NOT !, "; Tokens.NOT(!pos, !characters_inline));
"<" => (characters_inline := !characters_inline + 1; output := !output ^ "RelOP <, "; Tokens.LT( !pos, !characters_inline));
"<=" => (characters_inline := !characters_inline + 2; output := !output ^ "RelOP <=, "; Tokens.LEQ(!pos, !characters_inline));
"=" => (characters_inline := !characters_inline + 1; output := !output ^ "RelOP =, "; Tokens.EQ(!pos, !characters_inline));
">" => (characters_inline := !characters_inline + 1; output := !output ^ "RelOP >, "; Tokens.GT(!pos, !characters_inline));
">=" => (characters_inline := !characters_inline + 2; output := !output ^ "RelOP >=, "; Tokens.GEQ(!pos, !characters_inline));
"<>" => (characters_inline := !characters_inline + 2; output := !output ^ "RelOP <>, "; Tokens.NEQ(!pos, !characters_inline));
"+" => (characters_inline := !characters_inline + 1; output := !output ^ "AddOP +, "; Tokens.PLUS(!pos, !characters_inline));
"-" => (characters_inline := !characters_inline + 1; output := !output ^ "AddOP -, "; Tokens.MINUS(!pos, !characters_inline));
"~" => (characters_inline := !characters_inline + 1; output := !output ^ "Negation ~, "; Tokens.Negation(!pos, !characters_inline));
"*" => (characters_inline := !characters_inline + 1; output := !output ^ "MultOP *, "; Tokens.MULT(!pos, !characters_inline));
"/" => (characters_inline := !characters_inline + 1; output := !output ^ "MultOP /, "; Tokens.DIV(!pos, !characters_inline));
"%" => (characters_inline := !characters_inline + 1; output := !output ^ "MultOP %, "; Tokens.MOD(!pos, !characters_inline));
"," => (characters_inline := !characters_inline + 1; output := !output ^ "Comma , , "; Tokens.COMMA(!pos, !characters_inline));
{identifier} => (characters_inline := !characters_inline + size(yytext); output := !output ^ "ID " ^ yytext ^ ", "; Tokens.IDENTIFIER(yytext, !pos, !characters_inline));
{sign}?{numeral} => (characters_inline := !characters_inline + size(yytext); output := !output ^ "Numeral " ^ yytext ^ ", "; Tokens.NUMERAL(valOf(Int.fromString(yytext)), !pos, !characters_inline));
. => (error(yytext, !pos, !characters_inline);  raise badChar; lex());