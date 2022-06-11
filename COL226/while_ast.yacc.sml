functor while_astLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : while_ast_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
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

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\025\000\000\000\
\\001\000\001\000\063\000\000\000\
\\001\000\002\000\006\000\000\000\
\\001\000\003\000\024\000\000\000\
\\001\000\004\000\023\000\000\000\
\\001\000\005\000\003\000\000\000\
\\001\000\007\000\042\000\008\000\041\000\000\000\
\\001\000\009\000\035\000\024\000\034\000\025\000\033\000\026\000\032\000\
\\038\000\031\000\039\000\005\000\041\000\030\000\000\000\
\\001\000\010\000\078\000\022\000\057\000\023\000\056\000\027\000\055\000\
\\028\000\054\000\029\000\053\000\030\000\052\000\031\000\051\000\
\\032\000\050\000\033\000\049\000\034\000\048\000\035\000\047\000\
\\036\000\046\000\037\000\045\000\000\000\
\\001\000\011\000\012\000\000\000\
\\001\000\011\000\077\000\000\000\
\\001\000\011\000\079\000\000\000\
\\001\000\011\000\086\000\000\000\
\\001\000\012\000\026\000\000\000\
\\001\000\012\000\082\000\000\000\
\\001\000\012\000\083\000\000\000\
\\001\000\012\000\088\000\000\000\
\\001\000\016\000\062\000\022\000\057\000\023\000\056\000\027\000\055\000\
\\028\000\054\000\029\000\053\000\030\000\052\000\031\000\051\000\
\\032\000\050\000\033\000\049\000\034\000\048\000\035\000\047\000\
\\036\000\046\000\037\000\045\000\000\000\
\\001\000\017\000\085\000\000\000\
\\001\000\018\000\089\000\000\000\
\\001\000\020\000\058\000\022\000\057\000\023\000\056\000\027\000\055\000\
\\028\000\054\000\029\000\053\000\030\000\052\000\031\000\051\000\
\\032\000\050\000\033\000\049\000\034\000\048\000\035\000\047\000\
\\036\000\046\000\037\000\045\000\000\000\
\\001\000\021\000\084\000\000\000\
\\001\000\039\000\005\000\000\000\
\\001\000\040\000\000\000\000\000\
\\091\000\000\000\
\\092\000\000\000\
\\093\000\006\000\010\000\000\000\
\\094\000\000\000\
\\095\000\000\000\
\\096\000\000\000\
\\097\000\000\000\
\\098\000\000\000\
\\099\000\042\000\022\000\000\000\
\\100\000\000\000\
\\101\000\013\000\021\000\014\000\020\000\015\000\019\000\019\000\018\000\
\\039\000\005\000\000\000\
\\102\000\022\000\057\000\023\000\056\000\027\000\055\000\028\000\054\000\
\\029\000\053\000\030\000\052\000\031\000\051\000\032\000\050\000\
\\033\000\049\000\034\000\048\000\035\000\047\000\036\000\046\000\
\\037\000\045\000\000\000\
\\103\000\000\000\
\\104\000\022\000\057\000\023\000\056\000\027\000\055\000\028\000\054\000\
\\029\000\053\000\030\000\052\000\031\000\051\000\032\000\050\000\
\\033\000\049\000\034\000\048\000\035\000\047\000\036\000\046\000\
\\037\000\045\000\000\000\
\\105\000\000\000\
\\106\000\000\000\
\\107\000\000\000\
\\108\000\000\000\
\\109\000\000\000\
\\110\000\000\000\
\\111\000\033\000\049\000\034\000\048\000\035\000\047\000\036\000\046\000\
\\037\000\045\000\000\000\
\\112\000\033\000\049\000\034\000\048\000\035\000\047\000\036\000\046\000\
\\037\000\045\000\000\000\
\\113\000\033\000\049\000\034\000\048\000\035\000\047\000\036\000\046\000\
\\037\000\045\000\000\000\
\\114\000\033\000\049\000\034\000\048\000\035\000\047\000\036\000\046\000\
\\037\000\045\000\000\000\
\\115\000\033\000\049\000\034\000\048\000\035\000\047\000\036\000\046\000\
\\037\000\045\000\000\000\
\\116\000\033\000\049\000\034\000\048\000\035\000\047\000\036\000\046\000\
\\037\000\045\000\000\000\
\\117\000\027\000\055\000\028\000\054\000\029\000\053\000\030\000\052\000\
\\031\000\051\000\032\000\050\000\033\000\049\000\034\000\048\000\
\\035\000\047\000\036\000\046\000\037\000\045\000\000\000\
\\118\000\023\000\056\000\027\000\055\000\028\000\054\000\029\000\053\000\
\\030\000\052\000\031\000\051\000\032\000\050\000\033\000\049\000\
\\034\000\048\000\035\000\047\000\036\000\046\000\037\000\045\000\000\000\
\\119\000\000\000\
\\120\000\000\000\
\\121\000\000\000\
\\122\000\035\000\047\000\036\000\046\000\037\000\045\000\000\000\
\\123\000\035\000\047\000\036\000\046\000\037\000\045\000\000\000\
\\124\000\000\000\
\\125\000\000\000\
\\126\000\000\000\
\\127\000\000\000\
\\128\000\000\000\
\"
val actionRowNumbers =
"\005\000\022\000\002\000\060\000\
\\026\000\026\000\009\000\024\000\
\\022\000\027\000\034\000\032\000\
\\004\000\003\000\000\000\013\000\
\\007\000\007\000\007\000\022\000\
\\022\000\006\000\007\000\034\000\
\\025\000\042\000\043\000\020\000\
\\061\000\007\000\007\000\041\000\
\\040\000\007\000\017\000\037\000\
\\036\000\031\000\001\000\030\000\
\\029\000\035\000\033\000\007\000\
\\007\000\007\000\007\000\007\000\
\\007\000\007\000\007\000\007\000\
\\007\000\007\000\007\000\007\000\
\\010\000\054\000\053\000\008\000\
\\011\000\028\000\059\000\058\000\
\\057\000\056\000\055\000\049\000\
\\046\000\047\000\048\000\044\000\
\\045\000\050\000\051\000\034\000\
\\052\000\034\000\014\000\015\000\
\\021\000\018\000\039\000\012\000\
\\034\000\016\000\019\000\038\000\
\\023\000"
val gotoT =
"\
\\001\000\088\000\000\000\
\\010\000\002\000\000\000\
\\000\000\
\\000\000\
\\002\000\007\000\003\000\006\000\004\000\005\000\000\000\
\\003\000\009\000\004\000\005\000\000\000\
\\000\000\
\\000\000\
\\006\000\012\000\010\000\011\000\000\000\
\\000\000\
\\007\000\015\000\008\000\014\000\010\000\013\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\027\000\010\000\026\000\011\000\025\000\000\000\
\\009\000\034\000\010\000\026\000\011\000\025\000\000\000\
\\009\000\035\000\010\000\026\000\011\000\025\000\000\000\
\\010\000\036\000\000\000\
\\006\000\037\000\010\000\011\000\000\000\
\\005\000\038\000\000\000\
\\009\000\041\000\010\000\026\000\011\000\025\000\000\000\
\\007\000\042\000\008\000\014\000\010\000\013\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\057\000\010\000\026\000\011\000\025\000\000\000\
\\009\000\058\000\010\000\026\000\011\000\025\000\000\000\
\\000\000\
\\000\000\
\\009\000\059\000\010\000\026\000\011\000\025\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\062\000\010\000\026\000\011\000\025\000\000\000\
\\009\000\063\000\010\000\026\000\011\000\025\000\000\000\
\\009\000\064\000\010\000\026\000\011\000\025\000\000\000\
\\009\000\065\000\010\000\026\000\011\000\025\000\000\000\
\\009\000\066\000\010\000\026\000\011\000\025\000\000\000\
\\009\000\067\000\010\000\026\000\011\000\025\000\000\000\
\\009\000\068\000\010\000\026\000\011\000\025\000\000\000\
\\009\000\069\000\010\000\026\000\011\000\025\000\000\000\
\\009\000\070\000\010\000\026\000\011\000\025\000\000\000\
\\009\000\071\000\010\000\026\000\011\000\025\000\000\000\
\\009\000\072\000\010\000\026\000\011\000\025\000\000\000\
\\009\000\073\000\010\000\026\000\011\000\025\000\000\000\
\\009\000\074\000\010\000\026\000\011\000\025\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\078\000\008\000\014\000\010\000\013\000\000\000\
\\000\000\
\\007\000\079\000\008\000\014\000\010\000\013\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\085\000\008\000\014\000\010\000\013\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 89
val numrules = 38
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | NUMERAL of unit ->  (int) | IDENTIFIER of unit ->  (string)
 | VAR of unit ->  (string) | Number of unit ->  (ASTree.AST)
 | Variable of unit ->  (ASTree.AST)
 | Expression of unit ->  (ASTree.AST)
 | command of unit ->  (ASTree.AST)
 | CommandSeq of unit ->  (ASTree.AST)
 | VariableList of unit ->  (ASTree.AST) | dt of unit ->  (ASTree.AST)
 | Declaration of unit ->  (ASTree.AST)
 | DeclarationSeq of unit ->  (ASTree.AST)
 | BLOCK of unit ->  (ASTree.AST) | program of unit ->  (ASTree.AST)
end
type svalue = MlyValue.svalue
type result = ASTree.AST
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 39) => true | _ => false
val showTerminal =
fn (T 0) => "TERMINATOR"
  | (T 1) => "DEFINE"
  | (T 2) => "ASSIGNA"
  | (T 3) => "ASSIGNB"
  | (T 4) => "PROGRAM"
  | (T 5) => "VAR"
  | (T 6) => "INT"
  | (T 7) => "BOOL"
  | (T 8) => "LPAREN"
  | (T 9) => "RPAREN"
  | (T 10) => "LBRACE"
  | (T 11) => "RBRACE"
  | (T 12) => "Read"
  | (T 13) => "Write"
  | (T 14) => "IF"
  | (T 15) => "THEN"
  | (T 16) => "ELSE"
  | (T 17) => "ENDIF"
  | (T 18) => "WHILE"
  | (T 19) => "DO"
  | (T 20) => "ENDWH"
  | (T 21) => "OR"
  | (T 22) => "AND"
  | (T 23) => "TRUE"
  | (T 24) => "FALSE"
  | (T 25) => "NOT"
  | (T 26) => "LT"
  | (T 27) => "GT"
  | (T 28) => "GEQ"
  | (T 29) => "EQ"
  | (T 30) => "LEQ"
  | (T 31) => "NEQ"
  | (T 32) => "PLUS"
  | (T 33) => "MINUS"
  | (T 34) => "MULT"
  | (T 35) => "DIV"
  | (T 36) => "MOD"
  | (T 37) => "Negation"
  | (T 38) => "IDENTIFIER"
  | (T 39) => "EOF"
  | (T 40) => "NUMERAL"
  | (T 41) => "COMMA"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 41) $$ (T 39) $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33)
 $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26)
 $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19)
 $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12)
 $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 4) $$ 
(T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.BLOCK BLOCK1, _, BLOCK1right)) :: _ :: ( _,
 ( MlyValue.Variable Variable1, _, _)) :: ( _, ( _, PROGRAM1left, _))
 :: rest671)) => let val  result = MlyValue.program (fn _ => let val 
 (Variable as Variable1) = Variable1 ()
 val  (BLOCK as BLOCK1) = BLOCK1 ()
 in (
main(node(PROG, Variable, BLOCK), check_type(node(PROG, Variable, BLOCK)))
)
end)
 in ( LrTable.NT 0, ( result, PROGRAM1left, BLOCK1right), rest671)
end
|  ( 1, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.CommandSeq 
CommandSeq1, _, _)) :: _ :: ( _, ( MlyValue.DeclarationSeq 
DeclarationSeq1, DeclarationSeq1left, _)) :: rest671)) => let val  
result = MlyValue.BLOCK (fn _ => let val  (DeclarationSeq as 
DeclarationSeq1) = DeclarationSeq1 ()
 val  (CommandSeq as CommandSeq1) = CommandSeq1 ()
 in (ASTree.node(BLK, DeclarationSeq, CommandSeq))
end)
 in ( LrTable.NT 1, ( result, DeclarationSeq1left, RBRACE1right), 
rest671)
end
|  ( 2, ( rest671)) => let val  result = MlyValue.DeclarationSeq (fn _
 => (ASTree.node(DES, empty, empty)))
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 3, ( ( _, ( MlyValue.DeclarationSeq DeclarationSeq1, _, 
DeclarationSeq1right)) :: ( _, ( MlyValue.Declaration Declaration1, 
Declaration1left, _)) :: rest671)) => let val  result = 
MlyValue.DeclarationSeq (fn _ => let val  (Declaration as Declaration1
) = Declaration1 ()
 val  (DeclarationSeq as DeclarationSeq1) = DeclarationSeq1 ()
 in (ASTree.node(DES, Declaration, DeclarationSeq))
end)
 in ( LrTable.NT 2, ( result, Declaration1left, DeclarationSeq1right),
 rest671)
end
|  ( 4, ( ( _, ( _, _, TERMINATOR1right)) :: ( _, ( MlyValue.dt dt1, _
, _)) :: _ :: ( _, ( MlyValue.VariableList VariableList1, _, _)) :: (
 _, ( MlyValue.VAR VAR1, VAR1left, _)) :: rest671)) => let val  result
 = MlyValue.Declaration (fn _ => let val  VAR1 = VAR1 ()
 val  (VariableList as VariableList1) = VariableList1 ()
 val  (dt as dt1) = dt1 ()
 in (insert_dec(ASTree.node(DE, VariableList, dt)))
end)
 in ( LrTable.NT 3, ( result, VAR1left, TERMINATOR1right), rest671)

end
|  ( 5, ( ( _, ( _, INT1left, INT1right)) :: rest671)) => let val  
result = MlyValue.dt (fn _ => (ASTree.node(INT, empty, empty)))
 in ( LrTable.NT 4, ( result, INT1left, INT1right), rest671)
end
|  ( 6, ( ( _, ( _, BOOL1left, BOOL1right)) :: rest671)) => let val  
result = MlyValue.dt (fn _ => (ASTree.node(BOOL, empty,empty)))
 in ( LrTable.NT 4, ( result, BOOL1left, BOOL1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.VariableList VariableList1, _, 
VariableList1right)) :: _ :: ( _, ( MlyValue.Variable Variable1, 
Variable1left, _)) :: rest671)) => let val  result = 
MlyValue.VariableList (fn _ => let val  (Variable as Variable1) = 
Variable1 ()
 val  (VariableList as VariableList1) = VariableList1 ()
 in (ASTree.node(VL, Variable, VariableList))
end)
 in ( LrTable.NT 5, ( result, Variable1left, VariableList1right), 
rest671)
end
|  ( 8, ( ( _, ( MlyValue.Variable Variable1, Variable1left, 
Variable1right)) :: rest671)) => let val  result = 
MlyValue.VariableList (fn _ => let val  (Variable as Variable1) = 
Variable1 ()
 in (ASTree.node(VL, Variable, empty))
end)
 in ( LrTable.NT 5, ( result, Variable1left, Variable1right), rest671)

end
|  ( 9, ( ( _, ( MlyValue.CommandSeq CommandSeq1, _, CommandSeq1right)
) :: _ :: ( _, ( MlyValue.command command1, command1left, _)) :: 
rest671)) => let val  result = MlyValue.CommandSeq (fn _ => let val  (
command as command1) = command1 ()
 val  (CommandSeq as CommandSeq1) = CommandSeq1 ()
 in (ASTree.node(SEQ, command, CommandSeq))
end)
 in ( LrTable.NT 6, ( result, command1left, CommandSeq1right), rest671
)
end
|  ( 10, ( rest671)) => let val  result = MlyValue.CommandSeq (fn _ =>
 (ASTree.node(SEQ, empty, empty)))
 in ( LrTable.NT 6, ( result, defaultPos, defaultPos), rest671)
end
|  ( 11, ( ( _, ( MlyValue.Expression Expression1, _, Expression1right
)) :: _ :: ( _, ( MlyValue.Variable Variable1, Variable1left, _)) :: 
rest671)) => let val  result = MlyValue.command (fn _ => let val  (
Variable as Variable1) = Variable1 ()
 val  (Expression as Expression1) = Expression1 ()
 in (ASTree.node(SET, Variable, Expression))
end)
 in ( LrTable.NT 7, ( result, Variable1left, Expression1right), 
rest671)
end
|  ( 12, ( ( _, ( MlyValue.Variable Variable1, _, Variable1right)) :: 
( _, ( _, Read1left, _)) :: rest671)) => let val  result = 
MlyValue.command (fn _ => let val  (Variable as Variable1) = Variable1
 ()
 in (ASTree.node(READ, Variable, empty))
end)
 in ( LrTable.NT 7, ( result, Read1left, Variable1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.Expression Expression1, _, Expression1right
)) :: ( _, ( _, Write1left, _)) :: rest671)) => let val  result = 
MlyValue.command (fn _ => let val  (Expression as Expression1) = 
Expression1 ()
 in (ASTree.node(WRITE, Expression, empty))
end)
 in ( LrTable.NT 7, ( result, Write1left, Expression1right), rest671)

end
|  ( 14, ( ( _, ( _, _, ENDIF1right)) :: _ :: ( _, ( 
MlyValue.CommandSeq CommandSeq2, _, _)) :: _ :: _ :: _ :: ( _, ( 
MlyValue.CommandSeq CommandSeq1, _, _)) :: _ :: _ :: ( _, ( 
MlyValue.Expression Expression1, _, _)) :: ( _, ( _, IF1left, _)) :: 
rest671)) => let val  result = MlyValue.command (fn _ => let val  (
Expression as Expression1) = Expression1 ()
 val  CommandSeq1 = CommandSeq1 ()
 val  CommandSeq2 = CommandSeq2 ()
 in (ASTree.node3(ITE, Expression, CommandSeq1, CommandSeq2))
end)
 in ( LrTable.NT 7, ( result, IF1left, ENDIF1right), rest671)
end
|  ( 15, ( ( _, ( _, _, ENDWH1right)) :: _ :: ( _, ( 
MlyValue.CommandSeq CommandSeq1, _, _)) :: _ :: _ :: ( _, ( 
MlyValue.Expression Expression1, _, _)) :: ( _, ( _, WHILE1left, _))
 :: rest671)) => let val  result = MlyValue.command (fn _ => let val 
 (Expression as Expression1) = Expression1 ()
 val  (CommandSeq as CommandSeq1) = CommandSeq1 ()
 in (ASTree.node(WH, Expression, CommandSeq))
end)
 in ( LrTable.NT 7, ( result, WHILE1left, ENDWH1right), rest671)
end
|  ( 16, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  
result = MlyValue.Expression (fn _ => (
ASTree.node(ASTree.TT, empty, empty)))
 in ( LrTable.NT 8, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 17, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let
 val  result = MlyValue.Expression (fn _ => (
ASTree.node(ASTree.FF, empty, empty)))
 in ( LrTable.NT 8, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.Number Number1, Number1left, Number1right))
 :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  (Number as Number1) = Number1 ()
 in (ASTree.node(ASTree.NUM, Number, empty))
end)
 in ( LrTable.NT 8, ( result, Number1left, Number1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.Variable Variable1, Variable1left, 
Variable1right)) :: rest671)) => let val  result = MlyValue.Expression
 (fn _ => let val  (Variable as Variable1) = Variable1 ()
 in (ASTree.node(ASTree.Var, Variable, empty))
end)
 in ( LrTable.NT 8, ( result, Variable1left, Variable1right), rest671)

end
|  ( 20, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (ASTree.node(ASTree.GT, Expression1, Expression2))
end)
 in ( LrTable.NT 8, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 21, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (ASTree.node(ASTree.LT, Expression1, Expression2))
end)
 in ( LrTable.NT 8, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 22, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (ASTree.node(ASTree.LEQ, Expression1, Expression2))
end)
 in ( LrTable.NT 8, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 23, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (ASTree.node(ASTree.EQ, Expression1, Expression2))
end)
 in ( LrTable.NT 8, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 24, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (ASTree.node(ASTree.GEQ, Expression1, Expression2))
end)
 in ( LrTable.NT 8, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 25, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (ASTree.node(ASTree.NEQ, Expression1, Expression2))
end)
 in ( LrTable.NT 8, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 26, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (ASTree.node(ASTree.AND, Expression1, Expression2))
end)
 in ( LrTable.NT 8, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 27, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (ASTree.node(ASTree.OR, Expression1, Expression2))
end)
 in ( LrTable.NT 8, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 28, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.Expression 
Expression1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.Expression (fn _ => let val  (Expression as 
Expression1) = Expression1 ()
 in (Expression)
end)
 in ( LrTable.NT 8, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.Expression Expression1, _, Expression1right
)) :: ( _, ( _, NOT1left, _)) :: rest671)) => let val  result = 
MlyValue.Expression (fn _ => let val  (Expression as Expression1) = 
Expression1 ()
 in (ASTree.node(ASTree.NOT, Expression, empty))
end)
 in ( LrTable.NT 8, ( result, NOT1left, Expression1right), rest671)

end
|  ( 30, ( ( _, ( MlyValue.Expression Expression1, _, Expression1right
)) :: ( _, ( _, Negation1left, _)) :: rest671)) => let val  result = 
MlyValue.Expression (fn _ => let val  (Expression as Expression1) = 
Expression1 ()
 in (ASTree.node(ASTree.NEGATION, Expression, empty))
end)
 in ( LrTable.NT 8, ( result, Negation1left, Expression1right), 
rest671)
end
|  ( 31, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (ASTree.node(ASTree.PLUS, Expression1, Expression2))
end)
 in ( LrTable.NT 8, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 32, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (ASTree.node(ASTree.MINUS, Expression1, Expression2))
end)
 in ( LrTable.NT 8, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 33, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (ASTree.node(ASTree.TIMES, Expression1, Expression2))
end)
 in ( LrTable.NT 8, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 34, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (ASTree.node(ASTree.DIV, Expression1, Expression2))
end)
 in ( LrTable.NT 8, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 35, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (ASTree.node(ASTree.MOD, Expression1, Expression2))
end)
 in ( LrTable.NT 8, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 36, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, IDENTIFIER1left, 
IDENTIFIER1right)) :: rest671)) => let val  result = MlyValue.Variable
 (fn _ => let val  (IDENTIFIER as IDENTIFIER1) = IDENTIFIER1 ()
 in (ASTree.idnode(ASTree.ID, IDENTIFIER, empty))
end)
 in ( LrTable.NT 9, ( result, IDENTIFIER1left, IDENTIFIER1right), 
rest671)
end
|  ( 37, ( ( _, ( MlyValue.NUMERAL NUMERAL1, NUMERAL1left, 
NUMERAL1right)) :: rest671)) => let val  result = MlyValue.Number (fn
 _ => let val  (NUMERAL as NUMERAL1) = NUMERAL1 ()
 in (ASTree.intnode(ASTree.NUM, NUMERAL, empty))
end)
 in ( LrTable.NT 10, ( result, NUMERAL1left, NUMERAL1right), rest671)

end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.program x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : while_ast_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun TERMINATOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun DEFINE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGNA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGNB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun PROGRAM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VAR (fn () => i),p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun Read (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun Write (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDIF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDWH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun TRUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun FALSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun GEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun LEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun MULT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun MOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun Negation (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun IDENTIFIER (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.IDENTIFIER (fn () => i),p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun NUMERAL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.NUMERAL (fn () => i),p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
end
end
