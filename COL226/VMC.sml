exception Error of string;
signature STACK =
sig
    type 'a Stack
    exception EmptyStack
    exception Error of string
    val create: 'a Stack
    val push : 'a * 'a Stack -> 'a Stack
    val pop : 'a Stack -> 'a Stack
    val top : 'a Stack -> 'a
    val empty: 'a Stack -> bool
    val poptop : 'a Stack -> ('a * 'a Stack) option
    val nth : 'a Stack * int -> 'a
    val drop : 'a Stack * int -> 'a Stack
    val depth : 'a Stack -> int
    val app : ('a -> unit) -> 'a Stack -> unit
    val map : ('a -> 'b) -> 'a Stack -> 'b Stack
    val mapPartial : ('a -> 'b option) -> 'a Stack -> 'b Stack
    val find : ('a -> bool) -> 'a Stack -> 'a option
    val filter : ('a -> bool) -> 'a Stack -> 'a Stack
    val foldr : ('a * 'b -> 'b) -> 'b -> 'a Stack -> 'b
    val foldl : ('a * 'b -> 'b) -> 'b -> 'a Stack -> 'b
    val exists : ('a -> bool) -> 'a Stack -> bool
    val all : ('a -> bool) -> 'a Stack -> bool
    val list2stack : 'a list -> 'a Stack (* Convert a list into a Stack *)
    val stack2list: 'a Stack -> 'a list (* Convert a Stack into a list *)
    val toString: ('a -> string) -> 'a Stack -> string
end

(*According to my implementation, I push in the front of list and also pop from front of list*)
structure FunStack :> STACK =
struct
    type 'a Stack = 'a list;
    exception EmptyStack
    exception Error of string
    val create = [];
    fun push(x, s) = x::s;
    fun pop([]) = raise EmptyStack
         |  pop(x::s) = s;
    fun top([]) = raise EmptyStack
          | top(x::s) = x;
    fun empty([]) = true
          | empty(x::s) = false;
    fun poptop(s) = List.getItem(s);
    fun nth(s, x) = List.nth(s, x);
    fun drop(s, x) = List.drop(s, x);
    fun depth(s) = List.length(s);
    fun app(f) s = List.app(f) s;
    fun map(f) s = List.map(f) s;
    fun mapPartial(f) s = List.mapPartial(f) s;
    fun find(f) s = List.find(f) s;
    fun filter(f) s = List.filter(f) s;
    fun foldl (f) x s = List.foldl(f) x s;
    fun foldr (f) x s = List.foldr(f) x s;
    fun exists(f) s = List.exists(f) s;
    fun all(f) s = List.all(f) s;
    fun list2stack(l) = l;
    fun stack2list(s) = s;
    fun toString(f) s = 
        let 
            fun helper(func, [], str) = str
                  | helper(func, tp :: stk, str) = helper(func, stk, str ^ func(tp));
        in
            helper(f, s, "")
        end;
end


(*
fun value "+" = PLUS 
    | value "-" = MINUS
    | value "*" = TIMES
    | value "/" = DIV
    | value "%" = MOD
    | value "<" = LT 
    | value "<=" = LEQ
    | value ">" = GT
    | value ">" = GEQ
    | value "=" = EQ
    | value "<>" = NEQ
    | value "int" =   INT
    | value "bool" = BOOL

*)
datatype stackDataType = INTEGER of int | VARIABLE of string| Exp of stackDataType FunStack.Stack | commandSeq of stackDataType FunStack.Stack | Set | Seq | Ite |Wh | Gt | Lt | Leq | Geq | Eq | Neq | Plus | Minus | Times | And | Or | Div | Mod | True | False | Nothing | Read of string| Write of stackDataType FunStack.Stack | Not | Negation;

val symbTable : (string, int) HashTable.hash_table = HashTable.mkTable (HashString.hashString, op=) (100, Fail "not found");
(*This will store address of each variable in the memory array*)


fun postfix(ASTree.node(ASTree.PLUS, exp1, exp2), s: stackDataType FunStack.Stack) = FunStack.push(Exp(postfix(exp1, FunStack.list2stack([]))), (FunStack.push(Exp(postfix(exp2, FunStack.list2stack([]))) , FunStack.push(Plus, s))))
     | postfix(ASTree.node(ASTree.MINUS, exp1, exp2) ,s: stackDataType FunStack.Stack) = FunStack.push(Exp(postfix(exp1, FunStack.list2stack([]))), (FunStack.push(Exp(postfix(exp2, FunStack.list2stack([]))) , FunStack.push(Minus, s))))
     | postfix(ASTree.node(ASTree.TIMES, exp1, exp2) ,s: stackDataType FunStack.Stack) = FunStack.push(Exp(postfix(exp1, FunStack.list2stack([]))), (FunStack.push(Exp(postfix(exp2, FunStack.list2stack([]))) , FunStack.push(Times, s))))
     | postfix(ASTree.node(ASTree.DIV, exp1, exp2) ,s: stackDataType FunStack.Stack) = FunStack.push(Exp(postfix(exp1, FunStack.list2stack([]))), (FunStack.push(Exp(postfix(exp2, FunStack.list2stack([]))) , FunStack.push(Div, s))))
     | postfix(ASTree.node(ASTree.MOD, exp1, exp2) ,s: stackDataType FunStack.Stack) = FunStack.push(Exp(postfix(exp1, FunStack.list2stack([]))), (FunStack.push(Exp(postfix(exp2, FunStack.list2stack([]))) , FunStack.push(Mod, s))))
     | postfix(ASTree.node(ASTree.AND, exp1, exp2) ,s: stackDataType FunStack.Stack) = FunStack.push(Exp(postfix(exp1, FunStack.list2stack([]))), (FunStack.push(Exp(postfix(exp2, FunStack.list2stack([]))) , FunStack.push(And, s))))
     | postfix(ASTree.node(ASTree.OR, exp1, exp2) ,s: stackDataType FunStack.Stack) = FunStack.push(Exp(postfix(exp1, FunStack.list2stack([]))), (FunStack.push(Exp(postfix(exp2, FunStack.list2stack([]))) , FunStack.push(Or, s))))
     | postfix(ASTree.node(ASTree.LT, exp1, exp2) ,s: stackDataType FunStack.Stack) = FunStack.push(Exp(postfix(exp1, FunStack.list2stack([]))), (FunStack.push(Exp(postfix(exp2, FunStack.list2stack([]))) , FunStack.push(Lt, s))))
     | postfix(ASTree.node(ASTree.GT, exp1, exp2) ,s: stackDataType FunStack.Stack) = FunStack.push(Exp(postfix(exp1, FunStack.list2stack([]))), (FunStack.push(Exp(postfix(exp2, FunStack.list2stack([]))) , FunStack.push(Gt, s))))
     | postfix(ASTree.node(ASTree.GEQ, exp1, exp2) ,s: stackDataType FunStack.Stack) = FunStack.push(Exp(postfix(exp1, FunStack.list2stack([]))), (FunStack.push(Exp(postfix(exp2, FunStack.list2stack([]))) , FunStack.push(Geq, s))))
     | postfix(ASTree.node(ASTree.LEQ, exp1, exp2) ,s: stackDataType FunStack.Stack) = FunStack.push(Exp(postfix(exp1, FunStack.list2stack([]))), (FunStack.push(Exp(postfix(exp2, FunStack.list2stack([]))) , FunStack.push(Leq, s))))
     | postfix(ASTree.node(ASTree.EQ, exp1, exp2) ,s: stackDataType FunStack.Stack) = FunStack.push(Exp(postfix(exp1, FunStack.list2stack([]))), (FunStack.push(Exp(postfix(exp2, FunStack.list2stack([]))) , FunStack.push(Eq, s))))
     | postfix(ASTree.node(ASTree.NEQ, exp1, exp2) ,s: stackDataType FunStack.Stack) = FunStack.push(Exp(postfix(exp1, FunStack.list2stack([]))), (FunStack.push(Exp(postfix(exp2, FunStack.list2stack([]))) , FunStack.push(Neq, s))))
     | postfix(ASTree.node(ASTree.TT, ASTree.empty, ASTree.empty), s: stackDataType FunStack.Stack) = FunStack.push(True, s)
     | postfix(ASTree.node(ASTree.FF, ASTree.empty, ASTree.empty), s: stackDataType FunStack.Stack) = FunStack.push(False, s)
     | postfix(ASTree.intnode(ASTree.NUM, x, ASTree.empty), s: stackDataType FunStack.Stack) = FunStack.push(INTEGER(x), s)
     | postfix(ASTree.node(ASTree.NUM, ast1, ast2), s: stackDataType FunStack.Stack) = postfix(ast1, s)
     | postfix(ASTree.node(ASTree.PROG, ast1, ast2), s: stackDataType FunStack.Stack) = postfix(ast2, s)
     | postfix(ASTree.node(ASTree.BLK, ast1, ast2), s: stackDataType FunStack.Stack) =   fillSymbTable(ast1, ast2, s, 0)
     | postfix(ASTree.node(ASTree.SEQ, ast1, ast2), s: stackDataType FunStack.Stack) = commandSeqStack(ast1, ast2, FunStack.list2stack([]))
     | postfix(ASTree.node(ASTree.SET, ASTree.idnode(ID, x, ASTree.empty), ast2), s: stackDataType FunStack.Stack) = FunStack.push(VARIABLE(x), FunStack.push(Exp(postfix(ast2, FunStack.list2stack([]))), FunStack.push(Set,s)))
     | postfix(ASTree.node(ASTree.DES, _, _), s: stackDataType FunStack.Stack) = s (*Should never reach here!!*)
     | postfix(ASTree.node(ASTree.Var, ASTree.idnode(ID, x, ASTree.empty), ast2), s: stackDataType FunStack.Stack) = FunStack.push(VARIABLE(x), s)
     | postfix(ASTree.node(ASTree.READ, ASTree.idnode(ID, x, ASTree.empty), ASTree.empty), s: stackDataType FunStack.Stack) = FunStack.push(Read(x), s)
     | postfix(ASTree.node(ASTree.WRITE, x, _) ,s: stackDataType FunStack.Stack) = FunStack.push(Write((postfix(x, FunStack.list2stack([])))), s)
     | postfix(ASTree.node3(ASTree.ITE, exp1, exp2, exp3), s: stackDataType FunStack.Stack) = FunStack.push(Exp(postfix(exp1, FunStack.list2stack([]))), FunStack.push(commandSeq(postfix(exp2, FunStack.list2stack([]))), FunStack.push(commandSeq(postfix(exp3, FunStack.list2stack([]))), FunStack.push(Ite, s))))
     | postfix(ASTree.node(ASTree.WH, exp1, exp2) ,s: stackDataType FunStack.Stack) = FunStack.push(Exp(postfix(exp1, FunStack.list2stack([]))), FunStack.push(commandSeq(postfix(exp2, FunStack.list2stack([]))), FunStack.push(Wh, s)))
     | postfix(ASTree.node(ASTree.NOT, exp1, ASTree.empty), s: stackDataType FunStack.Stack) = FunStack.push(Not, FunStack.push(Exp(postfix(exp1, FunStack.list2stack([]))), s))
     | postfix(ASTree.node(ASTree.NEGATION, exp1, ASTree.empty) ,s: stackDataType FunStack.Stack) = FunStack.push(Negation, FunStack.push(Exp(postfix(exp1, FunStack.list2stack([]))), s))
     | postfix(ASTree.empty, _) = FunStack.create
     | postfix(_, _) = raise Error("Something went wrong! Please check your program again!")

and
    commandSeqStack(ast1, ASTree.empty, s) =
        FunStack.push(commandSeq(FunStack.list2stack([])), s)
|    commandSeqStack(ast1, ASTree.node(SEQ, ASTree.empty, ASTree.empty), s) = 
        FunStack.push(commandSeq(postfix(ast1, FunStack.list2stack([]))), s)
|   commandSeqStack(ast1, ASTree.node(SEQ, ast2, ast3), s) = 
    let
        val x = commandSeqStack(ast2, ast3, s)
    in
        FunStack.push(commandSeq(postfix(ast1, FunStack.list2stack([]))), x)
    end

and
    fillSymbTable(ASTree.node(DES, ASTree.node(DE, VList, ASTree.node(_, ASTree.empty, ASTree.empty)), DES2), ast2, s, i) = 
        let
            val nextIdx = fillVL (VList, i)
        in
            fillSymbTable(DES2, ast2, s, nextIdx)
        end
|   fillSymbTable(ASTree.node(DES, ASTree.empty, ASTree.empty), ast2, s, _) = postfix(ast2, s)

and
    fillVL (ASTree.node(VL, ASTree.idnode(ID, v, ASTree.empty) , VL2), i) =
        let 
            val notMatters = HashTable.insert(symbTable) (v, i)
        in
            fillVL (VL2, i+1)
        end
|   fillVL (ASTree.empty, i) = i
(*Even if I have one command in command sequence, I push it as commandSeq(stack)*)

(* and  *)
    (* main(_, ast2, s) = 
        let
            val typec = check_type(ast2)
            val x = print(Bool.toString(typec))
        in
            if(not(typec)) then  postfix(ast2, s)
            else raise Error("Type Mismatch :(")
        end *)



signature VMC = 
sig
    exception Error of string;
    val rules : (('a FunStack.Stack) * (int Array.array) * ('a FunStack.Stack)) -> (('a FunStack.Stack) * (int Array.array) * ('a FunStack.Stack)) 
    val helpExecute: (('a FunStack.Stack) * (int Array.array) * ('a FunStack.Stack)) -> (('a FunStack.Stack) * (int Array.array) * ('a FunStack.Stack)) 
    val execute: 'a FunStack.Stack -> (('a FunStack.Stack) * (int Array.array) * ('a FunStack.Stack)) 
    val toString: (('a FunStack.Stack) * (int Array.array) * ('a FunStack.Stack)) -> string
end;

structure Vmc = 
struct
    exception Error of string
    fun  rules((V, M: int array, C)) = 
        let
            val tp = 
                if(FunStack.depth(C) = 0) then
                    Nothing
                else
                    FunStack.top(C);
            fun typeof(INTEGER(n)) = "INTEGER"
                 | typeof(VARIABLE(x)) = "VARIABLE"
                 | typeof(And) = "BINOP"
                 | typeof(Or) = "BINOP"
                 | typeof(Plus) = "BINOP"
                 | typeof(Minus) = "BINOP"
                 | typeof(Times) = "BINOP"
                 | typeof(Div) = "BINOP"
                 | typeof(Mod) = "BINOP"
                 | typeof(Gt) = "BINOP"
                 | typeof(Lt) = "BINOP"
                 | typeof(Geq) = "BINOP"
                 | typeof(Leq) = "BINOP"
                 | typeof(Eq) = "BINOP"
                 | typeof(Neq) = "BINOP"
                 | typeof(Set) = "SET"
                 | typeof(Seq) = "SEQ"
                 | typeof(Ite) = "ITE"
                 | typeof(Wh) = "WH"
                 | typeof(True) = "TRUE"
                 | typeof(False) = "FALSE"
                 | typeof(Not) = "NOT"
                 | typeof(Negation) = "NEGATION"
                 | typeof(Read(s)) = "READ"
                 | typeof(Write(s)) = "WRITE"
                 | typeof(Exp(s)) = "EXP"
                 | typeof(commandSeq(s)) = if (FunStack.depth(s) = 0) then "NOTHING" else "COMMANDSEQ"
                  | typeof(_) = "DONTCARE";
            val first = 
                if(FunStack.depth(C) >= 2) then FunStack.nth(C, 1) else Nothing;
            val second = 
                if(FunStack.depth(C) >= 3) then FunStack.nth(C, 2) else Nothing;
            val vTop = 
                if(FunStack.depth(V) >= 1) then FunStack.nth(V, 0) else Nothing;
            val vFirst = 
                if(FunStack.depth(V) >= 2) then FunStack.nth(V, 1) else Nothing;

            fun extractVariableName(VARIABLE(x)) = x 
                  | extractVariableName(_) = raise Error ("Not a variable!");
                
            fun extractBool(True) = true
                  | extractBool(False) = false
                  | extractBool(_) = raise Error ("Not a boolean!");

            fun convertToStackDataType(true) = True
                  | convertToStackDataType(false) = False;

            fun extractInt(INTEGER(n)) = n
                 | extractInt(_) = raise Error ("Not an integer!");

            fun compareBooleans(x, y, operator) = 
                let
                    val op1 = if (x = true) then 1 else 0;
                    val op2 = if (y = true) then 1 else 0;
                in
                    if(operator = "GT") then
                        op1 > op2
                    else if (operator = "LT") then
                        op1 < op2
                    else if (operator = "GEQ") then
                        op1 >= op2
                    else if (operator = "LEQ") then
                        op1 <= op2
                    else if (operator = "NEQ") then
                        op1 <> op2
                    else  (*operator = "EQ*)
                        op1 = op2
                end;

            fun stackDataTypesEqual(Lt, Lt) = true
                  | stackDataTypesEqual(Gt, Gt) = true
                  | stackDataTypesEqual(Geq, Geq) = true 
                  | stackDataTypesEqual(Leq, Leq) = true
                  | stackDataTypesEqual(Neq, Neq) = true
                  | stackDataTypesEqual(Eq, Eq) = true
                  | stackDataTypesEqual(And, And) = true
                  | stackDataTypesEqual(Or, Or) = true
                  | stackDataTypesEqual(Plus, Plus) = true
                  | stackDataTypesEqual(Minus, Minus) = true
                  | stackDataTypesEqual(Times, Times) = true
                  | stackDataTypesEqual(Div, Div) = true
                  | stackDataTypesEqual(Mod, Mod) = true
                  | stackDataTypesEqual(INTEGER(_), INTEGER(0)) = true 
                  | stackDataTypesEqual(Nothing, Nothing) = true
                  | stackDataTypesEqual(Read(_), Read(_)) = true
                  | stackDataTypesEqual(Write(_), Write(_)) = true
                  | stackDataTypesEqual(Not, Not) = true
                  | stackDataTypesEqual(Negation, Negation) = true
                  | stackDataTypesEqual(_, _) = false;
                  (*Check this once!*)

            fun extractReadVariable(Read(x)) = x
                  | extractReadVariable(_) = raise Error ("Not Read Statement!!!");

            fun extractWriteExp(Write(exp)) = exp
                   | extractWriteExp(_) = raise Error ("Not a Write Expression!");

            fun updateMemory(m, i, x) = 
                let 
                    val notCare = Array.update(m, i, x)
                in
                    m
                end;

            fun extractStackFromCommandSeq(commandSeq(s)) = s 
                  | extractStackFromCommandSeq(_) = raise Error ("Not a commandSeq");

            fun extractStackFromExp(Exp(s)) = s 
                  | extractStackFromExp(_) = raise Error ("Not an Exp");

            fun boolToInt(true) = 1
                  | boolToInt(false) = 0;

        in 
            if(typeof(tp) = "INTEGER" orelse typeof(tp) = "TRUE" orelse typeof(tp) = "FALSE") then 
                if(typeof(second) = "BINOP") then
                    if(typeof(first) = "INTEGER" orelse typeof(first) = "TRUE" orelse typeof(first) = "FALSE") then
                        ((FunStack.push(first ,FunStack.push(tp, V))), M, FunStack.pop(FunStack.pop(C)))
                    else
                        ((FunStack.push(INTEGER(Array.sub(M, HashTable.lookup symbTable (extractVariableName(first)))) ,FunStack.push(tp, V))), M, FunStack.pop(FunStack.pop(C)))
                else
                    ((FunStack.push(tp, V)), M, FunStack.pop(C))
            else  if (typeof(tp) = "VARIABLE") then
                if(typeof(second) = "BINOP") then
                    if(typeof(first) = "INTEGER" orelse typeof(first) = "TRUE" orelse typeof(first) = "FALSE") then
                        ((FunStack.push(first ,FunStack.push(INTEGER(Array.sub(M, HashTable.lookup symbTable (extractVariableName(tp)))), V))), M, FunStack.pop(FunStack.pop(C)))
                    else
                        ( (FunStack.push(INTEGER(Array.sub(M, HashTable.lookup symbTable (extractVariableName(first)))) , FunStack.push(INTEGER(Array.sub(M, HashTable.lookup symbTable (extractVariableName(tp)))), V)) , M, FunStack.pop(FunStack.pop(C))))
                else if(typeof(second) = "SET") then 
                        (FunStack.push(tp, V), M, FunStack.pop(C))
                else
                    ((FunStack.push(INTEGER(Array.sub(M, HashTable.lookup symbTable (extractVariableName(tp)))), V)), M, FunStack.pop(C))
            else if (typeof(tp) = "BINOP") then 
                if(stackDataTypesEqual(tp, And)) then
                    (FunStack.push(convertToStackDataType((extractBool(vTop)) andalso (extractBool(vFirst))) ,FunStack.pop(FunStack.pop(V))), M, FunStack.pop(C))
                else if(stackDataTypesEqual(tp, Or)) then
                    (FunStack.push(convertToStackDataType((extractBool(vTop)) orelse (extractBool(vFirst))) ,FunStack.pop(FunStack.pop(V))), M, FunStack.pop(C))
                else if(stackDataTypesEqual(tp, Gt)) then
                    if(typeof(vFirst) = "TRUE" orelse typeof(vFirst) = "FALSE") then
                        (FunStack.push(convertToStackDataType(compareBooleans(extractBool(vFirst), extractBool(vTop), "GT")) ,FunStack.pop(FunStack.pop(V))), M, FunStack.pop(C))
                    else
                        (FunStack.push(convertToStackDataType((extractInt(vFirst)) > (extractInt(vTop))) ,FunStack.pop(FunStack.pop(V))), M, FunStack.pop(C))
                else if(stackDataTypesEqual(tp, Lt)) then
                    if(typeof(vFirst) = "TRUE" orelse typeof(vFirst) = "FALSE") then
                        (FunStack.push(convertToStackDataType(compareBooleans(extractBool(vFirst), extractBool(vTop), "LT")) ,FunStack.pop(FunStack.pop(V))), M, FunStack.pop(C))
                    else
                        (FunStack.push(convertToStackDataType((extractInt(vFirst)) < (extractInt(vTop))) ,FunStack.pop(FunStack.pop(V))), M, FunStack.pop(C))
                else if(stackDataTypesEqual(tp, Geq)) then
                    if(typeof(vFirst) = "TRUE" orelse typeof(vFirst) = "FALSE") then
                        (FunStack.push(convertToStackDataType(compareBooleans(extractBool(vFirst), extractBool(vTop), "GEQ")) ,FunStack.pop(FunStack.pop(V))), M, FunStack.pop(C))
                    else
                        (FunStack.push(convertToStackDataType((extractInt(vFirst)) >= (extractInt(vTop))) ,FunStack.pop(FunStack.pop(V))), M, FunStack.pop(C))
                else if(stackDataTypesEqual(tp, Leq)) then
                    if(typeof(vFirst) = "TRUE" orelse typeof(vFirst) = "FALSE") then
                        (FunStack.push(convertToStackDataType(compareBooleans(extractBool(vFirst), extractBool(vTop), "LEQ")) ,FunStack.pop(FunStack.pop(V))), M, FunStack.pop(C))
                    else
                        (FunStack.push(convertToStackDataType((extractInt(vFirst)) <= (extractInt(vTop))) ,FunStack.pop(FunStack.pop(V))), M, FunStack.pop(C))
                else if(stackDataTypesEqual(tp, Eq)) then
                    if(typeof(vFirst) = "TRUE" orelse typeof(vFirst) = "FALSE") then
                        (FunStack.push(convertToStackDataType(compareBooleans(extractBool(vFirst), extractBool(vTop), "EQ")) ,FunStack.pop(FunStack.pop(V))), M, FunStack.pop(C))
                    else
                        (FunStack.push(convertToStackDataType((extractInt(vFirst)) = (extractInt(vTop))) ,FunStack.pop(FunStack.pop(V))), M, FunStack.pop(C))
                else if(stackDataTypesEqual(tp, Neq)) then
                    if(typeof(vFirst) = "TRUE" orelse typeof(vFirst) = "FALSE") then
                        (FunStack.push(convertToStackDataType(compareBooleans(extractBool(vFirst), extractBool(vTop), "NEQ")) ,FunStack.pop(FunStack.pop(V))), M, FunStack.pop(C))
                    else
                        (FunStack.push(convertToStackDataType((extractInt(vFirst)) <> (extractInt(vTop))) ,FunStack.pop(FunStack.pop(V))), M, FunStack.pop(C))
                else if(stackDataTypesEqual(tp, Plus)) then
                    (FunStack.push(INTEGER((extractInt(vFirst)) + (extractInt(vTop))) ,FunStack.pop(FunStack.pop(V))), M, FunStack.pop(C))
                else if(stackDataTypesEqual(tp, Minus)) then
                    (FunStack.push(INTEGER((extractInt(vFirst)) - (extractInt(vTop))) ,FunStack.pop(FunStack.pop(V))), M, FunStack.pop(C))
                else if(stackDataTypesEqual(tp, Times)) then
                    (FunStack.push(INTEGER((extractInt(vFirst)) * (extractInt(vTop))) ,FunStack.pop(FunStack.pop(V))), M, FunStack.pop(C))
                else if(stackDataTypesEqual(tp, Div)) then
                    (FunStack.push(INTEGER((extractInt(vFirst)) div (extractInt(vTop))) ,FunStack.pop(FunStack.pop(V))), M, FunStack.pop(C))
                else if(stackDataTypesEqual(tp, Mod)) then
                    (FunStack.push(INTEGER((extractInt(vFirst)) mod (extractInt(vTop))) ,FunStack.pop(FunStack.pop(V))), M, FunStack.pop(C))
                else (*Should never reach here bruhhh*)
                    (V, M, C)
            (*If top is intexp or boolexp, just recursively use these set of rules and first solve these expressions and push the resulting value in V*)
            else if (typeof(tp) = "SET") then
                   if(typeof(vFirst) = "VARIABLE") then
                        if(typeof(vTop) = "INTEGER") then
                            (FunStack.pop(FunStack.pop(V)), updateMemory(M, HashTable.lookup symbTable (extractVariableName(vFirst)), (extractInt(vTop))), FunStack.pop(C))
                        else
                            (FunStack.pop(FunStack.pop(V)), updateMemory(M, HashTable.lookup symbTable (extractVariableName(vFirst)), boolToInt(extractBool(vTop))), FunStack.pop(C))                            
                    else
                        (FunStack.pop(FunStack.pop(V)), updateMemory(M, HashTable.lookup symbTable (extractVariableName(FunStack.top(extractStackFromExp(vFirst)))), (extractInt(vTop))), FunStack.pop(C))
            else if (typeof(tp) = "COMMANDSEQ") then
                if(typeof(second) = "ITE") then
                    if(extractBool(vTop)= true) then
                        (FunStack.pop(V), M, FunStack.push(tp, FunStack.pop(FunStack.pop(FunStack.pop(C)))))
                    else 
                        (FunStack.pop(V), M, FunStack.push(first, FunStack.pop(FunStack.pop(FunStack.pop(C)))))
                else if (typeof(first) = "WH") then
                    (FunStack.push(tp, V), M, FunStack.push(vTop, FunStack.pop(C)))
                else 
                    let
                        val (V1, M1, _) = helpExecute((FunStack.list2stack([]), M, extractStackFromCommandSeq(tp)));
                    in
                        if(FunStack.depth(V1) = 0) then
                            (V, M1, FunStack.pop(C))
                        else
                            (FunStack.push(FunStack.top(V1), V), M1, FunStack.pop(C))
                    end
            else if (typeof(tp) = "WH") then
                if(extractBool(vTop) = true) then
                    (FunStack.pop(FunStack.pop(FunStack.pop(V))), M, FunStack.push(vFirst, FunStack.push(FunStack.nth(V, 2), FunStack.push(vFirst, C))))
                else
                    (FunStack.pop(FunStack.pop(FunStack.pop(V))), M, FunStack.pop(C))
            else if (typeof(tp) = "EXP") then 
                if(typeof(second) = "WH") then
                    (FunStack.push(first, FunStack.push(tp, V)), M, FunStack.push(tp, FunStack.pop(FunStack.pop(C))))
                else
                    let
                        val (V1, M1, _) = helpExecute((FunStack.list2stack([]), M, extractStackFromExp(tp)));
                    in
                        if(FunStack.depth(V1) = 0) then
                            (V, M1, FunStack.pop(C))
                        else
                            (FunStack.push(FunStack.top(V1), V), M1, FunStack.pop(C))
                    end
            (* else if (typeof(tp) = "EXP") then 
                let
                        val (V1, M1, _) = helpExecute((FunStack.list2stack([]), M, extractStackFromBoolExp(tp)));
                    in
                        if(FunStack.depth(V1) = 0) then
                            (V, M1, FunStack.pop(C))
                        else
                            (FunStack.push(FunStack.top(V1), V), M1, FunStack.pop(C))
                    end *)
            else if (typeof(tp) = "READ") then
                let
                    val adr = extractReadVariable(tp);
                    val printStatement = print("Please enter the value for VARIABLE" ^ adr ^ "that you want to set in the memory :- ");
                    val  input_value = valOf(TextIO.inputLine(TextIO.stdIn));
                    val new_memory = Array.update(M, HashTable.lookup(symbTable) adr, valOf(Int.fromString(input_value)))
                in 
                    (V, M, FunStack.pop(C))
                end
            else if (typeof(tp) = "WRITE") then
                let 
                    val exp = extractWriteExp(tp);
                    val (V1, M1, _) = helpExecute((FunStack.list2stack([]), M, exp));
                    val notNeeded = 
                        if(FunStack.depth(V1) = 0) then
                            ()
                        else
                            print(Int.toString(extractInt(FunStack.top(V1))))
                in
                    (V, M, FunStack.pop(C))
                end
            else if (typeof(tp) = "NOT") then
                if (typeof(first) = "EXP") then
                    let
                        val (V1, M1, _) = helpExecute((FunStack.list2stack([]), M, extractStackFromExp(first)));
                        val ans = not (extractBool(FunStack.top(V1)))
                    in
                        (FunStack.push(convertToStackDataType(ans), V), M1, FunStack.pop(FunStack.pop(C)))
                    end
                else (*typeof(first) should be "NOT" here*)
                    (V, M, FunStack.pop(FunStack.pop(C)))
            else if (typeof(tp) = "NEGATION") then
                if (typeof(first) = "EXP") then
                    let
                        val (V1, M1, _) = helpExecute((FunStack.list2stack([]), M, extractStackFromExp(first)));
                        val ans = (0 - (extractInt(FunStack.top(V1))))
                    in
                        (FunStack.push(INTEGER(ans), V), M1, FunStack.pop(FunStack.pop(C)))
                    end
                else (*typeof(first) should be "NOT" here*)
                    (V, M, FunStack.pop(FunStack.pop(C)))
            else
                (V, M, FunStack.pop(C)) 
        end
        (* | rules(_) = ([], memory, []) *)
    and
    helpExecute((V, M, C)) =
        let
            val notNeed = toString((V, M, C))
        in  
            if(FunStack.depth(C) = 0) then (V, M, C)
            else helpExecute(rules((V, M, C)))
        end
    and
    execute(s) =   
        let
            val memory = Array.array(100, 0);
            val output = helpExecute((FunStack.list2stack([]), memory, s));

            fun clearSymbolTable (table) = HashTable.clear table

            val notmatter = clearSymbolTable(symbTable)
        in
            output
        end
    and
    toString((V, M, C)) =
    let        
        fun toStringHelper(INTEGER(n)) = "INTEGER " ^ Int.toString(n) ^"\n"
            |   toStringHelper(VARIABLE(n)) = "VARIABLE " ^ n ^"\n"
            |   toStringHelper(Exp(s)) = 
                let
                    val x = FunStack.toString(toStringHelper) s
                in
                    if(String.size(x) >= 10 andalso String.substring(x, 0, 10) = "EXPRESSION") then ""
                    else "EXPRESSION " ^ x ^"\n"
                end
            |   toStringHelper(commandSeq(s)) =
                let
                    val x = FunStack.toString(toStringHelper) s
                in
                    if(String.size(x) >= 7 andalso String.substring(x, 0, 7) = "COMMAND") then ""
                    else "COMMAND " ^ x ^"\n"
                end
            |   toStringHelper(Set) = "SET \n" 
            |   toStringHelper(Seq) = "SEQ \n"
            |   toStringHelper(Ite) = "ITE \n"
            |   toStringHelper(Wh) = "WH \n"
            |   toStringHelper(Gt) = "GT \n"
            |   toStringHelper(Lt) = "LT \n"
            |   toStringHelper(Leq) = "LEQ \n"
            |   toStringHelper(Geq) = "GEQ \n"
            |   toStringHelper(Eq) = "EQ \n"
            |   toStringHelper(Neq) = "NEQ \n"
            |   toStringHelper(Plus) = "PLUS \n"
            |   toStringHelper(Minus) = "MINUS \n"
            |   toStringHelper(Times) = "TIMES \n"
            |   toStringHelper(And) = "AND \n"
            |   toStringHelper(Or) = "OR \n"
            |   toStringHelper(Div) = "DIV \n"
            |   toStringHelper(Mod) = "MOD \n"
            |   toStringHelper(True) = "TRUE \n"
            |   toStringHelper(False) = "FALSE \n"
            |   toStringHelper(Read(n)) = "READ " ^ n ^ "\n"
            |   toStringHelper(Write(exp)) = "WRITE " ^ FunStack.toString(toStringHelper) exp ^"\n"
            |   toStringHelper(_) = "STACK HAS ENDED \n"

        fun printMemory(m, i, str) = 
            if(i < Array.length(m)) then printMemory(m, i+1, str ^ " " ^ (Int.toString(Array.sub(m, i)))) else str
    in  
        print("The configuration of V is as follows: \n" ^ FunStack.toString(toStringHelper) V ^"\n\n\n" ^
        "The configuration of C is as follows: \n" ^ FunStack.toString(toStringHelper) C ^ "\n\n\n" ^
        "The configuration of M is as follows: \n" ^ printMemory(M, 0, " ") ^ "\n\n\n")
    end
end;


(* val bestTest = Vmc.execute(postfix(ASTree.node
    (PROG,ASTree.idnode (ID,"test12",ASTree.empty),
     ASTree.node
       (BLK,
        ASTree.node
          (DES,
           ASTree.node
             (DE,ASTree.node (VL,ASTree.idnode (ID,"c",ASTree.empty),ASTree.empty),ASTree.node (INT,ASTree.empty,ASTree.empty)),
           ASTree.node (DES,ASTree.empty,ASTree.empty)),
        ASTree.node
          (SEQ,
           ASTree.node
             (SET,ASTree.idnode (ID,"c",ASTree.empty),
              ASTree.node (NUM,ASTree.intnode (NUM,0,ASTree.empty),ASTree.empty)),
           ASTree.node
             (SEQ,
              ASTree.node
                (WH,
                 ASTree.node
                   (LT,ASTree.node (Var,ASTree.idnode (ID,"c",ASTree.empty),ASTree.empty),
                    ASTree.node (NUM,ASTree.intnode (NUM,10,ASTree.empty),ASTree.empty)),
                 ASTree.node
                   (SEQ,
                    ASTree.node3
                      (ITE,
                       ASTree.node
                         (EQ,
                          ASTree.node
                            (MOD,ASTree.node (Var,ASTree.idnode (ID,"c",ASTree.empty),ASTree.empty),
                             ASTree.node (NUM,ASTree.intnode (NUM,2,ASTree.empty),ASTree.empty)),
                          ASTree.node (NUM,ASTree.intnode (NUM,0,ASTree.empty),ASTree.empty)),
                       ASTree.node
                         (SEQ,ASTree.node (READ,ASTree.idnode (ID,"c",ASTree.empty),ASTree.empty),
                          ASTree.node (SEQ,ASTree.empty,ASTree.empty)),
                       ASTree.node
                         (SEQ,
                          ASTree.node
                            (WRITE,
                             ASTree.node
                               (MOD,ASTree.node (Var,ASTree.idnode (ID,"c",ASTree.empty),ASTree.empty),
                                ASTree.node (NUM,ASTree.intnode (NUM,2,ASTree.empty),ASTree.empty)),ASTree.empty),
                          ASTree.node (SEQ,ASTree.empty,ASTree.empty))),
                    ASTree.node
                      (SEQ,
                       ASTree.node
                         (SET,ASTree.idnode (ID,"c",ASTree.empty),
                          ASTree.node
                            (PLUS,ASTree.node (Var,ASTree.idnode (ID,"c",ASTree.empty),ASTree.empty),
                             ASTree.node (NUM,ASTree.intnode (NUM,1,ASTree.empty),ASTree.empty))),
                       ASTree.node (SEQ,ASTree.empty,ASTree.empty)))),ASTree.node (SEQ,ASTree.empty,ASTree.empty)))))
, FunStack.list2stack([]))); *)

(* val t = Vmc.execute(postfix(ASTree.intnode(NUM, 2, ASTree.empty), FunStack.list2stack([])));

val s = Vmc.execute(postfix(ASTree.node(PLUS, ASTree.intnode(NUM, 2, ASTree.empty), ASTree.intnode(NUM, 3, ASTree.empty)), FunStack.list2stack([]))); *)

(* val s = postfix(ASTree.node(PROG,ASTree.idnode (ID,"test3",ASTree.empty),
     ASTree.node
       (BLK,
        ASTree.node
          (DES,
           ASTree.node
             (DE,
              ASTree.node
                (VL,ASTree.idnode (ID,"x",ASTree.empty),
                 ASTree.node (VL,ASTree.idnode (ID,"y",ASTree.empty),ASTree.empty)),
              ASTree.node (INT,ASTree.empty,ASTree.empty)),ASTree.node (DES,ASTree.empty,ASTree.empty)),
        ASTree.node
          (SEQ,
           ASTree.node
             (SET,ASTree.idnode (ID,"x",ASTree.empty),
              ASTree.node
                (PLUS,ASTree.node (NUM,ASTree.intnode (NUM,5,ASTree.empty),ASTree.empty),
                 ASTree.node
                   (TIMES,ASTree.node (NUM,ASTree.intnode (NUM,5,ASTree.empty),ASTree.empty),
                    ASTree.node (NUM,ASTree.intnode (NUM,5,ASTree.empty),ASTree.empty)))),
           ASTree.node
             (SEQ,
              ASTree.node
                (SET,ASTree.idnode (ID,"y",ASTree.empty),
                 ASTree.node
                   (PLUS,
                    ASTree.node
                      (TIMES,ASTree.node (NUM,ASTree.intnode (NUM,5,ASTree.empty),ASTree.empty),
                       ASTree.node (NUM,ASTree.intnode (NUM,5,ASTree.empty),ASTree.empty)),
                    ASTree.node (NUM,ASTree.intnode (NUM,5,ASTree.empty),ASTree.empty))),
              ASTree.node (SEQ,ASTree.empty,ASTree.empty))))), FunStack.list2stack([])); *)

(* val s = Vmc.execute(postfix(ASTree.node
    (PROG,ASTree.idnode (ID,"test1",ASTree.empty),
     ASTree.node
       (BLK,
        ASTree.node
          (DES,
           ASTree.node
             (DE,ASTree.node (VL,ASTree.idnode (ID,"x",ASTree.empty),ASTree.empty),ASTree.node (INT,ASTree.empty,ASTree.empty)),
           ASTree.node (DES,ASTree.empty,ASTree.empty)),
        ASTree.node
          (SEQ,
           ASTree.node
             (SET,ASTree.idnode (ID,"x",ASTree.empty),
              ASTree.node
                (PLUS,ASTree.node (NUM,ASTree.intnode (NUM,1,ASTree.empty),ASTree.empty),
                 ASTree.node (NUM,ASTree.intnode (NUM,2,ASTree.empty),ASTree.empty))),
           ASTree.node (SEQ,ASTree.empty,ASTree.empty)))), FunStack.list2stack([]))); *)

(* val no = Vmc.execute(postfix(ASTree.node
          (SEQ,
              ASTree.node
                (LT,
                 ASTree.node
                   (PLUS,ASTree.node (NUM,ASTree.intnode (NUM,1,ASTree.empty),ASTree.empty),
                    ASTree.node (NUM,ASTree.intnode (NUM,2,ASTree.empty),ASTree.empty)),
                 ASTree.node
                   (PLUS,ASTree.node (NUM,ASTree.intnode (NUM,2,ASTree.empty),ASTree.empty),
                    ASTree.node (NUM,ASTree.intnode (NUM,3,ASTree.empty), ASTree.empty))),
           ASTree.node (SEQ,ASTree.empty,ASTree.empty)), FunStack.list2stack([]))); *)
(* 
val shouldBeCorrect = Vmc.execute(postfix(ASTree.node
                (LT,
                 ASTree.node
                   (PLUS,ASTree.node (NUM,ASTree.intnode (NUM,1,ASTree.empty),ASTree.empty),
                    ASTree.node (NUM,ASTree.intnode (NUM,2,ASTree.empty),ASTree.empty)),
                 ASTree.node
                   (PLUS,ASTree.node (NUM,ASTree.intnode (NUM,2,ASTree.empty),ASTree.empty),
                    ASTree.node (NUM,ASTree.intnode (NUM,3,ASTree.empty),ASTree.empty))), FunStack.list2stack([]))); *)

(* val test10 = Vmc.execute(postfix(ASTree.node
    (PROG,ASTree.idnode (ID,"test10",ASTree.empty),
     ASTree.node
       (BLK,
        ASTree.node
          (DES,
           ASTree.node
             (DE,ASTree.node (VL,ASTree.idnode (ID,"c",ASTree.empty),ASTree.empty),
              ASTree.node (BOOL,ASTree.empty,ASTree.empty)),ASTree.node (DES,ASTree.empty,ASTree.empty)),
        ASTree.node
          (SEQ,
           ASTree.node
             (SET,ASTree.idnode (ID,"c",ASTree.empty),
              ASTree.node
                (LEQ,ASTree.node (LT,ASTree.node (TT,ASTree.empty,ASTree.empty),ASTree.node (FF,ASTree.empty,ASTree.empty)),
                 ASTree.node (LT,ASTree.node (FF,ASTree.empty,ASTree.empty),ASTree.node (FF,ASTree.empty,ASTree.empty)))),
           ASTree.node (SEQ,ASTree.empty,ASTree.empty)))), FunStack.list2stack([]))); *)

(* val test11 = Vmc.execute(postfix(ASTree.node
    (PROG,ASTree.idnode (ID,"test11",ASTree.empty),
     ASTree.node
       (BLK,
        ASTree.node
          (DES,
           ASTree.node
             (DE,ASTree.node (VL,ASTree.idnode (ID,"c",ASTree.empty),ASTree.empty),
              ASTree.node (BOOL,ASTree.empty,ASTree.empty)),ASTree.node (DES,ASTree.empty,ASTree.empty)),
        ASTree.node
          (SEQ,
           ASTree.node
             (SET,ASTree.idnode (ID,"c",ASTree.empty),
              ASTree.node
                (LEQ,ASTree.node (LT,ASTree.node (TT,ASTree.empty,ASTree.empty),ASTree.node (FF,ASTree.empty,ASTree.empty)),
                 ASTree.node (FF,ASTree.empty,ASTree.empty))),ASTree.node (SEQ,ASTree.empty,ASTree.empty))))
, FunStack.list2stack([]))); *)

 (* val check = Vmc.execute(postfix(ASTree.node
    (PROG,ASTree.idnode (ID,"test9",ASTree.empty),
     ASTree.node
       (BLK,
        ASTree.node
          (DES,
           ASTree.node
             (DE,ASTree.node (VL,ASTree.idnode (ID,"c",ASTree.empty),ASTree.empty),
              ASTree.node (BOOL,ASTree.empty,ASTree.empty)),ASTree.node (DES,ASTree.empty,ASTree.empty)),
        ASTree.node
          (SEQ,
           ASTree.node
             (SET, ASTree.idnode (ID,"c",ASTree.empty),
              ASTree.node
                (LT,
                 ASTree.node
                   (PLUS,ASTree.node (NUM,ASTree.intnode (NUM,1,ASTree.empty),ASTree.empty),
                    ASTree.node (NUM,ASTree.intnode (NUM,2,ASTree.empty),ASTree.empty)),
                 ASTree.node
                   (PLUS,ASTree.node (NUM,ASTree.intnode (NUM,2,ASTree.empty),ASTree.empty),
                    ASTree.node (NUM,ASTree.intnode (NUM,3,ASTree.empty),ASTree.empty)))),
           ASTree.node (SEQ,ASTree.empty,ASTree.empty)))), FunStack.list2stack([])));  *)
(*
val big = Vmc.execute(postfix(ASTree.node
    (PROG,ASTree.idnode (ID,"test3",ASTree.empty),
     ASTree.node
       (BLK,
        ASTree.node
          (DES,
           ASTree.node
             (DE,
              ASTree.node
                (VL,ASTree.idnode (ID,"x",ASTree.empty),
                 ASTree.node (VL,ASTree.idnode (ID,"y",ASTree.empty),ASTree.empty)),
              ASTree.node (INT,ASTree.empty,ASTree.empty)),ASTree.node (DES,ASTree.empty,ASTree.empty)),
        ASTree.node
          (SEQ,
           ASTree.node
             (SET,ASTree.idnode (ID,"x",ASTree.empty),
              ASTree.node
                (PLUS,ASTree.node (NUM,ASTree.intnode (NUM,5,ASTree.empty),ASTree.empty),
                 ASTree.node
                   (TIMES,ASTree.node (NUM,ASTree.intnode (NUM,5,ASTree.empty),ASTree.empty),
                    ASTree.node (NUM,ASTree.intnode (NUM,5,ASTree.empty),ASTree.empty)))),
           ASTree.node
             (SEQ,
              ASTree.node
                (SET,ASTree.idnode (ID,"y",ASTree.empty),
                 ASTree.node
                   (PLUS,
                    ASTree.node
                      (TIMES,ASTree.node (NUM,ASTree.intnode (NUM,5,ASTree.empty),ASTree.empty),
                       ASTree.node (NUM,ASTree.intnode (NUM,5,ASTree.empty),ASTree.empty)),
                    ASTree.node (NUM,ASTree.intnode (NUM,5,ASTree.empty),ASTree.empty))),
              ASTree.node (SEQ,ASTree.empty,ASTree.empty))))), FunStack.list2stack([])));

val bestTest = (postfix(ASTree.node
    (PROG,ASTree.idnode (ID,"good2",ASTree.empty),
     ASTree.node
       (BLK,
        ASTree.node
          (DES,
           ASTree.node
             (DE,
              ASTree.node
                (VL,ASTree.idnode (ID,"A",ASTree.empty),
                 ASTree.node
                   (VL,ASTree.idnode (ID,"B",ASTree.empty),
                    ASTree.node
                      (VL,ASTree.idnode (ID,"C",ASTree.empty),
                       ASTree.node (VL,ASTree.idnode (ID,"D",ASTree.empty),ASTree.empty)))),
              ASTree.node (INT,ASTree.empty,ASTree.empty)),
           ASTree.node
             (DES,
              ASTree.node
                (DE,
                 ASTree.node
                   (VL,ASTree.idnode (ID,"We",ASTree.empty),
                    ASTree.node
                      (VL,ASTree.idnode (ID,"E",ASTree.empty),
                       ASTree.node
                         (VL,ASTree.idnode (ID,"F",ASTree.empty),
                          ASTree.node (VL,ASTree.idnode (ID,"G",ASTree.empty),ASTree.empty)))),
                 ASTree.node (BOOL,ASTree.empty,ASTree.empty)),ASTree.node (DES,ASTree.empty,ASTree.empty))),
        ASTree.node
          (SEQ,
           ASTree.node
             (SET,ASTree.idnode (ID,"A",ASTree.empty),
              ASTree.node
                (PLUS,ASTree.node (NUM,ASTree.intnode (NUM,23,ASTree.empty),ASTree.empty),
                 ASTree.node (NUM,ASTree.intnode (NUM,44,ASTree.empty),ASTree.empty))),
           ASTree.node
             (SEQ,
              ASTree.node
                (SET,ASTree.idnode (ID,"B",ASTree.empty),
                 ASTree.node
                   (PLUS,ASTree.node (Var,ASTree.idnode (ID,"A",ASTree.empty),ASTree.empty),
                    ASTree.node (NUM,ASTree.intnode (NUM,1,ASTree.empty),ASTree.empty))),
              ASTree.node
                (SEQ,
                 ASTree.node
                   (SET,ASTree.idnode (ID,"D",ASTree.empty),
                    ASTree.node
                      (PLUS,ASTree.node (Var,ASTree.idnode (ID,"B",ASTree.empty),ASTree.empty),
                       ASTree.node (NUM,ASTree.intnode (NUM,2,ASTree.empty),ASTree.empty))),
                 ASTree.node (SEQ,ASTree.empty,ASTree.empty)))))), FunStack.list2stack([]))); *)

(* val (a,b,c) =Vmc.execute(postfix(ASTree.node(PLUS,ASTree.node (NUM,ASTree.intnode (NUM,1,ASTree.empty),ASTree.empty),
                 ASTree.node (NUM,ASTree.intnode (NUM,2,ASTree.empty),ASTree.empty)), FunStack.list2stack([])));

val s3 = postfix(ASTree.node(NUM, ASTree.intnode(NUM, 2, ASTree.empty), ASTree.empty), FunStack.list2stack([]));
val s4 = postfix(ASTree.intnode(NUM, 2, ASTree.empty), FunStack.list2stack([])); *)

(* fun help(commandSeq(s)) = s; *)