structure While_AST :
sig val  main : string -> ((stackDataType FunStack.Stack) * (int Array.array) * (stackDataType FunStack.Stack)) 
end =
struct
    exception WhileError;
    fun main (fileName) =
        let 

            fun invoke lexstream =
    	     	let fun error (s,row:int,col:int) =
		    	(TextIO.output(TextIO.stdOut, "Syntax Error Found! \n"^Int.toString(row)^" : "^Int.toString(col)^": \n"^s) ; raise syntaxErr)
                in
                    WhileParser.parse(0,lexstream,error,())
                end

            fun stringToLexer str =
                let val done = ref false
                    val lexer=  WhileParser.makeLexer (fn _ => if (!done) then "" else (done:=true;str))
                in
                    lexer
                end	
                        
            fun parse (lexer) =
                let val dummyEOF = WhileLrVals.Tokens.EOF(0,0)
                    val (result, lexer) = invoke lexer
                    val (nextToken, lexer) = WhileParser.Stream.get lexer
                    (* val type_mismatch = if(result = WhileParser.empty) then true else false
                    val print_mismatch = if(type_mismatch) then print("Ah! There was a type mismatch found! Please recheck your program!\n") else print("\n") *)
                in
                    (result)
                end

            fun read (infile:string) =
                let 
                    val instream = TextIO.openIn infile
                    fun loop instream =
                    String.implode(String.explode(TextIO.inputAll instream))

                in
                    loop instream before TextIO.closeIn instream
                end

            val parseString = parse o stringToLexer 
            val parseFile = parse o stringToLexer o read 
        (* in 
            parseFile(fileName)
        end *)

            fun evaluate (infile: string) =
                let
                    val postFixedStack = postfix(parseFile(infile), FunStack.list2stack([]))
                in 	
                    Vmc.execute (postFixedStack)
                end
        in
            evaluate(fileName)
        end
end