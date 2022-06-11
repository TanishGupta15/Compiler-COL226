exception syntaxErr
structure WhileLrVals = while_astLrValsFun(structure Token = LrParser.Token)
structure WhileLex = WhileLexFun(structure Tokens = WhileLrVals.Tokens);
structure WhileParser =
	  Join(structure LrParser = LrParser
     	       structure ParserData = WhileLrVals.ParserData
     	       structure Lex = WhileLex)