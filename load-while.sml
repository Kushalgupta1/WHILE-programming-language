structure WhileLangLrVals = WhileLangLrValsFun(structure Token = LrParser.Token)
structure WhileLangLex = WhileLangLexFun(structure Tokens = WhileLangLrVals.Tokens);
structure WhileLangParser =
	  Join(structure LrParser = LrParser
     	       structure ParserData = WhileLangLrVals.ParserData
     	       structure Lex = WhileLangLex)
     
fun invoke lexstream =
    	     	let fun print_error (s,pos:int,_) =
		    	TextIO.output(TextIO.stdOut, "Error, line " ^ (Int.toString pos) ^ "," ^ s ^ "\n")
		in
		    WhileLangParser.parse(0,lexstream,print_error,())
		end

fun stringToLexer str =
    let val done = ref false
    	val lexer=  WhileLangParser.makeLexer (fn _ => if (!done) then "" else (done:=true;str))
    in
	lexer
    end	
		
fun parse (lexer) =
    let val dummyEOF = WhileLangLrVals.Tokens.EOF(0,0)
    	val (result, lexer) = invoke lexer
	val (nextToken, lexer) = WhileLangParser.Stream.get lexer
    in
        if WhileLangParser.sameToken(nextToken, dummyEOF) then result
 	else (TextIO.output(TextIO.stdOut, "Warning: Unconsumed input \n"); result)
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