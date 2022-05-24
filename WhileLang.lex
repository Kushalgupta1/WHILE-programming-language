structure T = Tokens

type pos = int
type svalue = T.svalue
type('a, 'b) token = ('a, 'b) T.token
type lexresult = (svalue, pos) token

val lin = ref 1;
val col = ref 2;

val eof = fn () => T.EOF (!lin,!col);
val badCh : string * int * int -> unit = fn(bad,line,col) =>TextIO.output(TextIO.stdOut,"["^Int.toString line^"."^Int.toString col^"] Invalid character \""^bad^"\"\n");
	
%%
%header(functor WhileLangLexFun(structure Tokens: WhileLang_TOKENS));

alpha=[A-Za-z];
digit=[0-9];
ws = [\ \t];
eol = "\013\010"|"\010"|"\013";
alphanumeric = [A-Za-z0-9];
%%
{ws}+    => (col := (!col) + size yytext; continue ());
{eol}    => (col := 1; lin:= (!lin) + 1; continue());
{digit}+ => (col := (!col)+size yytext; T.INTCONST(valOf(Int.fromString(yytext)), !lin, !col));
"::"     => (col := (!col) + 2; T.CONS(!lin, !col) );
":"      => (col := (!col) + 1; T.COLON(!lin, !col) );
";"      => (col := (!col) + 1; T.SEMICOLON(!lin, !col) );
","      => (col := (!col) + 1; T.COMMA(!lin, !col) );
"{"      => (col := (!col) + 1; T.LBRACE(!lin, !col) );
"}"      => (col := (!col) + 1; T.RBRACE(!lin, !col) );
":="     => (col := (!col) + 2; T.SET(!lin, !col) );
"("      => (col := (!col) + 1; T.LPAREN(!lin, !col) );
")"      => (col := (!col) + 1; T.RPAREN(!lin, !col) );
"~"      => (col := (!col) + 1; T.NEGSIGN(!lin, !col) );
"||"     => (col := (!col) + 2; T.OR(!lin, !col) );
"&&"     => (col := (!col) + 2; T.AND(!lin, !col) );
"!"      => (col := (!col) + 1; T.NOT(!lin, !col) );
"<"      => (col := (!col) + 1; T.LT(!lin, !col) );
"<="     => (col := (!col) + 2; T.LEQ(!lin, !col) );
"="      => (col := (!col) + 1; T.EQ(!lin, !col) );
">"      => (col := (!col) + 1; T.GT(!lin, !col) );
">="     => (col := (!col) + 2; T.GEQ(!lin, !col) );
"<>"     => (col := (!col) + 2; T.NEQ(!lin, !col) );
"+"      => (col := (!col) + 1; T.PLUS(!lin, !col) );
"-"      => (col := (!col) + 1; T.MINUS(!lin, !col) );
"*"      => (col := (!col) + 1; T.TIMES(!lin, !col) );
"/"      => (col := (!col) + 1; T.DIV(!lin, !col) );
"%"      => (col := (!col) + 1; T.MOD(!lin, !col) );
{alpha}+ => (col := (!col) + size yytext; 
            	if yytext = "program" then T.PROG(!lin, !col) else
            	if yytext = "var" then T.VAR(!lin, !col) else
            	if yytext = "int" then T.INT(!lin, !col) else
            	if yytext = "bool" then T.BOOL(!lin, !col) else
            	if yytext = "read" then T.READ(!lin, !col) else
            	if yytext = "write" then T.WRITE(!lin, !col) else
            	if yytext = "if" then T.IF(!lin, !col) else
            	if yytext = "then" then T.THEN(!lin, !col) else
            	if yytext = "else" then T.ELSE(!lin, !col) else
            	if yytext = "endif" then T.ENDIF(!lin, !col) else
            	if yytext = "while" then T.WHILE(!lin, !col) else
            	if yytext = "do" then T.DO(!lin, !col) else
            	if yytext = "endwh" then T.ENDWH(!lin, !col) else
            	if yytext = "tt" then T.TT(!lin, !col) else
            	if yytext = "ff" then T.FF(!lin, !col)  else
            	T.IDENTIFIER(yytext, !lin, !col));
{alpha}{alphanumeric}* => (col := (!col) + size yytext; T.IDENTIFIER(yytext, !lin, !col));
.        => (col := (!col) + size yytext; badCh(yytext, !lin, !col); T.ILLCH(!lin, !col) );