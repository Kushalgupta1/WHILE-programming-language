open While_AST

val LUT = Array.array(10000, ("" , ""));
val varCount = ref 0;

fun lookIfPresent(LUT, s)=
        let fun check(i)= 
                let val (a, b)=Array.sub(LUT, i)
                in
                        if i < (!varCount) then
                        if a =s then true else check(i+1)
                        else false
                end;
        in check(0)
        end;

fun returnVarType(LUT, s)=
        let fun find(i)= 
            let val (a, b)=Array.sub(LUT, i)
                in
                        if i < (!varCount) then
                        if a =s then b else find(i+1)
                        else (parseStatus := false; print("Variable \""^s^"\"  not declared, assuming that it is of INT type (will still throw not declared error, wherever present)\n"); "bogus")
                end;
        in find(0)
        end;


fun DecAgainErr(bad) =
(parseStatus := false; TextIO.output(TextIO.stdOut,"Variable already declared in the declarations "^bad^"\n"))

fun NotDeclaredErr(bad,line,col) =
(parseStatus := false; TextIO.output(TextIO.stdOut,"["^Int.toString line^"."^Int.toString col^"] Variable not declared \""^bad^"\"\n"))

fun typeError(line,col) =
(parseStatus := false; TextIO.output(TextIO.stdOut,"["^Int.toString line^"."^Int.toString col^"] Type mismatch here\n"))

fun applyType(f, []) = []
 |  applyType(f, VARIABLE(h) :: t) = 
 if lookIfPresent(LUT, h) then (DecAgainErr(h); applyType(f, t))
 else
let 
   val newhead = f(VARIABLE(h));
   fun storeInArr(INT(_)) = Array.update(LUT, !varCount, (h, "INT") ) 
        | storeInArr(_) = Array.update(LUT, !varCount, (h, "BOOL") );
   val c = storeInArr(newhead);
in 
   varCount := (!varCount) + 1;
   newhead :: applyType(f, t)
end;

fun append(h :: t, list2) = h :: append(t, list2)
 |  append([], list2) = list2;

%%

(*ML-Yacc declarations*)

%name WhileLang
%term PROG | CONS | VAR | COLON | SEMICOLON | INT | BOOL | COMMA | LBRACE | RBRACE | SET | READ | WRITE | IF | THEN | ELSE | ENDIF | WHILE | DO | ENDWH | LPAREN | RPAREN | NEGSIGN | OR | AND | NOT | LT | LEQ | EQ | GT | GEQ | NEQ | PLUS | MINUS | TIMES | DIV | MOD | INTCONST of int | TT | FF | IDENTIFIER of string | EOF | ILLCH

%nonterm Program of AST 
       | Block of BLK
       | DeclarationSeq of AST list
       | CommandBlock of AST
       | CommandSeq of AST list
       | Command of AST
       | Declaration of AST list
       | VariableList of AST list
       | Type of TYPE
       | Expression of AST


%left LT LEQ EQ GT GEQ NEQ
%left OR AND
%left PLUS MINUS
%left TIMES DIV MOD
%left NOT NEGSIGN
%eop EOF
%noshift EOF
%pos int
%verbose
%%

Program : PROG IDENTIFIER CONS Block (PROG(VARIABLE(IDENTIFIER), Block))

Block   : DeclarationSeq CommandBlock (BLK(DeclarationSeq, CommandBlock))

DeclarationSeq : Declaration SEMICOLON DeclarationSeq ( append(Declaration, DeclarationSeq))
               | ([])

Declaration : VAR VariableList COLON Type (applyType (Type, VariableList))

VariableList : IDENTIFIER COMMA VariableList (VARIABLE(IDENTIFIER) :: VariableList) 

             | IDENTIFIER ([VARIABLE(IDENTIFIER)]) 

Type :  INT  (INT)
     |  BOOL (BOOL)

CommandBlock : LBRACE CommandSeq RBRACE (SEQ(CommandSeq))

CommandSeq : Command SEMICOLON CommandSeq (Command :: CommandSeq)
           | ([])

Command : IDENTIFIER SET Expression (if lookIfPresent(LUT, IDENTIFIER) = false then NotDeclaredErr(IDENTIFIER, IDENTIFIER1left, IDENTIFIER1right) else (); 
                                     if (confirmExpType(Expression1, "BOOL") andalso returnVarType(LUT, IDENTIFIER)="BOOL") orelse (confirmExpType(Expression1, "INT") andalso returnVarType(LUT, IDENTIFIER)="INT") then () else typeError(SET1left, SET1right);
                                     SET( VARIABLE(IDENTIFIER), Expression) )

        | IF Expression THEN CommandBlock ELSE CommandBlock ENDIF (if confirmExpType(Expression, "BOOL") then () else typeError(IF1left, IF1right); ITE(Expression, CommandBlock1, CommandBlock2))

        | WHILE Expression DO CommandBlock ENDWH (if confirmExpType(Expression, "BOOL") then () else typeError(WHILE1left, WHILE1right); WH(Expression, CommandBlock))

        | READ IDENTIFIER (if lookIfPresent(LUT, IDENTIFIER) = false then NotDeclaredErr(IDENTIFIER, IDENTIFIER1left, IDENTIFIER1right) else (); 
                           READ(VARIABLE(IDENTIFIER)))

        | WRITE Expression (WRITE(Expression))

Expression : Expression PLUS Expression (if confirmExpType(Expression1, "INT") andalso confirmExpType(Expression2, "INT") then () else typeError(PLUS1left, PLUS1right); IEXP(PLUS(Expression1, Expression2)))
           | Expression MINUS Expression (if confirmExpType(Expression1, "INT") andalso confirmExpType(Expression2, "INT") then () else typeError(MINUS1left, MINUS1right); IEXP(MINUS(Expression1, Expression2)))
           | Expression TIMES Expression (if confirmExpType(Expression1, "INT") andalso confirmExpType(Expression2, "INT") then () else typeError(TIMES1left, TIMES1right); IEXP(TIMES(Expression1, Expression2)))
           | Expression DIV Expression (if confirmExpType(Expression1, "INT") andalso confirmExpType(Expression2, "INT") then () else typeError(DIV1left, DIV1right); IEXP(DIV(Expression1, Expression2)))
           | Expression MOD Expression (if confirmExpType(Expression1, "INT") andalso confirmExpType(Expression2, "INT") then () else typeError(MOD1left, MOD1right); IEXP(MOD(Expression1, Expression2)))

           | Expression AND Expression (if confirmExpType(Expression1, "BOOL") andalso confirmExpType(Expression2, "BOOL") then () else typeError(AND1left, AND1right); BEXP(AND(Expression1, Expression2)))
           | Expression OR Expression (if confirmExpType(Expression1, "BOOL") andalso confirmExpType(Expression2, "BOOL") then () else typeError(OR1left, OR1right); BEXP(OR(Expression1, Expression2)))
           | NOT Expression (if confirmExpType(Expression, "BOOL") then () else typeError(NOT1left, NOT1right); BEXP(NOT(Expression)))
        
           | Expression GT Expression (if (confirmExpType(Expression1, "BOOL") andalso confirmExpType(Expression2, "BOOL")) orelse (confirmExpType(Expression1, "INT") andalso confirmExpType(Expression2, "INT")) then () else typeError(GT1left, GT1right); BEXP(GT(Expression1, Expression2)))
           | Expression LT Expression (if (confirmExpType(Expression1, "BOOL") andalso confirmExpType(Expression2, "BOOL")) orelse (confirmExpType(Expression1, "INT") andalso confirmExpType(Expression2, "INT")) then () else typeError(LT1left, LT1right); BEXP(LT(Expression1, Expression2)))
           | Expression LEQ Expression (if (confirmExpType(Expression1, "BOOL") andalso confirmExpType(Expression2, "BOOL")) orelse (confirmExpType(Expression1, "INT") andalso confirmExpType(Expression2, "INT")) then () else typeError(LEQ1left, LEQ1right); BEXP(LEQ(Expression1, Expression2)))
           | Expression GEQ Expression (if (confirmExpType(Expression1, "BOOL") andalso confirmExpType(Expression2, "BOOL")) orelse (confirmExpType(Expression1, "INT") andalso confirmExpType(Expression2, "INT")) then () else typeError(GEQ1left, GEQ1right); BEXP(GEQ(Expression1, Expression2)))
           | Expression EQ Expression (if (confirmExpType(Expression1, "BOOL") andalso confirmExpType(Expression2, "BOOL")) orelse (confirmExpType(Expression1, "INT") andalso confirmExpType(Expression2, "INT")) then () else typeError(EQ1left, EQ1right); BEXP(EQ(Expression1, Expression2)))
           | Expression NEQ Expression (if (confirmExpType(Expression1, "BOOL") andalso confirmExpType(Expression2, "BOOL")) orelse (confirmExpType(Expression1, "INT") andalso confirmExpType(Expression2, "INT")) then () else typeError(NEQ1left, NEQ1right); BEXP(NEQ(Expression1, Expression2)))

           | NEGSIGN LPAREN Expression RPAREN (if confirmExpType(Expression, "INT") then () else typeError(NEGSIGN1left, NEGSIGN1right); BEXP(NEGSIGN(Expression)))
           | LPAREN Expression RPAREN (Expression)
           | INTCONST (IEXP(CONSTANT(INTCONST)))
           | TT (BEXP(TT))
           | FF (BEXP(FF))
           | IDENTIFIER (if lookIfPresent(LUT, IDENTIFIER) = false then NotDeclaredErr(IDENTIFIER, IDENTIFIER1left, IDENTIFIER1right) else (); 
           if returnVarType(LUT, IDENTIFIER)="BOOL" then BEXP(VARIABLE(IDENTIFIER)) else IEXP(VARIABLE(IDENTIFIER)))