open While_AST

signature STACK =
    sig
        type 'a Stack
        exception NoElementStack
        exception Error of string
        val create : unit -> 'a Stack
        val push : 'a * 'a Stack -> 'a Stack
        val pop : 'a Stack -> 'a Stack
        val top : 'a Stack -> 'a
        val empty : 'a Stack -> bool
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
        val list2stack : 'a list -> 'a Stack  (* Convert a list into a Stack *)
        val stack2list : 'a Stack -> 'a list (* Convert a Stack into a list *)
        val toString: ('a -> string) -> 'a Stack -> string 
end;

structure FunStack :> STACK =
struct
    type 'a Stack= 'a list;
    exception NoElementStack;
    exception Error of string;

    fun create()= [];

    fun push(x, l)= x::l;

    fun pop([])= raise NoElementStack
     |  pop(h :: t)= t;

    fun top([])= raise NoElementStack
     |  top(h :: t)= h;

    fun empty([])= true
     |  empty(_)= false;

    fun poptop([])= raise NoElementStack
     |  poptop(h :: t)= SOME(h, t);

    fun nth([], _)= raise Error("Index out of bounds exception")
     |  nth(h :: t, 0)= h
     |  nth(h :: t, i)= nth(t, i-1);

    fun drop(l, 0)= l
    |   drop([], i)= raise Error("Stack size smaller than the number of elements to be dropped")
    |   drop(h :: t, i)= drop(t, i-1);

    fun depth(l)= List.length(l) 

    fun app f l= List.app f l

    fun map f l = (List.map f l)
    
    fun mapPartial f l = (List.mapPartial f l)

    fun find f l = List.find f l
          
    fun filter f l = (List.filter f l)

    fun foldr f init l = List.foldr f init l
    
    fun foldl f init l = List.foldl f init l
            
    fun exists f l = List.exists f l

    fun all f l= List.all f l
 
    fun list2stack([])= create()
     |  list2stack(h :: t) = push(h, list2stack(t));

    fun stack2list(l)= l;

    fun toString f l = 
    let fun toStringFromList f [] = ""
        |  toStringFromList f (h :: t) = f(h) ^ ", " ^ (toStringFromList f t)
    in "[" ^ toStringFromList f l ^ "]\n"
    end;

end;    

structure elementDefinition =
struct
    datatype V_C_element = CommandElement of V_C_element * V_C_element * V_C_element * V_C_element
                    |      ExpressionElement of V_C_element * V_C_element * V_C_element  
                    |      SeqElement of V_C_element * V_C_element
                    |      VariableElement of string 
                    |      ConstantElement of int 
                    |      BinaryOperator of string 
                    |      UnaryOperator of string 
                    |      InstructionElement of string 
                    |      NoElement;

    fun ElementToString(CommandElement(a, b, c, d))= "Command [" ^ ElementToString(a) ^ ", "^ ElementToString(b)^ ", "^ ElementToString(c) ^", "^ ElementToString(d) ^"]"
    |   ElementToString(ExpressionElement(a, b, c))= "Expression["^ElementToString(a)^ ", "^ElementToString(b)^", "^ElementToString(c)^"]"
    |   ElementToString(SeqElement(c, d)) = "Sequencing [" ^ ElementToString(c) ^ ", "^ ElementToString(d) ^"]" 
    |   ElementToString(InstructionElement(s))= "Instruction ["^s^"]"
    |   ElementToString(BinaryOperator(k))= "Binary Operator ["^ k ^"]"
    |   ElementToString(UnaryOperator(k))= "Unary Operator ["^ k ^"]"
    |   ElementToString(ConstantElement(m))= "Constant Value ["^ Int.toString(m) ^"]"
    |   ElementToString(VariableElement(var))= "Variable ["^var^"]"
    |   ElementToString(NoElement)= "No Element character"

    val varList= Array.array(1000, ""); (*the variable name stored at index i has the value of that variable at index i in M*)
    val M = Array.array(1000, 100000000); 
    val varCount= ref 0;

    fun initialise_M([]) = ()
        |  initialise_M(INT(VARIABLE(var)) :: t) = 
                (Array.update(varList, !varCount, var); varCount := (!varCount)+ 1; initialise_M(t))
        |  initialise_M(BOOL(VARIABLE(var)) :: t) = 
                (Array.update(varList, !varCount, var); varCount := (!varCount)+ 1; initialise_M(t));

    fun findIndexOfVar(var) =
        let fun lookAt(i) = if Array.sub(varList, i)= var then i else lookAt(i+1);
        in lookAt(0)
        end;

    fun M_toString()=
        let fun varAt(i) = if Array.sub(varList, i) = "" then "\n" else Array.sub(varList, i) ^ " has value " ^ Int.toString(Array.sub(M, i))^"\n"^ varAt(i+1);
        in varAt(0)
        end;
end;

open elementDefinition

signature VMC =
    sig
        type 'a Stack;
        val toString: V_C_element Stack * int array * V_C_element Stack -> string;    
        val rules: V_C_element Stack * int array * V_C_element Stack -> V_C_element Stack * int array * V_C_element Stack;
end;

structure Vmc : VMC =
    struct
        type 'a Stack = 'a list;
        fun ToString f l = 
        let fun toStringFromList f [] = ""
            |  toStringFromList f (h :: t) = f(h) ^ ", " ^ (toStringFromList f t)
        in "[" ^ toStringFromList f l ^ "]\n"
        end;

        fun toString(V, M, C)=
            "V: "^(ToString ElementToString V)^ "M: "^ M_toString() ^ "C: "^(ToString ElementToString C)
    
        fun rules(V, M, ExpressionElement(b) :: SeqElement(c) :: InstructionElement("WHILE") :: C) =
                (SeqElement(c) :: ExpressionElement(b) :: V , M, ExpressionElement(b) :: InstructionElement("WHILE") :: C)

        |   rules(ConstantElement(m) :: SeqElement(c) :: ExpressionElement(b) :: V, M, InstructionElement("WHILE") :: C)= 
               if m = 0 then 
                    (V, M, C)
               else 
                    (V, M, SeqElement(c) :: ExpressionElement(b) :: SeqElement(c) :: InstructionElement("WHILE") :: C) 

        |   rules(ConstantElement(m) :: V, M, SeqElement(c) :: SeqElement(d) :: InstructionElement("ITE") :: C) = 
                if m = 0 then
                    (V, M, SeqElement(d) :: C)
                else
                    (V, M, SeqElement(c) :: C)

        |   rules(V, M, SeqElement(c, d) :: C) = 
                (V, M, c :: d :: C)
        
        |   rules(V, M, CommandElement(c1, c2, c3, c4) :: C) = 
                (V, M, c1 :: c2 :: c3 :: c4 :: C)
            
        |   rules(V, M, VariableElement(x) :: ExpressionElement(e) :: InstructionElement("SET") :: C) = 
                (VariableElement(x) :: V, M, ExpressionElement(e) :: InstructionElement("SET") :: C)
        
        |   rules(ConstantElement(m) :: VariableElement(x) :: V, M, InstructionElement("SET") :: C) = 
                (Array.update(M, findIndexOfVar(x), m); (V, M, C))

        |   rules (ConstantElement(m) :: V, M, InstructionElement("WRITE") :: C) =
                (print(Int.toString(m)^"\n"); (V, M, C))

        |   rules (V, M, VariableElement(x) :: InstructionElement("READ") :: C) =
                let val str= valOf(TextIO.inputLine TextIO.stdIn)
                    val m= valOf(Int.fromString str)
                in
                    (Array.update(M, findIndexOfVar(x), m); (V, M, C))
                end

        |   rules(ConstantElement(m) :: V, M, UnaryOperator(k) :: C) =
                if k = "NOT" then
                    if m = 0 then (ConstantElement(1) :: V, M, C) else (ConstantElement(0) :: V, M, C)
                else 
                    (ConstantElement((~1) * m) :: V, M, C)

        |   rules(ConstantElement(n) :: ConstantElement(m) :: V, M, BinaryOperator(k) :: C) = 
                if k = "PLUS" then (ConstantElement(m + n) :: V, M, C)
                else if k = "MINUS" then (ConstantElement(m - n) :: V, M, C)
                else if k = "TIMES" then (ConstantElement(m * n) :: V, M, C)
                else if k = "DIV" then if n = 0 then (print("Error: Division by zero"); raise FunStack.Error("Error: Division by zero"); (V, M, C)) else (ConstantElement(m div n) :: V, M, C)
                else if k = "MOD" then if n = 0 then (print("Error: Modulo by zero"); raise FunStack.Error("Modulo by zero error" ); (V, M, C)) else (ConstantElement(m mod n) :: V, M, C)
                else if k = "AND" then if (m = 0) orelse (n = 0) then (ConstantElement(0) :: V, M, C) else (ConstantElement(1) :: V, M, C)
                else if k = "OR" then if (m = 1) orelse (n = 1) then (ConstantElement(1) :: V, M, C) else (ConstantElement(0) :: V, M, C)
                else if k = "EQ" then if m = n then (ConstantElement(1) :: V, M, C) else (ConstantElement(0) :: V, M, C)
                else if k = "NEQ" then if m <> n then (ConstantElement(1) :: V, M, C) else (ConstantElement(0) :: V, M, C)
                else if k = "LT" then if m < n then (ConstantElement(1) :: V, M, C) else (ConstantElement(0) :: V, M, C)
                else if k = "GT" then if m > n then (ConstantElement(1) :: V, M, C) else (ConstantElement(0) :: V, M, C)
                else if k = "LEQ" then if m <= n then (ConstantElement(1) :: V, M, C) else (ConstantElement(0) :: V, M, C)
                else if k = "GEQ" then if m >= n then (ConstantElement(1) :: V, M, C) else (ConstantElement(0) :: V, M, C)
                else (V, M, C)

        |   rules(V, M, ExpressionElement(b1, b2, b3) :: C) = 
                (V, M, b1 :: b2 :: b3 :: C)

        |   rules(V, M, VariableElement(x) :: C) = 
        let val m = Array.sub(M, findIndexOfVar(x))
        in if m = 100000000 then (print("ERROR: Uninitialised variable:" ^ x  ^ "\n"); raise FunStack.Error("Uninitialised variable error"); (V, M, C))
                else(ConstantElement(Array.sub(M, findIndexOfVar(x))) :: V, M, C)
        end    

        |   rules(V, M, ConstantElement(m) :: C) = 
                (ConstantElement(m) :: V, M, C)
        
        |   rules(V, M, NoElement :: C) = 
                (V, M, C)

end;

open Vmc

(*postfix: AST list -> V_C_element*)
fun postfix([])= SeqElement(NoElement, NoElement)

 |  postfix([SEQ(comSeq)])= postfix(comSeq)

 |  postfix(SET(VARIABLE(var), exp):: comSeq)= SeqElement(CommandElement(VariableElement(var), postfix([exp]), InstructionElement("SET"), NoElement), postfix(comSeq)) 

 |  postfix(ITE(exp, comBlock1, comBlock2):: comSeq)= SeqElement(CommandElement(postfix([exp]), postfix([comBlock1]), postfix([comBlock2]), InstructionElement("ITE")), postfix(comSeq))

 |  postfix(WH(exp, comBlock):: comSeq)= SeqElement(CommandElement(postfix([exp]), postfix([comBlock]), InstructionElement("WHILE"), NoElement), postfix(comSeq))

 |  postfix(READ(VARIABLE(var)):: comSeq)= SeqElement(CommandElement(VariableElement(var), InstructionElement("READ"), NoElement, NoElement), postfix(comSeq))

 |  postfix(WRITE(exp):: comSeq)= SeqElement(CommandElement(postfix([exp]), InstructionElement("WRITE"), NoElement, NoElement), postfix(comSeq))

|   postfix([IEXP(VARIABLE(var))])=ExpressionElement(VariableElement(var), NoElement, NoElement)

|   postfix([BEXP(VARIABLE(var))])= ExpressionElement(VariableElement(var), NoElement, NoElement)

|   postfix([IEXP(CONSTANT(num))])= ExpressionElement(ConstantElement(num), NoElement, NoElement)

|   postfix([BEXP(FF)])= ExpressionElement(ConstantElement(0), NoElement, NoElement)

|   postfix([BEXP(TT)])= ExpressionElement(ConstantElement(1), NoElement, NoElement)

|   postfix([IEXP(NEGSIGN(op1))])= ExpressionElement(postfix([op1]), UnaryOperator("UMINUS"), NoElement)

|   postfix([BEXP(NOT(op1))])= ExpressionElement(postfix([op1]), UnaryOperator("NOT"), NoElement)

|   postfix([IEXP(PLUS(op1, op2))])= ExpressionElement(postfix([op1]), postfix([op2]), BinaryOperator("PLUS"))

|   postfix([IEXP(MINUS(op1, op2))])= ExpressionElement(postfix([op1]), postfix([op2]), BinaryOperator("MINUS"))

|   postfix([IEXP(TIMES(op1, op2))])= ExpressionElement(postfix([op1]), postfix([op2]), BinaryOperator("TIMES"))

|   postfix([IEXP(DIV(op1, op2))])= ExpressionElement(postfix([op1]), postfix([op2]), BinaryOperator("DIV"))

|   postfix([IEXP(MOD(op1, op2))])= ExpressionElement(postfix([op1]), postfix([op2]), BinaryOperator("MOD"))

|   postfix([BEXP(AND(op1, op2))])= ExpressionElement(postfix([op1]), postfix([op2]), BinaryOperator("AND"))

|   postfix([BEXP(OR(op1, op2))])= ExpressionElement(postfix([op1]), postfix([op2]), BinaryOperator("OR"))

|   postfix([BEXP(EQ(op1, op2))])= ExpressionElement(postfix([op1]), postfix([op2]), BinaryOperator("EQ"))

|   postfix([BEXP(NEQ(op1, op2))])= ExpressionElement(postfix([op1]), postfix([op2]), BinaryOperator("NEQ"))

|   postfix([BEXP(LT(op1, op2))])= ExpressionElement(postfix([op1]), postfix([op2]), BinaryOperator("LT"))

|   postfix([BEXP(GT(op1, op2))])= ExpressionElement(postfix([op1]), postfix([op2]), BinaryOperator("GT"))

|   postfix([BEXP(GEQ(op1, op2))])= ExpressionElement(postfix([op1]), postfix([op2]), BinaryOperator("GEQ"))

|   postfix([BEXP(LEQ(op1, op2))])= ExpressionElement(postfix([op1]), postfix([op2]), BinaryOperator("LEQ"))

fun execute(postfixCommandSeq)=
    let val C : V_C_element Stack = [postfixCommandSeq]
        val V= []
        fun operate(V, M, C) =  
            if C = [] then (V, M, C) else 
                let val newState = rules(V, M, C)
                in operate(newState)
                end
    in
        operate(V, M, C)
    end;

fun WhileLang(s)=
    let val tree = parseFile(s);
        val toExecute = if !parseStatus then true else (print("Error: Parse Unsuccesful"); raise FunStack.Error("parse unsuccessful error"); false)
        val (PROG(_, BLK(DeclarationSeq, CommandBlock)))= tree;
        val postfixCommandSeq= postfix([CommandBlock]);
        val initMem = initialise_M(DeclarationSeq)
        val exec = execute(postfixCommandSeq)
        val strFinal = toString(exec)
    in
        print(strFinal)
    end;