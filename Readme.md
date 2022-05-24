# COL226: The VMC machine for the ASTs of WHILE

***Kushal Kumar Gupta, 2020CS10355***

### How to use:

You must have `SML`, `ML-Lex` and `ML-Yacc` installed. Go to the source folder in the command prompt and type-

```bash
make
```

Then, to execute a program file, type

```ocaml
WhileLang("fileName");
```

where `fileName` is the name of the program file. 

Any parse errors will be displayed on the terminal and the program will not execute. If there are no parse errors, the program will execute. Any run-time errors will be displayed and the program execution will stop. 

NOTE: To execute another program, exit the SML prompt (`CTRL + Z`) and start again by typing  `make`. This is done to clear the variable list and memory of previous execution.

### Design Decisions:

1. Structure `FunStack` defines the Stack type to be simply the List datatype. 

2. Structure `ElementDefiniton` defines the stack element `V_C_element` for the Value and Command stack of the VMC Machine. 

   ```ocaml
       datatype V_C_element = CommandElement of V_C_element * V_C_element * V_C_element * V_C_element
                       |      ExpressionElement of V_C_element * V_C_element * V_C_element  
                       |      SeqElement of V_C_element * V_C_element
                       |      VariableElement of string 
                       |      ConstantElement of int 
                       |      BinaryOperator of string 
                       |      UnaryOperator of string 
                       |      InstructionElement of string 
                       |      NoElement;
   ```

   Memory stack `M` has been defines as an int array of size `1000`, initialized to `100000000`. This initial value of variables is used to check whether the variable is being used without initialization. Another string array `varList` of size 1000 is used to store the variable names where the index corresponding to each variable gives the value of the variable in the Memory stack . The function `initialise_M` takes the declaration sequence from the parse tree and stores the variables in `varList`. Function `findIndexOfVar` takes a variable name and returns its corresponding index. Functions `ElementToString` converts a `V_C_element` to string, and `M_toString` gives the string corresponding to the Memory stack.

   

3. Structure `Vmc` contains the function `toString` which gives the string corresponding to the state of the `VMC` given as input. Function `rules` takes a state of the VMC and gives the next state after applying one of the rules.

   

4. Function `postfix` takes an AST list and gives the `V_C_element` corresponding to it after converting the instructions and expressions to postfix form. It uses the various constructors of `V_C_element` to handle all the `AST` constructors of the input.

   

5. Function `execute` takes the Postfix Command Sequence and runs the VMC machine till the Command Stack becomes empty.

   

6. Function `WhileLang`  first generates the parse tree of the input file using the `parseFile` function defined in the previous assignment. Then, if there are no parse errors, it obtains the postfix command sequence from `postfix` function initializes the array of variables using `initialise_M`. Then it calls the  `execute` function and runs the machine.

   

7. If there is multiple declaration of variables, use of variable without declaration or type mismatch in expressions, assignment statements anywhere, the program is parsed completely and all these errors are reported. However, program execution does not take place. Run time errors include division by zero, modulus operator with zero and use of uninitialized variables.



#### Context-free grammar



Terminals are 

`PROG , CONS , VAR , COLON , SEMICOLON , INT , BOOL , COMMA , LBRACE , RBRACE , SET , READ , WRITE , IF , THEN , ELSE , ENDIF , WHILE , DO , ENDWH , LPAREN , RPAREN , NEGSIGN , OR , AND , NOT , LT , LEQ , EQ , GT , GEQ , NEQ , PLUS , MINUS , TIMES , DIV , MOD , INTCONST of int , TT , FF , IDENTIFIER of string , EOF , ILLCH`

Non Terminals are

  `Program, Block, DeclarationSeq, CommandBlock, CommandSeq, Command, Declaration, VariableList, Type, Expression`

Expression has been shortened as E below.

Rules are
\begin{align*}
$$ \text{Program} \to  \text{program} \quad \text{id} \quad \text{::}\quad  \text{Block}\\ $$

$$ \text{Block} \to \text{DeclarationSeq} \quad \text{CommandBlock}\\ $$

$$ \text{DeclarationSeq} \to \text{Declaration} \quad \text{;} \quad \text{DeclarationSeq} \\ $$

$$ \text{Declaration} \to \text{var   } \text{VariableList} \text{: } \text{Type}\\ $$

$$ \text{VariableList}\to \text{id} \text{,} \text{VariableList   | id}\\ $$

$$ \text{Type} \to \text{INT | BOOL }  \\\ $$

$$ \text{CommandBlock}\to \text{\{$CommandSeq$ \}}\\ $$

$$ \text{$CommandSeq$}\to \text{$Command$ ; $CommandSeq$ | }\epsilon\\ $$

$$ \text{$Command$}\to \text{id} := \text{E}\\  $$

$$ | \text{ read I}\\ $$

$$ | \text{ write E}\\ $$

$$ | \text{ if E then $CommandBlock$ else $CommandBlock$ endif }\\ $$

$$ | \text{ while E do $CommandBlock$ endwh}\\ $$

$$ \text{E}\to \text{E + E | E - E | E * E | E / E | E \% E | }\\ \text{E \&\& E | E || E | ! E | tt | ff | integer | E < E  } \\ \text{| E > E | E <= E | E >= E | E = E | E <> E }\\\text{| ~ E  | ( E ) | id } $$


### AST datatype definition

Defined in while_ast.sml under structure `While_AST`

```s
structure While_AST =
struct
    datatype BLK = BLK of AST list * AST 
    and      AST = PROG of AST * BLK
              |    SEQ of AST list
              |    SET of AST * AST
              |    ITE of AST * AST * AST
              |    WH of AST * AST
              |    READ of AST
              |    WRITE of AST
              |    VARIABLE of string
              |    INT of AST
              |    BOOL of AST
              |    CONSTANT of int
              |    TT
              |    FF
              |    NOT of AST 
              |    AND of AST * AST
              |    OR of AST * AST 
              |    LT of AST * AST  
              |    GT of AST * AST  
              |    LEQ of AST * AST  
              |    NEQ of AST * AST  
              |    EQ of AST * AST  
              |    GEQ of AST * AST  
              |    PLUS of AST * AST 
              |    MINUS of AST * AST 
              |    TIMES of AST * AST 
              |    DIV of AST * AST 
              |    MOD of AST * AST
              |    NEGSIGN of AST
              |    BEXP of AST
              |    IEXP of AST;

    type TYPE = AST -> AST;
    fun confirmExpType(IEXP(_), "INT") = true 
    |   confirmExpType(BEXP(_), "BOOL") = true
    |   confirmExpType(x, y) = false;
end;
```

#### Other auxiliary datatypes

`Datatype BLK` and `type TYPE` as can be seen above

### Syntax-directed translation

Can be seen alongside the production rules in `WhileLang.yacc`

#### Auxiliary functions and Data

Checked for declaration before use and multiple declaration for same identifier by maintaining an array of declared variables.

For this, I have used

`val LUT = Array.array(10000, ("" , ""));`

`val varCount = ref 0;`

`fun lookIfPresent(LUT, s)`

`fun returnVarType(LUT, s)`



and functions for reporting errors are 

`DecAgainErr : string -> unit`

`NotDeclaredErr : string * int * int`

`val typeError: int * int -> unit`



For concatenating 2 lists, I have used

`fun append`



and applying INT or BOOL constructor to the list of variables I have used funtion-

`fun applyType`



### Other Design Decisions

#### Changing the grammar

The EBNF given is ambiguous (identifier can be reduced to both Int expression and bool expression), hence I have changed the grammar as can be seen above. My grammar becomes unambiguous after considering the operator precendence and associativity as given (feature of YACC).


### Other Implementation Decisions

#### Error Handling ( Declaration before Use and Type Mismatch )

As described above, used extra functions and data structures.



### Testcases:

Some testcases and their output have been included.



### Acknowledgements

1.)PDF titled `ML-Yacc User's Manual` by `David R. Tarditi`  and `Andrew Appel` for guidelines for using `ML-lex` and `ML-yacc`, as well as for the code for linking lexer to the parser and I/O.

I2.)this [site](https://www.cs.princeton.edu/~appel/modern/ml/ml-yacc/manual.html) for glue code in the `load-while.sml` file.
