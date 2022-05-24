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
    val parseStatus = ref true;
end;