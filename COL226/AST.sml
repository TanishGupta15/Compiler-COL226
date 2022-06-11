structure ASTree =
struct
type id = string
exception Error

datatype node_data = LT | LEQ | EQ | GT | GEQ | NEQ | PROG | INT | BOOL | TT | FF | NOT | AND | OR | PLUS | MINUS | TIMES | DIV | MOD | BLK | DES | VL | SEQ | SET | READ | WRITE | ITE | WH | IEXP | BEXP | NEGATION | Var | DE | ID | NUM;
datatype AST = empty | node of node_data*AST*AST | node3 of node_data*AST*AST*AST | idnode of node_data * string* AST | intnode of node_data * int * AST;

end