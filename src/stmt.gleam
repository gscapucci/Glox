import expr.{type Expr}

pub type Expression {
    Expression(expression: Expr)
}

pub type Print {
    Print(expression: Expr)
}

pub type Stmt {
    StmtExpression(Expression)
    StmtPrint(Print)
}