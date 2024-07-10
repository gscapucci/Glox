import expr.{type Expr}
import token.{type Token}

pub type Expression {
  Expression(expression: Expr)
}

pub type Print {
  Print(expression: Expr)
}

pub type Var {
  Var(name: Token, initializer: Expr)
}

pub type Stmt {
  StmtExpression(Expression)
  StmtPrint(Print)
  StmtVar(Var)
  StmtNone
}
