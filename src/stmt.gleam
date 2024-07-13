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

pub type Block {
  Block(statements: List(Stmt))
}

pub type If {
  If(condition: Expr, thenb: Stmt, elseb: Stmt)
}

pub type While {
  While(condition: Expr, body: Stmt)
}

pub type Stmt {
  StmtExpression(Expression)
  StmtPrint(Print)
  StmtVar(Var)
  StmtBlock(Block)
  StmtIf(If)
  StmtWhile(While)
  StmtNone
}
