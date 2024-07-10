import object.{type Object}
import token.{type Token}

pub type Binary {
  Binary(left: Expr, operator: Token, right: Expr)
}

pub type Grouping {
  Grouping(expression: Expr)
}

pub type Literal {
  Literal(value: Object)
}

pub type Unary {
  Unary(operator: Token, right: Expr)
}

pub type Var {
  Var(name: Token)
}

pub type Expr {
  ExprBinary(Binary)
  ExprGrouping(Grouping)
  ExprLiteral(Literal)
  ExprUnary(Unary)
  ExprVariable(Var)
  ExprNone
}

pub fn to_string(expr: Expr) -> String {
  case expr {
    ExprBinary(Binary(left, operand, right)) ->
      "( "
      <> operand.lexeme
      <> " "
      <> left |> to_string
      <> " "
      <> right |> to_string
      <> " )"
    ExprGrouping(Grouping(expr)) -> "( grouping " <> expr |> to_string <> " )"
    ExprLiteral(Literal(value)) -> value |> object.to_string
    ExprUnary(Unary(op, right)) ->
      "( " <> op.lexeme <> " " <> right |> to_string <> ")"
    ExprVariable(Var(name)) -> "var(" <> name.lexeme <> ")"
    ExprNone -> " NONE "
  }
}
