import expr.{type Expr}
import gleam/bool
import gleam/float
import gleam/int
import gleam/io
import gleam/list.{Continue, Stop}
import gleam/result
import object.{type Object, Object}
import stmt.{type Stmt}
import token_type

pub type Interpreter {
  Interpreter
}

pub fn interpret(interp: Interpreter, stmts: List(Stmt)) -> Result(Nil, String) {
  stmts
  |> list.fold_until(Ok(Nil), fn(acc: Result(Nil, String), i: Stmt) {
    case acc {
      Ok(Nil) -> Continue(execute(interp, i))
      Error(str) -> Stop(Error(str))
    }
  })
}

pub fn execute(i: Interpreter, stmt: Stmt) -> Result(Nil, String) {
  stmt |> accept_stmt(i)
}

pub fn accept_expr(expr: Expr, i: Interpreter) -> Result(Object, String) {
  case expr {
    expr.ExprBinary(b) -> visit_binary_expr(i, b)
    expr.ExprGrouping(g) -> visit_grouping_expr(i, g)
    expr.ExprLiteral(l) -> visit_literal_expr(i, l) |> Ok
    expr.ExprUnary(u) -> visit_unary_expr(i, u)
  }
}

pub fn accept_stmt(stmt: Stmt, i: Interpreter) -> Result(Nil, String) {
  case stmt {
    stmt.StmtExpression(e) -> visit_expression_stmt(i, e)
    stmt.StmtPrint(p) -> visit_print_stmt(i, p)
  }
}

pub fn visit_literal_expr(_i: Interpreter, e: expr.Literal) -> Object {
  e.value
}

pub fn visit_grouping_expr(
  i: Interpreter,
  e: expr.Grouping,
) -> Result(Object, String) {
  evaluate(i, e.expression)
}

pub fn evaluate(i: Interpreter, e: Expr) -> Result(Object, String) {
  e |> accept_expr(i)
}

pub fn visit_unary_expr(i: Interpreter, e: expr.Unary) -> Result(Object, String) {
  let right: Result(Object, String) = i |> evaluate(e.right)
  use <- bool.guard(when: right |> result.is_error, return: right)
  let assert Ok(right): Result(Object, String) = right
  case e.operator.ttype {
    token_type.Bang -> {
      let ret = !is_truthy(right)
      Object(object.ObjTypeBool(ret)) |> Ok
    }
    token_type.Minus -> {
      case right.ttype {
        object.ObjTypeInt(n) -> Object(object.ObjTypeInt(n |> int.negate))
        object.ObjTypeFloat(n) -> Object(object.ObjTypeFloat(n |> float.negate))
        _ -> Object(object.None)
      }
      |> Ok
    }
    _ -> Object(object.None) |> Ok
  }
}

pub fn is_truthy(o: Object) -> Bool {
  case o {
    Object(object.None) -> False
    Object(object.ObjTypeBool(b)) -> b
    _ -> True
  }
}

pub fn visit_binary_expr(
  i: Interpreter,
  e: expr.Binary,
) -> Result(Object, String) {
  let left: Result(Object, String) = evaluate(i, e.left)
  use <- bool.guard(when: left |> result.is_error, return: left)
  let assert Ok(left): Result(Object, String) = left

  let right: Result(Object, String) = evaluate(i, e.right)
  use <- bool.guard(when: right |> result.is_error, return: right)
  let assert Ok(right): Result(Object, String) = right

  case e.operator.ttype {
    token_type.Minus -> {
      case left, right {
        Object(object.ObjTypeInt(l)), Object(object.ObjTypeInt(r)) ->
          Object(object.ObjTypeInt(l - r)) |> Ok
        Object(object.ObjTypeFloat(l)), Object(object.ObjTypeFloat(r)) ->
          Object(object.ObjTypeFloat(l -. r)) |> Ok
        _, _ -> Error("Invalid Operands")
      }
    }
    token_type.Slash -> {
      case left, right {
        Object(object.ObjTypeInt(l)), Object(object.ObjTypeInt(r)) ->
          Object(object.ObjTypeInt(l / r)) |> Ok
        Object(object.ObjTypeFloat(l)), Object(object.ObjTypeFloat(r)) ->
          Object(object.ObjTypeFloat(l /. r)) |> Ok
        _, _ -> Error("Invalid Operands")
      }
    }
    token_type.Star -> {
      case left, right {
        Object(object.ObjTypeInt(l)), Object(object.ObjTypeInt(r)) ->
          Object(object.ObjTypeInt(l * r)) |> Ok
        Object(object.ObjTypeFloat(l)), Object(object.ObjTypeFloat(r)) ->
          Object(object.ObjTypeFloat(l *. r)) |> Ok
        _, _ -> Error("Invalid Operands")
      }
    }
    token_type.Plus -> {
      case left, right {
        Object(object.ObjTypeInt(l)), Object(object.ObjTypeInt(r)) ->
          Object(object.ObjTypeInt(l + r)) |> Ok
        Object(object.ObjTypeFloat(l)), Object(object.ObjTypeFloat(r)) ->
          Object(object.ObjTypeFloat(l +. r)) |> Ok
        Object(object.ObjTypeString(l)), Object(object.ObjTypeString(r)) ->
          Object(object.ObjTypeString(l <> r)) |> Ok
        _, _ -> Error("Invalid Operands")
      }
    }
    token_type.Greater -> {
      case left, right {
        Object(object.ObjTypeInt(l)), Object(object.ObjTypeInt(r)) ->
          Object(object.ObjTypeBool(l > r)) |> Ok
        Object(object.ObjTypeFloat(l)), Object(object.ObjTypeFloat(r)) ->
          Object(object.ObjTypeBool(l >. r)) |> Ok
        Object(object.ObjTypeString(_)), Object(object.ObjTypeString(_)) -> todo
        _, _ -> Error("Invalid Operands")
      }
    }
    token_type.GreaterEqual -> {
      case left, right {
        Object(object.ObjTypeInt(l)), Object(object.ObjTypeInt(r)) ->
          Object(object.ObjTypeBool(l >= r)) |> Ok
        Object(object.ObjTypeFloat(l)), Object(object.ObjTypeFloat(r)) ->
          Object(object.ObjTypeBool(l >=. r)) |> Ok
        Object(object.ObjTypeString(_)), Object(object.ObjTypeString(_)) -> todo
        _, _ -> Error("Invalid Operands")
      }
    }
    token_type.Less -> {
      case left, right {
        Object(object.ObjTypeInt(l)), Object(object.ObjTypeInt(r)) ->
          Object(object.ObjTypeBool(l < r)) |> Ok
        Object(object.ObjTypeFloat(l)), Object(object.ObjTypeFloat(r)) ->
          Object(object.ObjTypeBool(l <. r)) |> Ok
        Object(object.ObjTypeString(_)), Object(object.ObjTypeString(_)) -> todo
        _, _ -> Error("Invalid Operands")
      }
    }
    token_type.LessEqual -> {
      case left, right {
        Object(object.ObjTypeInt(l)), Object(object.ObjTypeInt(r)) ->
          Object(object.ObjTypeBool(l <= r)) |> Ok
        Object(object.ObjTypeFloat(l)), Object(object.ObjTypeFloat(r)) ->
          Object(object.ObjTypeBool(l <=. r)) |> Ok
        Object(object.ObjTypeString(_)), Object(object.ObjTypeString(_)) -> todo
        _, _ -> Error("Invalid Operands")
      }
    }
    token_type.BangEqual -> {
      Object(object.ObjTypeBool(!is_equal(left, right))) |> Ok
    }
    token_type.EqualEqual -> {
      Object(object.ObjTypeBool(is_equal(left, right))) |> Ok
    }
    _ -> Error("Invalid Operator")
  }
}

pub fn is_equal(left: Object, right: Object) -> Bool {
  left == right
}

pub fn visit_expression_stmt(
  i: Interpreter,
  stmt: stmt.Expression,
) -> Result(Nil, String) {
  case i |> evaluate(stmt.expression) {
    Ok(_) -> Ok(Nil)
    Error(err) -> Error(err)
  }
}

pub fn visit_print_stmt(i: Interpreter, stmt: stmt.Print) -> Result(Nil, String) {
  case i |> evaluate(stmt.expression) {
    Ok(obj) -> {
      io.println(obj |> object.to_string)
      Ok(Nil)
    }
    Error(err) -> Error(err)
  }
}
