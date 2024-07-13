import environment.{type Environment}
import error.{type LoxError, RuntimeError}
import expr.{type Expr}
import gleam/bool
import gleam/float
import gleam/int
import gleam/io
import gleam/list.{Continue, Stop}
import gleam/option.{Some}
import gleam/order
import gleam/result
import gleam/string
import object.{type Object, Object}
import stmt.{type Stmt}
import token_type

pub type Interpreter {
  Interpreter(environment: Environment)
}

pub fn interpret(
  interp: Interpreter,
  stmts: List(Stmt),
) -> Result(Interpreter, LoxError) {
  stmts
  |> list.fold_until(
    Ok(interp),
    fn(acc: Result(Interpreter, LoxError), x: Stmt) {
      case acc {
        Ok(i) -> Continue(execute(i, x))
        Error(str) -> Stop(Error(str))
      }
    },
  )
}

pub fn execute(i: Interpreter, stmt: Stmt) -> Result(Interpreter, LoxError) {
  stmt |> accept_stmt(i)
}

pub fn accept_expr(
  expr: Expr,
  i: Interpreter,
) -> Result(#(Interpreter, Object), LoxError) {
  case expr {
    expr.ExprBinary(b) -> visit_binary_expr(i, b)
    expr.ExprGrouping(g) -> visit_grouping_expr(i, g)
    expr.ExprLiteral(l) -> #(i, visit_literal_expr(i, l)) |> Ok
    expr.ExprUnary(u) -> visit_unary_expr(i, u)
    expr.ExprVariable(v) -> {
      case visit_variable_expr(i, v) {
        Error(err) -> Error(err)
        Ok(o) -> #(i, o) |> Ok
      }
    }
    expr.ExprAssign(a) -> visit_assign_expr(i, a)
    expr.ExprLogical(l) -> visit_logical_expr(i, l)
    expr.ExprNone -> RuntimeError("Unreachable") |> Error
  }
}

pub fn accept_stmt(stmt: Stmt, i: Interpreter) -> Result(Interpreter, LoxError) {
  case stmt {
    stmt.StmtExpression(e) -> visit_expression_stmt(i, e)
    stmt.StmtPrint(p) -> visit_print_stmt(i, p)
    stmt.StmtVar(v) -> visit_var_stmt(i, v)
    stmt.StmtNone -> RuntimeError("Unreachable") |> Error
    stmt.StmtBlock(b) -> visit_block_stmt(i, b)
    stmt.StmtIf(iff) -> visit_if_stmt(i, iff)
    stmt.StmtWhile(w) -> visit_while_stmt(i, w)
  }
}

pub fn visit_literal_expr(_i: Interpreter, e: expr.Literal) -> Object {
  e.value
}

pub fn visit_grouping_expr(
  i: Interpreter,
  e: expr.Grouping,
) -> Result(#(Interpreter, Object), LoxError) {
  evaluate(i, e.expression)
}

pub fn evaluate(
  i: Interpreter,
  e: Expr,
) -> Result(#(Interpreter, Object), LoxError) {
  e |> accept_expr(i)
}

pub fn visit_unary_expr(
  i: Interpreter,
  e: expr.Unary,
) -> Result(#(Interpreter, Object), LoxError) {
  use #(i, right): #(Interpreter, Object) <- result.try(i |> evaluate(e.right))
  case e.operator.ttype {
    token_type.Bang -> {
      let right: Object =
        right
        |> is_truthy
        |> bool.negate
        |> object.ObjTypeBool
        |> Object
      #(i, right) |> Ok
    }
    token_type.Minus -> {
      case right.ttype {
        object.ObjTypeInt(n) -> #(i, Object(object.ObjTypeInt(n |> int.negate)))
        object.ObjTypeFloat(n) -> #(
          i,
          Object(object.ObjTypeFloat(n |> float.negate)),
        )
        _ -> #(i, Object(object.None))
      }
      |> Ok
    }
    _ -> #(i, Object(object.None)) |> Ok
  }
}

pub fn is_truthy(o: Object) -> Bool {
  case o {
    Object(object.None) -> False
    Object(object.ObjTypeBool(b)) -> b
    Object(object.ObjTypeInt(0)) -> False
    Object(object.ObjTypeNil) -> False
    _ -> True
  }
}

pub fn visit_binary_expr(
  i: Interpreter,
  e: expr.Binary,
) -> Result(#(Interpreter, Object), LoxError) {
  use #(i, left): #(Interpreter, Object) <- result.try(evaluate(i, e.left))
  use #(i, right): #(Interpreter, Object) <- result.try(evaluate(i, e.right))

  case e.operator.ttype {
    token_type.Minus -> {
      case left, right {
        Object(object.ObjTypeInt(l)), Object(object.ObjTypeInt(r)) ->
          #(i, Object(object.ObjTypeInt(l - r))) |> Ok
        Object(object.ObjTypeFloat(l)), Object(object.ObjTypeFloat(r)) ->
          #(i, Object(object.ObjTypeFloat(l -. r))) |> Ok
        l, r ->
          RuntimeError(error.report_token_as_string(
            e.operator,
            "Invalid Operands(Left: "
              <> l |> object.to_string
              <> ", Right: "
              <> r |> object.to_string,
          ))
          |> Error
      }
    }
    token_type.Slash -> {
      case left, right {
        Object(object.ObjTypeInt(l)), Object(object.ObjTypeInt(r)) ->
          #(i, Object(object.ObjTypeInt(l / r))) |> Ok
        Object(object.ObjTypeFloat(l)), Object(object.ObjTypeFloat(r)) ->
          #(i, Object(object.ObjTypeFloat(l /. r))) |> Ok
        l, r ->
          RuntimeError(error.report_token_as_string(
            e.operator,
            "Invalid Operands(Left: "
              <> l |> object.to_string
              <> ", Right: "
              <> r |> object.to_string,
          ))
          |> Error
      }
    }
    token_type.Star -> {
      case left, right {
        Object(object.ObjTypeInt(l)), Object(object.ObjTypeInt(r)) ->
          #(i, Object(object.ObjTypeInt(l * r))) |> Ok
        Object(object.ObjTypeFloat(l)), Object(object.ObjTypeFloat(r)) ->
          #(i, Object(object.ObjTypeFloat(l *. r))) |> Ok
        l, r ->
          RuntimeError(error.report_token_as_string(
            e.operator,
            "Invalid Operands(Left: "
              <> l |> object.to_string
              <> ", Right: "
              <> r |> object.to_string,
          ))
          |> Error
      }
    }
    token_type.Plus -> {
      case left, right {
        Object(object.ObjTypeInt(l)), Object(object.ObjTypeInt(r)) ->
          #(i, Object(object.ObjTypeInt(l + r))) |> Ok
        Object(object.ObjTypeFloat(l)), Object(object.ObjTypeFloat(r)) ->
          #(i, Object(object.ObjTypeFloat(l +. r))) |> Ok
        Object(object.ObjTypeString(l)), Object(object.ObjTypeString(r)) ->
          #(i, Object(object.ObjTypeString(l <> r))) |> Ok
        l, r ->
          RuntimeError(error.report_token_as_string(
            e.operator,
            "Invalid Operands(Left: "
              <> l |> object.to_string
              <> ", Right: "
              <> r |> object.to_string,
          ))
          |> Error
      }
    }
    token_type.Greater -> {
      case left, right {
        Object(object.ObjTypeInt(l)), Object(object.ObjTypeInt(r)) ->
          #(i, Object(object.ObjTypeBool(l > r))) |> Ok
        Object(object.ObjTypeFloat(l)), Object(object.ObjTypeFloat(r)) ->
          #(i, Object(object.ObjTypeBool(l >. r))) |> Ok
        Object(object.ObjTypeString(l)), Object(object.ObjTypeString(r)) -> {
          #(
            i,
            { string.compare(l, r) == order.Gt }
              |> object.ObjTypeBool
              |> Object,
          )
          |> Ok
        }
        l, r ->
          RuntimeError(error.report_token_as_string(
            e.operator,
            "Invalid Operands(Left: "
              <> l |> object.to_string
              <> ", Right: "
              <> r |> object.to_string,
          ))
          |> Error
      }
    }
    token_type.GreaterEqual -> {
      case left, right {
        Object(object.ObjTypeInt(l)), Object(object.ObjTypeInt(r)) ->
          #(i, Object(object.ObjTypeBool(l >= r))) |> Ok
        Object(object.ObjTypeFloat(l)), Object(object.ObjTypeFloat(r)) ->
          #(i, Object(object.ObjTypeBool(l >=. r))) |> Ok
        Object(object.ObjTypeString(l)), Object(object.ObjTypeString(r)) -> {
          let cmp = string.compare(l, r)
          #(
            i,
            { cmp == order.Gt || cmp == order.Eq }
              |> object.ObjTypeBool
              |> Object,
          )
          |> Ok
        }
        l, r ->
          RuntimeError(error.report_token_as_string(
            e.operator,
            "Invalid Operands(Left: "
              <> l |> object.to_string
              <> ", Right: "
              <> r |> object.to_string,
          ))
          |> Error
      }
    }
    token_type.Less -> {
      case left, right {
        Object(object.ObjTypeInt(l)), Object(object.ObjTypeInt(r)) ->
          #(i, Object(object.ObjTypeBool(l < r))) |> Ok
        Object(object.ObjTypeFloat(l)), Object(object.ObjTypeFloat(r)) ->
          #(i, Object(object.ObjTypeBool(l <. r))) |> Ok
        Object(object.ObjTypeString(l)), Object(object.ObjTypeString(r)) -> {
          let cmp = string.compare(l, r)
          #(
            i,
            { cmp == order.Lt }
              |> object.ObjTypeBool
              |> Object,
          )
          |> Ok
        }
        l, r ->
          RuntimeError(error.report_token_as_string(
            e.operator,
            "Invalid Operands(Left: "
              <> l |> object.to_string
              <> ", Right: "
              <> r |> object.to_string,
          ))
          |> Error
      }
    }
    token_type.LessEqual -> {
      case left, right {
        Object(object.ObjTypeInt(l)), Object(object.ObjTypeInt(r)) ->
          #(i, Object(object.ObjTypeBool(l <= r))) |> Ok
        Object(object.ObjTypeFloat(l)), Object(object.ObjTypeFloat(r)) ->
          #(i, Object(object.ObjTypeBool(l <=. r))) |> Ok
        Object(object.ObjTypeString(l)), Object(object.ObjTypeString(r)) -> {
          let cmp = string.compare(l, r)
          #(
            i,
            { cmp == order.Lt || cmp == order.Eq }
              |> object.ObjTypeBool
              |> Object,
          )
          |> Ok
        }
        l, r ->
          RuntimeError(error.report_token_as_string(
            e.operator,
            "Invalid Operands(Left: "
              <> l |> object.to_string
              <> ", Right: "
              <> r |> object.to_string,
          ))
          |> Error
      }
    }
    token_type.BangEqual -> {
      #(i, Object(object.ObjTypeBool(!is_equal(left, right)))) |> Ok
    }
    token_type.EqualEqual -> {
      #(i, Object(object.ObjTypeBool(is_equal(left, right)))) |> Ok
    }
    _ ->
      RuntimeError(error.report_token_as_string(e.operator, "Invalid Operand"))
      |> Error
  }
}

pub fn is_equal(left: Object, right: Object) -> Bool {
  left == right
}

pub fn visit_expression_stmt(
  i: Interpreter,
  stmt: stmt.Expression,
) -> Result(Interpreter, LoxError) {
  case i |> evaluate(stmt.expression) {
    Ok(#(i, _)) -> Ok(i)
    Error(err) -> Error(err)
  }
}

pub fn visit_print_stmt(
  i: Interpreter,
  stmt: stmt.Print,
) -> Result(Interpreter, LoxError) {
  case i |> evaluate(stmt.expression) {
    Ok(#(i, obj)) -> {
      io.println(obj |> object.to_string)
      Ok(i)
    }
    Error(err) -> Error(err)
  }
}

pub fn visit_var_stmt(
  i: Interpreter,
  stmt: stmt.Var,
) -> Result(Interpreter, LoxError) {
  let value: Result(#(Interpreter, Object), LoxError) = case
    stmt.initializer
    != expr.ExprNone
  {
    True -> i |> evaluate(stmt.initializer)
    False -> #(i, Object(object.ObjTypeNil)) |> Ok
  }
  use #(i, value) <- result.try(value)
  i.environment
  |> environment.define(stmt.name.lexeme, value)
  |> Interpreter
  |> Ok
}

pub fn visit_variable_expr(
  i: Interpreter,
  e: expr.Var,
) -> Result(Object, LoxError) {
  i.environment |> environment.get(e.name)
}

pub fn visit_assign_expr(
  i: Interpreter,
  e: expr.Assign,
) -> Result(#(Interpreter, Object), LoxError) {
  use #(i, value): #(Interpreter, Object) <- result.try(i |> evaluate(e.value))
  use env: Environment <- result.try(
    i.environment |> environment.assign(e.name, value),
  )
  Ok(#(Interpreter(env), value))
}

pub fn visit_block_stmt(
  i: Interpreter,
  stmts: stmt.Block,
) -> Result(Interpreter, LoxError) {
  execute_block(stmts.statements, environment.from_env(i.environment))
}

pub fn execute_block(
  stmts: List(Stmt),
  env: Environment,
) -> Result(Interpreter, LoxError) {
  let i = Interpreter(env)
  let res: Result(Interpreter, LoxError) =
    stmts
    |> list.fold_until(Ok(i), fn(acc: Result(Interpreter, LoxError), x: Stmt) {
      case acc {
        Error(err) -> Stop(Error(err))
        Ok(interp) -> Continue(interp |> execute(x))
      }
    })

  use i <- result.try(res)
  let assert Some(previous) = i.environment.enclosing
  Interpreter(previous)
  |> Ok
}

pub fn visit_if_stmt(
  i: Interpreter,
  stmt: stmt.If,
) -> Result(Interpreter, LoxError) {
  use #(i, object) <- result.try(i |> evaluate(stmt.condition))

  case object |> is_truthy, stmt.elseb != stmt.StmtNone {
    True, _ -> i |> execute(stmt.thenb)
    False, True -> i |> execute(stmt.elseb)
    _, _ -> Ok(i)
  }
}

pub fn visit_logical_expr(
  i: Interpreter,
  expr: expr.Logical,
) -> Result(#(Interpreter, Object), LoxError) {
  use #(i, left): #(Interpreter, Object) <- result.try(i |> evaluate(expr.left))
  case expr.operator.ttype == token_type.Or, left |> is_truthy {
    True, True -> #(i, left) |> Ok
    False, False -> #(i, left) |> Ok
    _, _ -> {
      use #(i, right): #(Interpreter, Object) <- result.try(
        i |> evaluate(expr.right),
      )
      #(i, right) |> Ok
    }
  }
}

pub fn visit_while_stmt(
  i: Interpreter,
  stmt: stmt.While,
) -> Result(Interpreter, LoxError) {
  use #(i, obj): #(Interpreter, Object) <- result.try(
    i |> evaluate(stmt.condition),
  )
  case obj |> is_truthy {
    True -> {
      use i <- result.try(i |> execute(stmt.body))
      i |> visit_while_stmt(stmt)
    }
    False -> Ok(i)
  }
}
