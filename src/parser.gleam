import error.{type LoxError, ParseError}
import expr.{type Expr}
import gleam/bool
import gleam/io
import gleam/iterator
import gleam/list.{Continue, Stop}
import gleam/option.{type Option, None, Some}
import gleam/result
import object.{Object}
import stmt.{type Stmt}
import token.{type Token, Token}
import token_type.{type TokenType}

pub type Parser {
  Parser(tokens: List(Token), current: Int)
}

pub fn parse(parser: Parser) -> Option(List(Stmt)) {
  case parse_priv(parser, []) {
    Ok(l) -> Some(l)
    Error(r) -> {
      io.debug(r)
      None
    }
  }
}

pub fn parse_priv(
  parser: Parser,
  statements: List(Stmt),
) -> Result(List(Stmt), LoxError) {
  case parser |> is_at_end {
    True -> Ok(statements)
    False -> {
      let res: Result(#(Parser, Stmt), LoxError) = declaration(parser)

      use <- bool.guard(
        when: res |> result.is_error,
        return: res |> result.unwrap_error(ParseError("parse error")) |> Error,
      )
      let assert Ok(#(parser, stmt)): Result(#(Parser, Stmt), LoxError) = res
      let statements = statements |> list.append([stmt])
      parse_priv(parser, statements)
    }
  }
}

pub fn declaration(parser: Parser) -> Result(#(Parser, Stmt), LoxError) {
  let #(parser, m) = parser |> match([token_type.Var])
  let res = case m {
    True -> parser |> var_declaration
    False -> parser |> statement
  }
  case res {
    Error(err) -> {
      io.debug("Error: ")
      io.debug(err)
      let parser = parser |> synchronize
      #(parser, stmt.StmtNone) |> Ok
    }
    Ok(parser_and_stmt) -> parser_and_stmt |> Ok
  }
}

pub fn var_declaration(parser: Parser) -> Result(#(Parser, Stmt), LoxError) {
  let res: Result(#(Parser, Token), LoxError) =
    parser |> consume(token_type.Identifier, "Expect variable name")
  use <- bool.guard(
    when: res |> result.is_error,
    return: res
      |> result.unwrap_error(ParseError("Expect variable name"))
      |> Error,
  )
  let assert Ok(#(parser, name)) = res
  let #(parser, m) = parser |> match([token_type.Equal])
  let res: Result(#(Parser, Expr), LoxError) = case m {
    True -> parser |> expression
    False -> #(parser, expr.ExprNone) |> Ok
  }
  use <- bool.guard(
    when: res |> result.is_error,
    return: res |> result.unwrap_error(ParseError("expression error")) |> Error,
  )
  let assert Ok(#(parser, initializer)) = res
  let res: Result(#(Parser, Token), LoxError) =
    parser
    |> consume(token_type.Semicolon, "Expect ';' after variable declaration.")
  use <- bool.guard(
    when: res |> result.is_error,
    return: res
      |> result.unwrap_error(ParseError(
        "Expect ';' after variable declaration.",
      ))
      |> Error,
  )
  let assert Ok(#(parser, _)) = res
  #(parser, stmt.StmtVar(stmt.Var(name, initializer))) |> Ok
}

pub fn statement(parser: Parser) -> Result(#(Parser, Stmt), LoxError) {
  let #(parser, m): #(Parser, Bool) = parser |> match([token_type.If])
  use <- bool.guard(when: m, return: if_statement(parser))
  let #(parser, m): #(Parser, Bool) = parser |> match([token_type.Print])
  use <- bool.guard(when: m, return: print_statement(parser))
  let #(parser, m): #(Parser, Bool) = parser |> match([token_type.LeftBrace])
  use <- bool.lazy_guard(when: m, return: fn() {
    let res: Result(#(Parser, List(Stmt)), LoxError) = parser |> block
    use <- bool.guard(
      when: res |> result.is_error,
      return: res |> result.unwrap_error(ParseError("Block error")) |> Error,
    )
    let assert Ok(#(parser, stmts)) = res
    #(parser, stmt.StmtBlock(stmt.Block(stmts))) |> Ok
  })
  expression_statement(parser)
}

pub fn if_statement(parser: Parser) -> Result(#(Parser, Stmt), LoxError) {
  let res: Result(#(Parser, Token), LoxError) =
    parser
    |> consume(token_type.LeftParen, "Expect '(' after 'if'.")
  use <- bool.guard(
    when: res |> result.is_error,
    return: res
      |> result.unwrap_error(ParseError("Expect '(' after 'if'."))
      |> Error,
  )
  let assert Ok(#(parser, _)) = res
  let res: Result(#(Parser, Expr), LoxError) = parser |> expression
  use <- bool.guard(
    when: res |> result.is_error,
    return: res |> result.unwrap_error(ParseError("Expression error")) |> Error,
  )
  let assert Ok(#(parser, condition)) = res
  let res: Result(#(Parser, Token), LoxError) =
    parser |> consume(token_type.RightParen, "Expect ')' after if condition")
  use <- bool.guard(
    when: res |> result.is_error,
    return: res
      |> result.unwrap_error(ParseError("Expect ')' after if condition"))
      |> Error,
  )
  let assert Ok(#(parser, _)) = res

  let res: Result(#(Parser, Stmt), LoxError) = parser |> statement
  use <- bool.guard(
    when: res |> result.is_error,
    return: res |> result.unwrap_error(ParseError("Statement Error")) |> Error,
  )
  let assert Ok(#(parser, then_branch)): Result(#(Parser, Stmt), LoxError) = res

  let #(parser, m): #(Parser, Bool) = parser |> match([token_type.Else])
  let res: Result(#(Parser, Stmt), LoxError) = case m {
    True -> parser |> statement
    False -> Ok(#(parser, stmt.StmtNone))
  }
  use <- bool.guard(
    when: res |> result.is_error,
    return: res |> result.unwrap_error(ParseError("Statement error")) |> Error,
  )
  let assert Ok(#(parser, else_branch)) = res
  #(parser, stmt.StmtIf(stmt.If(condition, then_branch, else_branch))) |> Ok
}

pub fn block(parser: Parser) -> Result(#(Parser, List(Stmt)), LoxError) {
  parser |> block_priv([])
}

pub fn block_priv(
  parser: Parser,
  acc: List(Stmt),
) -> Result(#(Parser, List(Stmt)), LoxError) {
  case parser |> check(token_type.RightBrace), parser |> is_at_end {
    False, False -> {
      let res: Result(#(Parser, Stmt), LoxError) = parser |> declaration
      use <- bool.guard(
        when: res |> result.is_error,
        return: res
          |> result.unwrap_error(ParseError("declaration error"))
          |> Error,
      )
      let assert Ok(#(parser, stmt)) = res
      block_priv(parser, acc |> list.append([stmt]))
    }
    _, _ -> {
      let res: Result(#(Parser, Token), LoxError) =
        parser |> consume(token_type.RightBrace, "Expect '}' after block.")
      use <- bool.guard(
        when: res |> result.is_error,
        return: res
          |> result.unwrap_error(ParseError("consume error"))
          |> Error,
      )
      let assert Ok(#(parser, _)) = res
      Ok(#(parser, acc))
    }
  }
}

pub fn print_statement(parser: Parser) -> Result(#(Parser, Stmt), LoxError) {
  let value: Result(#(Parser, Expr), LoxError) = expression(parser)
  use <- bool.guard(
    when: value |> result.is_error,
    return: value
      |> result.unwrap_error(ParseError("expression error"))
      |> Error,
  )
  let assert Ok(#(parser, value)): Result(#(Parser, Expr), LoxError) = value
  let res: Result(#(Parser, Token), LoxError) =
    parser |> consume(token_type.Semicolon, "Expect ';' after value")
  use <- bool.guard(
    when: res |> result.is_error,
    return: Error(ParseError("Expect ';' after value")),
  )
  let assert Ok(#(parser, _)) = res
  #(parser, stmt.StmtPrint(stmt.Print(value))) |> Ok
}

pub fn expression_statement(parser: Parser) -> Result(#(Parser, Stmt), LoxError) {
  let expr: Result(#(Parser, Expr), LoxError) = expression(parser)
  use <- bool.guard(
    when: expr |> result.is_error,
    return: expr |> result.unwrap_error(ParseError("expression error")) |> Error,
  )
  let assert Ok(#(parser, expr)): Result(#(Parser, Expr), LoxError) = expr
  let res: Result(#(Parser, Token), LoxError) =
    parser |> consume(token_type.Semicolon, "Expect ';' after statement.")
  use <- bool.guard(
    when: res |> result.is_error,
    return: res
      |> result.unwrap_error(ParseError("Expect ';' after statement."))
      |> Error,
  )
  let assert Ok(#(parser, _)): Result(#(Parser, Token), LoxError) = res
  Ok(#(parser, stmt.StmtExpression(stmt.Expression(expr))))
}

pub fn new_parser(tokens: List(Token)) -> Parser {
  Parser(tokens, 0)
}

pub fn expression(parser: Parser) -> Result(#(Parser, Expr), LoxError) {
  parser |> assignment
}

pub fn assignment(parser: Parser) -> Result(#(Parser, Expr), LoxError) {
  let res: Result(#(Parser, Expr), LoxError) = parser |> equality
  use <- bool.guard(when: res |> result.is_error, return: res)
  let assert Ok(#(parser, expr)): Result(#(Parser, Expr), LoxError) = res
  let #(parser, m): #(Parser, Bool) = parser |> match([token_type.Equal])
  use <- bool.guard(when: !m, return: #(parser, expr) |> Ok)
  let equals: Token = parser |> previous
  let res = parser |> assignment
  use <- bool.guard(when: res |> result.is_error, return: res)
  let assert Ok(#(parser, value)): Result(#(Parser, Expr), LoxError) = res
  use <- bool.guard(
    when: !{ expr |> expr.is_instance_of(expr.ExprTypeVariable) },
    return: ParseError(error.report_token_as_string(
      equals,
      "Invalid assignment target.",
    ))
      |> Error,
  )
  let assert expr.ExprVariable(expr): Expr = expr
  let name: Token = expr.name
  #(parser, expr.ExprAssign(expr.Assign(name, value))) |> Ok
}

pub fn equality(parser: Parser) -> Result(#(Parser, Expr), LoxError) {
  let res: Result(#(Parser, Expr), LoxError) = parser |> comparision
  use <- bool.guard(
    when: res |> result.is_error,
    return: res |> result.unwrap_error(ParseError("Equality error")) |> Error,
  )
  let assert Ok(#(parser, expr)): Result(#(Parser, Expr), LoxError) = res
  parser |> equality_priv(expr)
}

pub fn equality_priv(
  parser: Parser,
  expr: Expr,
) -> Result(#(Parser, Expr), LoxError) {
  let #(parser, m): #(Parser, Bool) =
    parser |> match([token_type.BangEqual, token_type.EqualEqual])
  case m {
    True -> {
      let operator: Token = parser |> previous
      let res = parser |> comparision
      use <- bool.guard(
        when: res |> result.is_error,
        return: res
          |> result.unwrap_error(ParseError("Equality error"))
          |> Error,
      )
      let assert Ok(#(parser, right)): Result(#(Parser, Expr), LoxError) = res
      let expr: Expr = expr.ExprBinary(expr.Binary(expr, operator, right))
      parser |> equality_priv(expr)
    }
    False -> {
      #(parser, expr) |> Ok
    }
  }
}

pub fn match(parser: Parser, types: List(TokenType)) -> #(Parser, Bool) {
  let ret: Bool =
    types
    |> list.fold_until(False, fn(_acc: Bool, t: TokenType) {
      case parser |> check(t) {
        True -> Stop(True)
        False -> Continue(False)
      }
    })
  case ret {
    True -> {
      let #(parser, _): #(Parser, Token) = parser |> advance
      #(parser, True)
    }
    False -> #(parser, False)
  }
}

pub fn check(parser: Parser, ttype: TokenType) -> Bool {
  case is_at_end(parser), peek(parser).ttype == ttype {
    True, _ -> False
    False, True -> True
    False, False -> False
  }
}

pub fn advance(parser: Parser) -> #(Parser, Token) {
  let parser: Parser = case parser |> is_at_end {
    False -> Parser(..parser, current: parser.current + 1)
    True -> parser
  }
  #(parser, parser |> previous)
}

pub fn is_at_end(parser: Parser) -> Bool {
  peek(parser).ttype == token_type.Eof
}

pub fn peek(parser: Parser) -> Token {
  parser.tokens
  |> iterator.from_list
  |> iterator.at(parser.current)
  |> result.lazy_unwrap(fn() { panic as "Unreachable" })
}

pub fn previous(parser: Parser) -> Token {
  parser.tokens
  |> iterator.from_list
  |> iterator.at(parser.current - 1)
  |> result.lazy_unwrap(fn() { panic as "Unreachable" })
}

pub fn comparision(parser: Parser) -> Result(#(Parser, Expr), LoxError) {
  let res: Result(#(Parser, Expr), LoxError) = parser |> term
  use <- bool.guard(
    when: res |> result.is_error,
    return: res |> result.unwrap_error(ParseError("camparison error")) |> Error,
  )
  let assert Ok(#(parser, expr)): Result(#(Parser, Expr), LoxError) = res
  parser |> comparision_priv(expr)
}

pub fn comparision_priv(
  parser: Parser,
  expr: Expr,
) -> Result(#(Parser, Expr), LoxError) {
  let #(parser, m): #(Parser, Bool) =
    parser
    |> match([
      token_type.Greater,
      token_type.GreaterEqual,
      token_type.Less,
      token_type.LessEqual,
    ])
  case m {
    True -> {
      let operator: Token = parser |> previous
      let res: Result(#(Parser, Expr), LoxError) = parser |> term
      use <- bool.guard(
        when: res |> result.is_error,
        return: res
          |> result.unwrap_error(ParseError("comparision erro"))
          |> Error,
      )
      let assert Ok(#(parser, right)): Result(#(Parser, Expr), LoxError) = res
      let expr: Expr = expr.ExprBinary(expr.Binary(expr, operator, right))
      parser |> comparision_priv(expr)
    }
    False -> #(parser, expr) |> Ok
  }
}

pub fn term(parser: Parser) -> Result(#(Parser, Expr), LoxError) {
  let res: Result(#(Parser, Expr), LoxError) = parser |> factor
  use <- bool.guard(
    when: res |> result.is_error,
    return: res |> result.unwrap_error(ParseError("term error")) |> Error,
  )
  let assert Ok(#(parser, expr)): Result(#(Parser, Expr), LoxError) = res
  parser |> term_priv(expr)
}

pub fn term_priv(
  parser: Parser,
  expr: Expr,
) -> Result(#(Parser, Expr), LoxError) {
  let #(parser, m): #(Parser, Bool) =
    parser |> match([token_type.Minus, token_type.Plus])
  case m {
    True -> {
      let operator: Token = parser |> previous()
      let res: Result(#(Parser, Expr), LoxError) = parser |> factor
      use <- bool.guard(
        when: res |> result.is_error,
        return: res |> result.unwrap_error(ParseError("term error")) |> Error,
      )
      let assert Ok(#(parser, right)): Result(#(Parser, Expr), LoxError) = res
      let expr: Expr = expr.ExprBinary(expr.Binary(expr, operator, right))
      parser |> term_priv(expr)
    }
    False -> #(parser, expr) |> Ok
  }
}

pub fn factor(parser: Parser) -> Result(#(Parser, Expr), LoxError) {
  let res: Result(#(Parser, Expr), LoxError) = parser |> unary
  use <- bool.guard(
    when: res |> result.is_error,
    return: res |> result.unwrap_error(ParseError("factor error")) |> Error,
  )
  let assert Ok(#(parser, expr)): Result(#(Parser, Expr), LoxError) = res
  parser |> factor_priv(expr)
}

pub fn factor_priv(
  parser: Parser,
  expr: Expr,
) -> Result(#(Parser, Expr), LoxError) {
  let #(parser, m): #(Parser, Bool) =
    parser |> match([token_type.Slash, token_type.Star])
  case m {
    True -> {
      let operator: Token = parser |> previous
      let res: Result(#(Parser, Expr), LoxError) = parser |> unary
      use <- bool.guard(
        when: res |> result.is_error,
        return: res |> result.unwrap_error(ParseError("factor error")) |> Error,
      )
      let assert Ok(#(parser, right)) = res
      // io.println(right |> expr.to_string)
      // io.println(expr |> expr.to_string)
      let expr: Expr = expr.ExprBinary(expr.Binary(expr, operator, right))
      // io.println(expr |> expr.to_string)
      // io.println("")
      parser |> factor_priv(expr)
    }
    False -> #(parser, expr) |> Ok
  }
}

pub fn unary(parser: Parser) -> Result(#(Parser, Expr), LoxError) {
  let #(parser, m): #(Parser, Bool) =
    parser |> match([token_type.Bang, token_type.Minus])
  case m {
    True -> {
      let operator: Token = parser |> previous
      let res: Result(#(Parser, Expr), LoxError) = parser |> unary
      use <- bool.guard(when: res |> result.is_error, return: res)
      let assert Ok(#(parser, right)): Result(#(Parser, Expr), LoxError) = res
      #(parser, expr.ExprUnary(expr.Unary(operator, right))) |> Ok
    }
    False -> {
      parser |> primary
    }
  }
}

pub fn primary(parser: Parser) -> Result(#(Parser, Expr), LoxError) {
  let #(parser, m): #(Parser, Bool) =
    parser
    |> match([
      token_type.FFalse,
      token_type.TTrue,
      token_type.Nil,
      token_type.Identifier,
      token_type.String,
      token_type.Integer,
      token_type.FFloat,
      token_type.LeftParen,
    ])
  use <- bool.lazy_guard(when: m, return: fn() {
    case previous(parser).ttype {
      token_type.FFalse ->
        #(
          parser,
          expr.ExprLiteral(expr.Literal(Object(object.ObjTypeBool(False)))),
        )
        |> Ok
      token_type.TTrue ->
        #(
          parser,
          expr.ExprLiteral(expr.Literal(Object(object.ObjTypeBool(True)))),
        )
        |> Ok
      token_type.Nil ->
        #(parser, expr.ExprLiteral(expr.Literal(Object(object.ObjTypeNil))))
        |> Ok
      token_type.Identifier ->
        #(parser, expr.ExprVariable(expr.Var(parser |> previous))) |> Ok
      token_type.String ->
        #(parser, expr.ExprLiteral(expr.Literal(previous(parser).literal)))
        |> Ok
      token_type.Integer ->
        #(parser, expr.ExprLiteral(expr.Literal(previous(parser).literal)))
        |> Ok
      token_type.FFloat ->
        #(parser, expr.ExprLiteral(expr.Literal(previous(parser).literal)))
        |> Ok
      token_type.LeftParen -> {
        let res: Result(#(Parser, Expr), LoxError) = parser |> expression
        use <- bool.guard(
          when: res |> result.is_error,
          return: res
            |> result.unwrap_error(ParseError("expression error"))
            |> Error,
        )
        let assert Ok(#(parser, expr)): Result(#(Parser, Expr), LoxError) = res
        let res: Result(#(Parser, Token), LoxError) =
          parser
          |> consume(token_type.RightParen, "Expect ')' after expression")
        use <- bool.guard(
          when: res |> result.is_error,
          return: Error(res |> result.unwrap_error(ParseError("Unknown error"))),
        )
        let assert Ok(#(parser, _)): Result(#(Parser, Token), LoxError) = res
        #(parser, expr.ExprGrouping(expr.Grouping(expr))) |> Ok
      }
      _ -> {
        panic as "Unreachable"
      }
    }
  })
  error.report_token_as_string(parser |> peek, "Expect expression")
  |> ParseError
  |> Error
}

pub fn consume(
  parser: Parser,
  ttype: TokenType,
  message: String,
) -> Result(#(Parser, Token), LoxError) {
  case parser |> check(ttype) {
    True -> parser |> advance |> Ok
    False -> parser |> peek |> error(message) |> Error
  }
}

pub fn error(token: Token, message: String) -> LoxError {
  error.report_token_as_string(token, message)
  |> ParseError
}

pub fn synchronize(parser: Parser) -> Parser {
  let #(parser, _) = parser |> advance
  parser |> synchronize_priv
}

pub fn synchronize_priv(parser: Parser) -> Parser {
  case parser |> is_at_end |> bool.negate {
    True -> {
      use <- bool.guard(
        when: previous(parser).ttype == token_type.Semicolon,
        return: parser,
      )
      let ret = case peek(parser).ttype {
        token_type.Class
        | token_type.Fun
        | token_type.Var
        | token_type.For
        | token_type.If
        | token_type.While
        | token_type.Print
        | token_type.Return -> True
        _ -> False
      }
      use <- bool.guard(when: ret, return: parser)
      let #(parser, _) = parser |> advance
      parser |> synchronize_priv
    }
    False -> parser
  }
}
