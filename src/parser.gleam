import error
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

pub type ParseError {
  ParseError(String)
}

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
) -> Result(List(Stmt), ParseError) {
  case parser |> is_at_end {
    True -> Ok(statements)
    False -> {
      let res: Result(#(Parser, Stmt), ParseError) = statement(parser)
      use <- bool.guard(
        when: res |> result.is_error,
        return: res |> result.unwrap_error(ParseError("parse error")) |> Error,
      )
      let assert Ok(#(parser, stmt)): Result(#(Parser, Stmt), ParseError) = res
      let statements = statements |> list.append([stmt])
      parse_priv(parser, statements)
    }
  }
}

pub fn statement(parser: Parser) -> Result(#(Parser, Stmt), ParseError) {
  let #(parser, m): #(Parser, Bool) = parser |> match([token_type.Print])
  use <- bool.guard(when: m, return: print_statement(parser))
  expression_statement(parser)
}

pub fn print_statement(parser: Parser) -> Result(#(Parser, Stmt), ParseError) {
  let value: Result(#(Parser, Expr), ParseError) = expression(parser)
  use <- bool.guard(
    when: value |> result.is_error,
    return: value
      |> result.unwrap_error(ParseError("expression error"))
      |> Error,
  )
  let assert Ok(#(parser, value)): Result(#(Parser, Expr), ParseError) = value
  let res: Result(#(Parser, Token), ParseError) =
    parser |> consume(token_type.Semicolon, "Expect ';' after value")
  use <- bool.guard(
    when: res |> result.is_error,
    return: Error(ParseError("Expect ';' after value")),
  )
  let assert Ok(#(parser, _)) = res
  #(parser, stmt.StmtPrint(stmt.Print(value))) |> Ok
}

pub fn expression_statement(
  parser: Parser,
) -> Result(#(Parser, Stmt), ParseError) {
  let expr: Result(#(Parser, Expr), ParseError) = expression(parser)
  use <- bool.guard(
    when: expr |> result.is_error,
    return: expr |> result.unwrap_error(ParseError("expression error")) |> Error,
  )
  let assert Ok(#(parser, expr)): Result(#(Parser, Expr), ParseError) = expr
  let res: Result(#(Parser, Token), ParseError) =
    parser |> consume(token_type.Semicolon, "Expect ';' after statement.")
  use <- bool.guard(
    when: res |> result.is_error,
    return: res
      |> result.unwrap_error(ParseError("Expect ';' after statement."))
      |> Error,
  )
  let assert Ok(#(parser, _)): Result(#(Parser, Token), ParseError) = res
  Ok(#(parser, stmt.StmtExpression(stmt.Expression(expr))))
}

pub fn new_parser(tokens: List(Token)) -> Parser {
  Parser(tokens, 0)
}

pub fn expression(parser: Parser) -> Result(#(Parser, Expr), ParseError) {
  parser |> equality
}

pub fn equality(parser: Parser) -> Result(#(Parser, Expr), ParseError) {
  let res: Result(#(Parser, Expr), ParseError) = parser |> comparision
  use <- bool.guard(
    when: res |> result.is_error,
    return: res |> result.unwrap_error(ParseError("Equality error")) |> Error,
  )
  let assert Ok(#(parser, expr)): Result(#(Parser, Expr), ParseError) = res
  parser |> equality_priv(expr)
}

pub fn equality_priv(
  parser: Parser,
  expr: Expr,
) -> Result(#(Parser, Expr), ParseError) {
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
      let assert Ok(#(parser, right)): Result(#(Parser, Expr), ParseError) = res
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

pub fn comparision(parser: Parser) -> Result(#(Parser, Expr), ParseError) {
  let res: Result(#(Parser, Expr), ParseError) = parser |> term
  use <- bool.guard(
    when: res |> result.is_error,
    return: res |> result.unwrap_error(ParseError("camparison error")) |> Error,
  )
  let assert Ok(#(parser, expr)): Result(#(Parser, Expr), ParseError) = res
  parser |> comparision_priv(expr)
}

pub fn comparision_priv(
  parser: Parser,
  expr: Expr,
) -> Result(#(Parser, Expr), ParseError) {
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
      let res: Result(#(Parser, Expr), ParseError) = parser |> term
      use <- bool.guard(
        when: res |> result.is_error,
        return: res
          |> result.unwrap_error(ParseError("comparision erro"))
          |> Error,
      )
      let assert Ok(#(parser, right)): Result(#(Parser, Expr), ParseError) = res
      let expr: Expr = expr.ExprBinary(expr.Binary(expr, operator, right))
      parser |> comparision_priv(expr)
    }
    False -> #(parser, expr) |> Ok
  }
}

pub fn term(parser: Parser) -> Result(#(Parser, Expr), ParseError) {
  let res: Result(#(Parser, Expr), ParseError) = parser |> factor
  use <- bool.guard(
    when: res |> result.is_error,
    return: res |> result.unwrap_error(ParseError("term error")) |> Error,
  )
  let assert Ok(#(parser, expr)): Result(#(Parser, Expr), ParseError) = res
  parser |> term_priv(expr)
}

pub fn term_priv(
  parser: Parser,
  expr: Expr,
) -> Result(#(Parser, Expr), ParseError) {
  let #(parser, m): #(Parser, Bool) =
    parser |> match([token_type.Minus, token_type.Plus])
  case m {
    True -> {
      let operator: Token = parser |> previous()
      let res: Result(#(Parser, Expr), ParseError) = parser |> factor
      use <- bool.guard(
        when: res |> result.is_error,
        return: res |> result.unwrap_error(ParseError("term error")) |> Error,
      )
      let assert Ok(#(parser, right)): Result(#(Parser, Expr), ParseError) = res
      let expr: Expr = expr.ExprBinary(expr.Binary(expr, operator, right))
      parser |> term_priv(expr)
    }
    False -> #(parser, expr) |> Ok
  }
}

pub fn factor(parser: Parser) -> Result(#(Parser, Expr), ParseError) {
  let res: Result(#(Parser, Expr), ParseError) = parser |> unary
  use <- bool.guard(
    when: res |> result.is_error,
    return: res |> result.unwrap_error(ParseError("factor error")) |> Error,
  )
  let assert Ok(#(parser, expr)): Result(#(Parser, Expr), ParseError) = res
  parser |> factor_priv(expr)
}

pub fn factor_priv(
  parser: Parser,
  expr: Expr,
) -> Result(#(Parser, Expr), ParseError) {
  let #(parser, m): #(Parser, Bool) =
    parser |> match([token_type.Slash, token_type.Star])
  case m {
    True -> {
      let operator: Token = parser |> previous
      let res: Result(#(Parser, Expr), ParseError) = parser |> unary
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

pub fn unary(parser: Parser) -> Result(#(Parser, Expr), ParseError) {
  let #(parser, m): #(Parser, Bool) =
    parser |> match([token_type.Bang, token_type.Minus])
  case m {
    True -> {
      let operator: Token = parser |> previous
      let res: Result(#(Parser, Expr), ParseError) = parser |> unary
      use <- bool.guard(when: res |> result.is_error, return: res)
      let assert Ok(#(parser, right)): Result(#(Parser, Expr), ParseError) = res
      #(parser, expr.ExprUnary(expr.Unary(operator, right))) |> Ok
    }
    False -> {
      parser |> primary
    }
  }
}

pub fn primary(parser: Parser) -> Result(#(Parser, Expr), ParseError) {
  let #(parser, m): #(Parser, Bool) = parser |> match([token_type.FFalse])
  use <- bool.guard(
    when: m,
    return: #(
      parser,
      expr.ExprLiteral(expr.Literal(Object(object.ObjTypeBool(False)))),
    )
      |> Ok,
  )
  let #(parser, m): #(Parser, Bool) = parser |> match([token_type.TTrue])
  use <- bool.guard(
    when: m,
    return: #(
      parser,
      expr.ExprLiteral(expr.Literal(Object(object.ObjTypeBool(True)))),
    )
      |> Ok,
  )
  let #(parser, m): #(Parser, Bool) = parser |> match([token_type.Nil])
  use <- bool.guard(
    when: m,
    return: #(parser, expr.ExprLiteral(expr.Literal(Object(object.ObjTypeNil))))
      |> Ok,
  )
  let #(parser, m): #(Parser, Bool) = parser |> match([token_type.String])
  use <- bool.guard(
    when: m,
    return: #(parser, expr.ExprLiteral(expr.Literal(previous(parser).literal)))
      |> Ok,
  )
  let #(parser, m): #(Parser, Bool) = parser |> match([token_type.Integer])
  use <- bool.guard(
    when: m,
    return: #(parser, expr.ExprLiteral(expr.Literal(previous(parser).literal)))
      |> Ok,
  )
  let #(parser, m): #(Parser, Bool) = parser |> match([token_type.FFloat])
  use <- bool.guard(
    when: m,
    return: #(parser, expr.ExprLiteral(expr.Literal(previous(parser).literal)))
      |> Ok,
  )
  let #(parser, m): #(Parser, Bool) = parser |> match([token_type.LeftParen])
  case m {
    True -> {
      let res: Result(#(Parser, Expr), ParseError) = parser |> expression
      use <- bool.guard(
        when: res |> result.is_error,
        return: res
          |> result.unwrap_error(ParseError("expression error"))
          |> Error,
      )
      let assert Ok(#(parser, expr)): Result(#(Parser, Expr), ParseError) = res
      let res: Result(#(Parser, Token), ParseError) =
        parser |> consume(token_type.RightParen, "Expect ')' after expression")
      use <- bool.guard(
        when: res |> result.is_error,
        return: Error(res |> result.unwrap_error(ParseError("Unknown error"))),
      )
      let assert Ok(#(parser, _)): Result(#(Parser, Token), ParseError) = res
      #(parser, expr.ExprGrouping(expr.Grouping(expr))) |> Ok
    }
    False -> {
      error.report_token_as_string(parser |> peek, "Expect expression")
      |> ParseError
      |> Error
    }
  }
}

pub fn consume(
  parser: Parser,
  ttype: TokenType,
  message: String,
) -> Result(#(Parser, Token), ParseError) {
  case parser |> check(ttype) {
    True -> parser |> advance |> Ok
    False -> parser |> peek |> error(message) |> Error
  }
}

pub fn error(token: Token, message: String) -> ParseError {
  let _ = error.report_token(token, message)
  ParseError("")
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
