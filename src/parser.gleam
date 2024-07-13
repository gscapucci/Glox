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
      use #(parser, stmt) <- result.try(parser |> declaration)
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
  use #(parser, name) <- result.try(
    parser |> consume(token_type.Identifier, "Expect variable name"),
  )
  let #(parser, m) = parser |> match([token_type.Equal])

  use #(parser, initializer) <- result.try(case m {
    True -> parser |> expression
    False -> #(parser, expr.ExprNone) |> Ok
  })
  use #(parser, _) <- result.try(
    parser
    |> consume(token_type.Semicolon, "Expect ';' after variable declaration."),
  )
  #(parser, stmt.StmtVar(stmt.Var(name, initializer))) |> Ok
}

pub fn statement(parser: Parser) -> Result(#(Parser, Stmt), LoxError) {
  let #(parser, m): #(Parser, Bool) =
    parser
    |> match([
      token_type.If,
      token_type.Print,
      token_type.While,
      token_type.LeftBrace,
    ])

  case m, previous(parser).ttype {
    True, token_type.If -> if_statement(parser)
    True, token_type.Print -> print_statement(parser)
    True, token_type.While -> while_statement(parser)
    True, token_type.LeftBrace -> {
      use #(parser, stmts) <- result.try(parser |> block)
      #(parser, stmt.StmtBlock(stmt.Block(stmts))) |> Ok
    }
    _, _ -> expression_statement(parser)
  }
}

pub fn while_statement(parser: Parser) -> Result(#(Parser, Stmt), LoxError) {
  use #(parser, _): #(Parser, Token) <- result.try(
    parser |> consume(token_type.LeftParen, "Expect '(' after 'while'."),
  )
  use #(parser, condition): #(Parser, Expr) <- result.try(parser |> expression)
  use #(parser, _): #(Parser, Token) <- result.try(
    parser |> consume(token_type.RightParen, "Expect ')' after condition."),
  )
  use #(parser, body): #(Parser, Stmt) <- result.try(parser |> statement)
  #(parser, stmt.StmtWhile(stmt.While(condition, body))) |> Ok
}

pub fn if_statement(parser: Parser) -> Result(#(Parser, Stmt), LoxError) {
  use #(parser, _) <- result.try(
    parser |> consume(token_type.LeftParen, "Expect '(' after 'if'."),
  )
  use #(parser, condition) <- result.try(parser |> expression)
  use #(parser, _) <- result.try(
    parser |> consume(token_type.RightParen, "Expect ')' after if condition"),
  )
  use #(parser, then_branch) <- result.try(parser |> statement)

  let #(parser, m): #(Parser, Bool) = parser |> match([token_type.Else])
  use #(parser, else_branch) <- result.try(case m {
    True -> parser |> statement
    False -> Ok(#(parser, stmt.StmtNone))
  })
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
      use #(parser, stmt) <- result.try(parser |> declaration)
      block_priv(parser, acc |> list.append([stmt]))
    }
    _, _ -> {
      use #(parser, _) <- result.try(
        parser |> consume(token_type.RightBrace, "Expect '}' after block."),
      )
      Ok(#(parser, acc))
    }
  }
}

pub fn print_statement(parser: Parser) -> Result(#(Parser, Stmt), LoxError) {
  use #(parser, value) <- result.try(expression(parser))
  use #(parser, _) <- result.try(
    parser |> consume(token_type.Semicolon, "Expect ';' after value"),
  )

  #(parser, stmt.StmtPrint(stmt.Print(value))) |> Ok
}

pub fn expression_statement(parser: Parser) -> Result(#(Parser, Stmt), LoxError) {
  use #(parser, expr) <- result.try(expression(parser))
  use #(parser, _) <- result.try(
    parser |> consume(token_type.Semicolon, "Expect ';' after statement."),
  )
  Ok(#(parser, stmt.StmtExpression(stmt.Expression(expr))))
}

pub fn new_parser(tokens: List(Token)) -> Parser {
  Parser(tokens, 0)
}

pub fn expression(parser: Parser) -> Result(#(Parser, Expr), LoxError) {
  parser |> assignment
}

pub fn assignment(parser: Parser) -> Result(#(Parser, Expr), LoxError) {
  use #(parser, expr) <- result.try(parser |> or)
  // use #(parser, expr) <- result.try(parser |> equality)
  let #(parser, m): #(Parser, Bool) = parser |> match([token_type.Equal])
  use <- bool.guard(when: !m, return: #(parser, expr) |> Ok)
  let equals: Token = parser |> previous
  use #(parser, value) <- result.try(parser |> assignment)
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

pub fn or(parser: Parser) -> Result(#(Parser, Expr), LoxError) {
  use #(parser, expr) <- result.try(parser |> and)
  parser |> or_priv(expr)
}

pub fn or_priv(parser: Parser, expr: Expr) -> Result(#(Parser, Expr), LoxError) {
  let #(parser, m): #(Parser, Bool) = parser |> match([token_type.Or])
  case m {
    True -> {
      let operator: Token = parser |> previous
      use #(parser, right) <- result.try(parser |> and)
      #(parser, expr.ExprLogical(expr.Logical(expr, operator, right))) |> Ok
    }
    False -> #(parser, expr) |> Ok
  }
}

pub fn and(parser: Parser) -> Result(#(Parser, Expr), LoxError) {
  use #(parser, expr): #(Parser, Expr) <- result.try(parser |> equality)
  parser |> and_priv(expr)
}

pub fn and_priv(parser: Parser, expr: Expr) -> Result(#(Parser, Expr), LoxError) {
  let #(parser, m): #(Parser, Bool) = parser |> match([token_type.And])
  case m {
    True -> {
      let operator: Token = parser |> previous
      use #(parser, right): #(Parser, Expr) <- result.try(parser |> equality)
      #(parser, expr.ExprLogical(expr.Logical(expr, operator, right))) |> Ok
    }
    False -> #(parser, expr) |> Ok
  }
}

pub fn equality(parser: Parser) -> Result(#(Parser, Expr), LoxError) {
  use #(parser, expr) <- result.try(parser |> comparision)
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
      use #(parser, right) <- result.try(parser |> comparision)
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
  use #(parser, expr) <- result.try(parser |> term)
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
      use #(parser, right) <- result.try(parser |> term)
      let expr: Expr = expr.ExprBinary(expr.Binary(expr, operator, right))
      parser |> comparision_priv(expr)
    }
    False -> #(parser, expr) |> Ok
  }
}

pub fn term(parser: Parser) -> Result(#(Parser, Expr), LoxError) {
  use #(parser, expr) <- result.try(parser |> factor)
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
      use #(parser, right) <- result.try(parser |> factor)
      let expr: Expr = expr.ExprBinary(expr.Binary(expr, operator, right))
      parser |> term_priv(expr)
    }
    False -> #(parser, expr) |> Ok
  }
}

pub fn factor(parser: Parser) -> Result(#(Parser, Expr), LoxError) {
  use #(parser, expr) <- result.try(parser |> unary)
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
      use #(parser, right) <- result.try(parser |> unary)
      let expr: Expr = expr.ExprBinary(expr.Binary(expr, operator, right))
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
      use #(parser, right) <- result.try(parser |> unary)
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
        use #(parser, expr) <- result.try(parser |> expression)
        use #(parser, _) <- result.try(
          parser
          |> consume(token_type.RightParen, "Expect ')' after expression"),
        )

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
