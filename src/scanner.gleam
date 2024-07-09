import common
import error
import gleam/bool
import gleam/dict.{type Dict}
import gleam/erlang
import gleam/float
import gleam/int
import gleam/iterator
import gleam/list
import gleam/result
import gleam/string
import object.{type Object, Object}
import token.{type Token, Token}
import token_type.{type TokenType}

pub type Scanner {
  Scanner(
    source: String,
    tokens: List(Token),
    start: Int,
    current: Int,
    line: Int,
    keywords: Dict(String, TokenType),
  )
}

pub fn init_keywords(
  items: List(#(String, TokenType)),
) -> Dict(String, TokenType) {
  dict.from_list(items)
}

pub fn new_scanner(source: String) -> Scanner {
  Scanner(
    source,
    [],
    0,
    0,
    1,
    init_keywords([
      #("and", token_type.And),
      #("class", token_type.Class),
      #("else", token_type.Else),
      #("false", token_type.FFalse),
      #("for", token_type.For),
      #("fun", token_type.Fun),
      #("if", token_type.If),
      #("nil", token_type.Nil),
      #("or", token_type.Or),
      #("print", token_type.Print),
      #("return", token_type.Return),
      #("super", token_type.Super),
      #("this", token_type.This),
      #("true", token_type.TTrue),
      #("var", token_type.Var),
      #("while", token_type.While),
    ]),
  )
}

pub fn scan_tokens(scan: Scanner) -> Result(Scanner, String) {
  case scan |> is_at_end {
    True -> {
      let ttokens =
        scan.tokens
        |> list.append([
          Token(token_type.Eof, "", Object(object.None), scan.line),
        ])
      Ok(Scanner(..scan, tokens: ttokens))
    }
    False -> {
      let scan = Scanner(..scan, start: scan.current)
      let scan: Result(Scanner, String) = scan_token(scan)
      use <- bool.guard(
        when: scan |> result.is_error,
        return: Error(scan |> result.unwrap_error("Scan token error")),
      )
      scan_tokens(scan |> result.lazy_unwrap(fn() { panic as "Unreachable" }))
    }
  }
}

pub fn is_at_end(scan: Scanner) -> Bool {
  scan.current >= string.length(scan.source)
}

pub fn scan_token(scan: Scanner) -> Result(Scanner, String) {
  let #(scan, c): #(Scanner, String) = advance(scan)
  case c {
    "(" -> add_token(scan, token_type.LeftParen) |> Ok
    ")" -> add_token(scan, token_type.RightParen) |> Ok
    "{" -> add_token(scan, token_type.LeftBrace) |> Ok
    "}" -> add_token(scan, token_type.RightBrace) |> Ok
    "," -> add_token(scan, token_type.Comma) |> Ok
    "." -> add_token(scan, token_type.Dot) |> Ok
    "-" -> add_token(scan, token_type.Minus) |> Ok
    "+" -> add_token(scan, token_type.Plus) |> Ok
    ";" -> add_token(scan, token_type.Semicolon) |> Ok
    "*" -> add_token(scan, token_type.Star) |> Ok
    "!" -> {
      let #(scan, is_match) = match(scan, "=")
      add_token(
        scan,
        bool.guard(
          when: is_match,
          return: token_type.BangEqual,
          otherwise: fn() { token_type.Bang },
        ),
      )
      |> Ok
    }
    "=" -> {
      let #(scan, is_match) = match(scan, "=")
      add_token(
        scan,
        bool.guard(
          when: is_match,
          return: token_type.EqualEqual,
          otherwise: fn() { token_type.Equal },
        ),
      )
      |> Ok
    }
    "<" -> {
      let #(scan, is_match) = match(scan, "=")
      add_token(
        scan,
        bool.guard(
          when: is_match,
          return: token_type.LessEqual,
          otherwise: fn() { token_type.Less },
        ),
      )
      |> Ok
    }
    ">" -> {
      let #(scan, is_match) = match(scan, "=")
      add_token(
        scan,
        bool.guard(
          when: is_match,
          return: token_type.GreaterEqual,
          otherwise: fn() { token_type.Greater },
        ),
      )
      |> Ok
    }
    "/" -> {
      let #(scan, is_match) = match(scan, "/")
      bool.guard(when: is_match, return: ignore_comment(scan), otherwise: fn() {
        add_token(scan, token_type.Slash)
      })
      |> Ok
    }
    " " | "\r" | "\t" -> {
      Ok(scan)
    }
    "\n" -> {
      Ok(Scanner(..scan, line: scan.line + 1))
    }
    "\"" -> {
      Ok(sstring(scan))
    }
    _ -> {
      use <- bool.guard(when: is_digit(c), return: number(scan))
      use <- bool.guard(when: is_alpha(c), return: identifier(scan))
      Error("Unkown token '" <> c <> "'")
    }
  }
}

pub fn is_alpha(c: String) -> Bool {
  let to_ascii: fn(String) -> Int = fn(x) {
    case erlang.term_to_binary(x) {
      <<_, _, _, _, _, _, ascii>> -> ascii
      _ -> panic as "Unreachable"
    }
  }

  let i = to_ascii(c)
  let a = to_ascii("a")
  let z = to_ascii("z")
  let a2 = to_ascii("A")
  let z2 = to_ascii("Z")

  { i >= a && i <= z } || { i >= a2 && i <= z2 } || c == "_"
}

pub fn identifier(scan: Scanner) -> Result(Scanner, String) {
  case is_alphanumeric(peek(scan)) {
    True -> {
      let #(scan, _) = advance(scan)
      identifier(scan)
    }
    False -> {
      let text = common.sub_string(scan.source, scan.start, scan.current)
      let ttype = scan.keywords |> dict.get(text)
      use <- bool.guard(
        when: ttype |> result.is_error,
        return: Ok(add_token(scan, token_type.Identifier)),
      )
      let assert Ok(ttype) = ttype
      Ok(add_token(scan, ttype))
    }
  }
}

pub fn is_alphanumeric(c: String) -> Bool {
  is_alpha(c) || is_digit(c)
}

pub fn number(scan: Scanner) -> Result(Scanner, String) {
  number_priv(scan, False)
}

pub fn number_priv(scan: Scanner, found_dot: Bool) -> Result(Scanner, String) {
  case is_digit(peek(scan)) {
    True -> {
      let #(scan, _) = advance(scan)
      number_priv(scan, False)
    }
    False -> {
      use <- bool.guard(
        when: found_dot,
        return: Error("Scan number error line: " <> int.to_string(scan.line)),
      )
      case peek(scan) == "." && is_digit(peek_next(scan)) {
        True -> {
          let #(scan, _) = advance(scan)
          number_priv(scan, True)
        }
        False -> {
          let text = common.sub_string(scan.source, scan.start, scan.current)
          let num = float.parse(text)
          use <- bool.lazy_guard(when: num |> result.is_error, return: fn() {
            let num = int.parse(text)
            use <- bool.guard(
              when: num |> result.is_error,
              return: Error("parse error"),
            )
            let scan =
              add_token_obj(
                scan,
                token_type.Integer,
                Object(object.ObjTypeInt(
                  num
                  |> result.lazy_unwrap(fn() { panic as "Unreachable" }),
                )),
              )
            Ok(scan)
          })
          let scan =
            add_token_obj(
              scan,
              token_type.FFloat,
              Object(
                object.ObjTypeFloat(
                  result.lazy_unwrap(num, fn() { panic as "Unreachable" }),
                ),
              ),
            )
          Ok(scan)
        }
      }
    }
  }
}

pub fn peek_next(scan: Scanner) -> String {
  case scan.current + 1 >= string.length(scan.source) {
    True -> ""
    False -> {
      let c =
        scan.source
        |> string.to_graphemes
        |> iterator.from_list
        |> iterator.at(scan.current + 1)
      use <- bool.lazy_guard(when: c |> result.is_error, return: fn() {
        panic as "Unreachable"
      })
      c |> result.lazy_unwrap(fn() { panic as "Unreachable" })
    }
  }
}

pub fn is_digit(c: String) -> Bool {
  case c {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    _ -> False
  }
}

pub fn advance(scan: Scanner) -> #(Scanner, String) {
  let scan: Scanner = Scanner(..scan, current: scan.current + 1)
  let str: Result(String, Nil) =
    scan.source
    |> string.to_graphemes
    |> iterator.from_list
    |> iterator.at(scan.current - 1)
  use <- bool.lazy_guard(when: str |> result.is_error, return: fn() {
    panic as {
      "Iterator error on function advance, source_len: "
      <> string.length(scan.source) |> int.to_string
      <> " scan.current: "
      <> scan.current |> int.to_string
    }
  })
  #(scan, str |> result.lazy_unwrap(fn() { panic as "Unreachable" }))
}

pub fn add_token(scan: Scanner, ttype: TokenType) -> Scanner {
  let lexe = common.sub_string(scan.source, scan.start, scan.current)
  Scanner(
    ..scan,
    tokens: scan.tokens
      |> list.append([Token(ttype, lexe, Object(object.None), scan.line)]),
  )
}

pub fn add_token_obj(scan: Scanner, ttype: TokenType, obj: Object) -> Scanner {
  let lexe = common.sub_string(scan.source, scan.start, scan.current)
  Scanner(
    ..scan,
    tokens: scan.tokens |> list.append([Token(ttype, lexe, obj, scan.line)]),
  )
}

pub fn match(scan: Scanner, expected: String) -> #(Scanner, Bool) {
  use <- bool.guard(is_at_end(scan), #(scan, False))
  let c =
    scan.source
    |> string.to_graphemes
    |> iterator.from_list
    |> iterator.at(scan.current)
    |> result.lazy_unwrap(fn() { panic as "Iterator error on function match" })
  use <- bool.guard(c != expected, #(scan, False))
  let scan = Scanner(..scan, current: scan.current + 1)
  #(scan, True)
}

pub fn ignore_comment(scan: Scanner) -> Scanner {
  case { peek(scan) != "\n" }, !is_at_end(scan) {
    True, True -> {
      let #(scan, _) = advance(scan)
      ignore_comment(scan)
    }
    _, _ -> scan
  }
}

pub fn peek(scan: Scanner) -> String {
  case is_at_end(scan) {
    True -> ""
    False -> {
      scan.source
      |> string.to_graphemes
      |> iterator.from_list
      |> iterator.at(scan.current)
      |> result.lazy_unwrap(fn() { panic as "Iterator error on function peek" })
    }
  }
}

pub fn sstring(scan: Scanner) -> Scanner {
  case { peek(scan) != "\"" }, { !is_at_end(scan) } {
    True, True -> {
      let scan = case peek(scan) == "\n" {
        True -> Scanner(..scan, line: scan.line + 1)
        False -> scan
      }
      let #(scan, _) = advance(scan)
      sstring(scan)
    }
    _, _ -> {
      case is_at_end(scan) {
        True -> {
          error.error(scan.line, "Unterminated string")
          scan
        }
        False -> {
          let #(scan, _) = advance(scan)
          let value =
            common.sub_string(scan.source, scan.start + 1, scan.current - 1)
          add_token_obj(
            scan,
            token_type.String,
            Object(object.ObjTypeString(value)),
          )
        }
      }
    }
  }
}
