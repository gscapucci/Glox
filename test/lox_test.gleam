import environment.{Environment}
import gleam/dict
import gleam/option.{type Option, None, Some}
import gleam/result
import gleeunit
import gleeunit/should
import interpreter.{Interpreter}
import object.{type Object, Object}
import parser.{type Parser}
import scanner.{type Scanner}
import stmt.{type Stmt}
import token.{type Token, Token}
import token_type

pub fn main() {
  gleeunit.main()
}

pub fn scanner_test() {
  let scan: Scanner = scanner.new_scanner("print var 1 + lala \"lolo\"")
  let scan: Scanner =
    scan
    |> scanner.scan_tokens
    |> result.lazy_unwrap(fn() { panic as "scan_tokens error" })

  scan.tokens
  |> should.equal([
    Token(token_type.Print, "print", Object(object.None), 1),
    Token(token_type.Var, "var", Object(object.None), 1),
    Token(token_type.Integer, "1", Object(object.ObjTypeInt(1)), 1),
    Token(token_type.Plus, "+", Object(object.None), 1),
    Token(token_type.Identifier, "lala", Object(object.None), 1),
    Token(
      token_type.String,
      "\"lolo\"",
      Object(object.ObjTypeString("lolo")),
      1,
    ),
    Token(token_type.Eof, "", Object(object.None), 1),
  ])
}

pub fn peek_test() {
  let scan: Scanner = scanner.new_scanner("1 + 2 * 3;")
  let assert Ok(scan): Result(Scanner, String) = scan |> scanner.scan_tokens
  let par: Parser = parser.new_parser(scan.tokens)

  let p = par |> parser.peek
  p.ttype |> should.equal(token_type.Integer)
  let #(par, tk) = par |> parser.advance
  tk.ttype |> should.equal(token_type.Integer)

  let p = par |> parser.peek
  p.ttype |> should.equal(token_type.Plus)
  let #(par, tk) = par |> parser.advance
  tk.ttype |> should.equal(token_type.Plus)

  let p = par |> parser.peek
  p.ttype |> should.equal(token_type.Integer)
  let #(par, tk) = par |> parser.advance
  tk.ttype |> should.equal(token_type.Integer)

  let p = par |> parser.peek
  p.ttype |> should.equal(token_type.Star)
  let #(par, tk) = par |> parser.advance
  tk.ttype |> should.equal(token_type.Star)

  let p = par |> parser.peek
  p.ttype |> should.equal(token_type.Integer)
  let #(par, tk) = par |> parser.advance
  tk.ttype |> should.equal(token_type.Integer)

  let p = par |> parser.peek
  p.ttype |> should.equal(token_type.Semicolon)
  let #(par, tk) = par |> parser.advance
  tk.ttype |> should.equal(token_type.Semicolon)

  par |> parser.is_at_end |> should.be_true
}

pub fn parser_test() {
  let scan: Scanner = scanner.new_scanner("1 + 2 * 3;")
  let assert Ok(scan): Result(Scanner, String) = scan |> scanner.scan_tokens
  let par: Parser = parser.new_parser(scan.tokens)
  par |> parser.parse |> should.be_some
}

pub fn interpreter_test() {
  let assert Ok(scan): Result(Scanner, String) =
    scanner.new_scanner(
      "print 123;\n
       print true;\n
       print 1 + 1;\n
       print 1.5 * 2.5;\n
       print \"lala\";",
    )
    |> scanner.scan_tokens
  let assert Some(stmts): Option(List(Stmt)) =
    parser.new_parser(scan.tokens) |> parser.parse
  interpreter.interpret(Interpreter(Environment(dict.new(), None)), stmts)
  |> should.be_ok
}
