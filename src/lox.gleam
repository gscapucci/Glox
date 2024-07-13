import argv
import environment.{Environment}
import error.{type LoxError, LoxError, ParseError}
import gleam/bool
import gleam/dict
import gleam/erlang
import gleam/io
import gleam/iterator.{type Iterator}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import interpreter.{type Interpreter, Interpreter}
import parser.{type Parser}
import scanner
import simplifile
import stmt.{type Stmt}

pub type Lox {
  Lox(had_error: Bool, had_runtime_error: Bool, interp: Interpreter)
}

pub fn run(lox: Lox, source: String) -> Result(Lox, LoxError) {
  use scan <- result.try(scanner.new_scanner(source) |> scanner.scan_tokens)
  let par: Parser = parser.new_parser(scan.tokens)
  let parsed: Option(List(Stmt)) = par |> parser.parse
  use <- bool.guard(
    when: parsed |> option.is_none,
    return: ParseError("Parse Error") |> Error,
  )
  let assert Some(stmts): Option(List(Stmt)) = parsed
  case lox.interp |> interpreter.interpret(stmts) {
    Ok(i) -> Ok(Lox(..lox, interp: i))
    Error(err) -> Error(err)
  }
}

pub fn run_file(lox: Lox, file: String) -> Result(Lox, LoxError) {
  let file: String =
    simplifile.read(file)
    |> result.lazy_unwrap(fn() { panic as "Could not open the file" })
  lox |> run(file)
}

pub fn run_prompt(lox: Lox) -> Result(Lox, LoxError) {
  case erlang.get_line(">") {
    Error(_) -> LoxError("Forced exit\n") |> Error
    Ok("EXIT" <> _) -> Ok(lox)
    Ok(source) -> {
      let res = lox |> run(source)
      case res {
        Error(err) -> {
          io.debug(err)
          let lox = Lox(..lox, had_error: False)
          run_prompt(lox)
        }
        Ok(_) -> run_prompt(lox)
      }
    }
  }
}

pub fn lox_main(lox: Lox) -> Result(Lox, LoxError) {
  case list.length(argv.load().arguments) {
    0 -> run_prompt(lox)
    1 -> {
      lox
      |> run_file(
        iterator.from_list(argv.load().arguments)
        |> iterator.at(0)
        |> result.lazy_unwrap(fn() { panic }),
      )
    }
    2 -> {
      let args_it: Iterator(String) = iterator.from_list(argv.load().arguments)
      let assert Ok(option) =
        args_it
        |> iterator.at(0)
      case option {
        "tokens" -> {
          let assert Ok(source) =
            args_it
            |> iterator.at(1)
          use scan <- result.try(
            scanner.new_scanner(source) |> scanner.scan_tokens,
          )
          scan.tokens |> list.map(io.debug)
          Ok(lox)
        }
        _ -> LoxError("Usage: jlox <options> [script]") |> Error
      }
    }
    _ -> LoxError("Usage: jlox <options> [script]") |> Error
  }
}

pub fn main() {
  case lox_main(Lox(False, False, Interpreter(Environment(dict.new(), None)))) {
    Error(err) -> {
      io.debug(err)
      Nil
    }
    _ -> Nil
  }
}
