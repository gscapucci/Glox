import argv
import error
import expr.{type Expr}
import gleam/bool
import gleam/erlang
import gleam/io
import gleam/iterator.{type Iterator}
import gleam/list
import gleam/option.{type Option, Some}
import gleam/result
import interpreter.{type Interpreter, Interpreter}
import parser.{type Parser}
import scanner.{type Scanner, scan_tokens}
import simplifile
import stmt.{type Stmt}

pub type Lox {
  Lox(had_error: Bool, had_runtime_error: Bool, interp: Interpreter)
}

pub fn run(lox: Lox, source: String) -> Result(Nil, String) {
  let scan: Result(Scanner, String) = scanner.new_scanner(source) |> scan_tokens
  use <- bool.guard(
    when: scan |> result.is_error,
    return: Error(scan |> result.unwrap_error("Unknown Error")),
  )
  let assert Ok(scan): Result(Scanner, String) = scan
  let par: Parser = parser.new_parser(scan.tokens)
  let parsed: Option(List(Stmt)) = par |> parser.parse
  use <- bool.guard(
    when: parsed |> option.is_none,
    return: Error("Parse Error"),
  )
  let assert Some(stmts): Option(List(Stmt)) = parsed
  lox.interp |> interpreter.interpret(stmts)
}

pub fn run_file(lox: Lox, file: String) -> Result(Nil, String) {
  let file: String =
    simplifile.read(file)
    |> result.lazy_unwrap(fn() { panic as "Could not open the file" })
  let res: Result(Nil, String) = lox |> run(file)
  use <- bool.guard(when: result.is_ok(res), return: Ok(Nil))
  Error(result.unwrap_error(res, "Unknown Error"))
}

pub fn run_prompt(lox: Lox) -> Result(Nil, String) {
  case erlang.get_line(">") {
    Error(_) -> {
      Error("Forced exit\n")
    }
    Ok("EXIT" <> _) -> {
      Ok(Nil)
    }
    Ok(source) -> {
      let res = lox |> run(source)
      case res {
        Error(err) -> {
          error.report(0, "run_prompt", "run source error: '" <> err <> "'")
          let lox = Lox(..lox, had_error: False)
          run_prompt(lox)
        }
        Ok(_) -> run_prompt(lox)
      }
    }
  }
}

pub fn lox_main(lox: Lox) -> Result(Nil, String) {
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
      let option: String =
        args_it
        |> iterator.at(0)
        |> result.lazy_unwrap(fn() { panic as "Unreachable" })
      case option {
        "tokens" -> {
          let source: String =
            args_it
            |> iterator.at(1)
            |> result.lazy_unwrap(fn() { panic as "Unreachable" })
          let scan_result: Result(Scanner, String) =
            scanner.new_scanner(source) |> scan_tokens
          case scan_result {
            Ok(scanner) -> {
              scanner.tokens |> list.map(io.debug)
              Ok(Nil)
            }
            Error(err) -> Error(err)
          }
        }
        _ -> Error("Usage: jlox <options> [script]")
      }
    }
    _ -> Error("Usage: jlox <options> [script]")
  }
}

pub fn main() {
  case lox_main(Lox(False, False, Interpreter)) {
    Error(err) -> error.error(0, err)
    _ -> Nil
  }
}
