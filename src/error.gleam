import gleam/int
import gleam/io
import token.{type Token}

pub type LoxError {
  RuntimeError(String)
  ParseError(String)
  ScanError(String)
  LoxError(String)
}

pub fn report(line: Int, where: String, message: String) {
  report_as_string(line, where, message)
  |> io.print_error
}

pub fn report_token(tk: Token, message: String) {
  report(tk.line, "", tk |> token.to_string <> ": " <> message)
}

pub fn error(line: Int, message: String) {
  report(line, "", message)
}

pub fn report_as_string(line: Int, where: String, message: String) -> String {
  "[line "
  <> int.to_string(line)
  <> "] Error"
  <> where
  <> ": "
  <> message
  <> "\n"
}

pub fn report_token_as_string(tk: Token, message: String) {
  report_as_string(tk.line, "", tk |> token.to_string <> ": " <> message)
}

pub fn error_as_string(line: Int, message: String) {
  report_as_string(line, "", message)
}
