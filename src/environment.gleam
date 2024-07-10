import error.{type LoxError, RuntimeError}
import gleam/bool
import gleam/dict.{type Dict}
import gleam/result
import object.{type Object}
import token.{type Token}

pub type Environment {
  Environment(values: Dict(String, Object))
}

pub fn define(env: Environment, name: String, value: Object) -> Environment {
  let d = env.values |> dict.insert(name, value)
  Environment(values: d)
}

pub fn get(env: Environment, name: Token) -> Result(Object, LoxError) {
  use <- bool.guard(
    when: env.values |> dict.has_key(name.lexeme),
    return: env.values
      |> dict.get(name.lexeme)
      |> result.lazy_unwrap(fn() { panic as "Unreachable" })
      |> Ok,
  )
  Error(
    RuntimeError(error.report_token_as_string(
      name,
      "Undefined variable '" <> name.lexeme <> "'.",
    )),
  )
}
