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

pub fn assign(
  env: Environment,
  name: Token,
  obj: Object,
) -> Result(Environment, LoxError) {
  case env.values |> dict.has_key(name.lexeme) {
    True -> {
      let d: Dict(String, Object) = env.values |> dict.insert(name.lexeme, obj)
      let env = Environment(values: d)
      Ok(env)
    }
    False -> {
      RuntimeError(error.report_token_as_string(
        name,
        "Undefined variable '" <> name.lexeme <> "'.",
      ))
      |> Error
    }
  }
}
