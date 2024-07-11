import error.{type LoxError, RuntimeError}
import gleam/bool
import gleam/dict.{type Dict}
import gleam/option.{type Option, None, Some}
import gleam/result
import object.{type Object}
import token.{type Token}

pub type Environment {
  Environment(values: Dict(String, Object), enclosing: Option(Environment))
}

pub fn new() -> Environment {
  Environment(dict.new(), None)
}

pub fn from_env(enclosing: Environment) -> Environment {
  Environment(dict.new(), Some(enclosing))
}

pub fn define(env: Environment, name: String, value: Object) -> Environment {
  let d = env.values |> dict.insert(name, value)
  Environment(values: d, enclosing: env.enclosing)
}

pub fn get(env: Environment, name: Token) -> Result(Object, LoxError) {
  case env.values |> dict.has_key(name.lexeme) {
    True ->
      env.values
      |> dict.get(name.lexeme)
      |> result.lazy_unwrap(fn() {
        panic as { "Could not get " <> name.lexeme }
      })
      |> Ok
    False -> {
      use <- bool.guard(
        when: env.enclosing |> option.is_some,
        return: env.enclosing
          |> option.lazy_unwrap(fn() { panic as "Unreachable" })
          |> get(name),
      )
      Error(
        RuntimeError(error.report_token_as_string(
          name,
          "Undefined variable '" <> name.lexeme <> "'.",
        )),
      )
    }
  }
}

pub fn assign(
  env: Environment,
  name: Token,
  value: Object,
) -> Result(Environment, LoxError) {
  use <- bool.lazy_guard(
    when: env.values |> dict.has_key(name.lexeme),
    return: fn() {
      let d: Dict(String, Object) =
        env.values |> dict.insert(name.lexeme, value)
      let env = Environment(values: d, enclosing: env.enclosing)
      Ok(env)
    },
  )
  use <- bool.lazy_guard(when: env.enclosing |> option.is_some, return: fn() {
    let enc: Result(Environment, LoxError) =
      env.enclosing
      |> option.lazy_unwrap(fn() { panic as "Unreachable" })
      |> assign(name, value)
    use <- bool.guard(
      when: enc |> result.is_error,
      return: enc |> result.unwrap_error(RuntimeError("Assign error")) |> Error,
    )
    let assert Ok(enc): Result(Environment, LoxError) = enc
    Environment(env.values, Some(enc)) |> Ok
  })
  RuntimeError(error.report_token_as_string(
    name,
    "Undefined variable '" <> name.lexeme <> "'.",
  ))
  |> Error
}
