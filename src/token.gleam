import object.{type Object}
import token_type.{type TokenType}

pub type Token {
  Token(ttype: TokenType, lexeme: String, literal: Object, line: Int)
}

pub fn to_string(token: Token) -> String {
  token.ttype |> token_type.to_string
  <> " "
  <> token.lexeme
  <> " "
  <> token.literal |> object.to_string
}
