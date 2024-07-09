pub type TokenType {
  LeftParen
  RightParen
  LeftBrace
  RightBrace
  Comma
  Dot
  Minus
  Plus
  Semicolon
  Slash
  Star

  // One or two character tokens.
  Bang
  BangEqual
  Equal
  EqualEqual
  Greater
  GreaterEqual
  Less
  LessEqual

  // Literals.
  Identifier
  String
  Integer
  FFloat

  // Keywords.
  And
  Class
  Else
  FFalse
  Fun
  For
  If
  Nil
  Or
  Print
  Return
  Super
  This
  TTrue
  Var
  While
  Eof
}

pub fn to_string(ttype: TokenType) -> String {
  case ttype {
    LeftParen -> "LeftParen"
    RightParen -> "RightParen"
    LeftBrace -> "LeftBrace"
    RightBrace -> "RightBrace"
    Comma -> "Comma"
    Dot -> "Dot"
    Minus -> "Minus"
    Plus -> "Plus"
    Semicolon -> "Semicolon"
    Slash -> "Slash"
    Star -> "Star"
    Bang -> "Bang"
    BangEqual -> "BangEqual"
    Equal -> "Equal"
    EqualEqual -> "EqualEqual"
    Greater -> "Greater"
    GreaterEqual -> "GreaterEqual"
    Less -> "Less"
    LessEqual -> "LessEqual"
    Identifier -> "Identifier"
    String -> "String"
    Integer -> "Integer"
    FFloat -> "FFloat"
    And -> "And"
    Class -> "Class"
    Else -> "Else"
    FFalse -> "FFalse"
    Fun -> "Fun"
    For -> "For"
    If -> "If"
    Nil -> "NNil"
    Or -> "Or"
    Print -> "Print"
    Return -> "Retrun"
    Super -> "Super"
    This -> "This"
    TTrue -> "TTrue"
    Var -> "Var"
    While -> "While"
    Eof -> "Eof"
  }
}
