package org.lox

sealed trait KeywordLexeme extends TokenType {
  val lexeme: String
}

sealed trait TokenType {

  //  case object SLASH extends KeywordLexeme {
  //    override val lexeme: String = "/"
  //  }

  // One or two character tokens.
  //  BANG,
  //  BANG_EQUAL,
  //  EQUAL,
  //  EQUAL_EQUAL,
  //  GREATER,
  //  GREATER_EQUAL,
  //  LESS,
  //  LESS_EQUAL,

  // Literals.
  //      IDENTIFIER,
  //    STRING,
  //    NUMBER,
  //
  //  // Keywords.
  //  AND,
  //  CLASS,
  //  ELSE,
  //  FALSE,
  //  FUN,
  //  FOR,
  //  IF,
  //  NIL,
  //  OR,
  //  PRINT,
  //  RETURN,
  //  SUPER,
  //  THIS,
  //  TRUE,
  //  VAR,
  //  WHILE,
  //  EOF
}

case object LeftParen extends KeywordLexeme {
  override val lexeme: String = "("
}

case object RightParen extends KeywordLexeme {
  override val lexeme: String = ")"
}

case object LeftBrace extends KeywordLexeme {
  override val lexeme: String = "{"
}

case object RightBrace extends KeywordLexeme {
  override val lexeme: String = "}"
}

case object Comma extends KeywordLexeme {
  override val lexeme: String = ","
}

case object Dot extends KeywordLexeme {
  override val lexeme: String = "."
}

case object Minus extends KeywordLexeme {
  override val lexeme: String = "-"
}

case object Plus extends KeywordLexeme {
  override val lexeme: String = "+"
}

case object Semicolon extends KeywordLexeme {
  override val lexeme: String = ";"
}

case object Star extends KeywordLexeme {
  override val lexeme: String = "*"
}
