package org.lox

sealed trait KeywordLexeme extends TokenType {
  val lexeme: String
}

object TokenType {
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

  case object Bang extends KeywordLexeme {
    override val lexeme: String = "!"
  }

  case object BangEqual extends KeywordLexeme {
    override val lexeme: String = "!="
  }

  case object Equal extends KeywordLexeme {
    override val lexeme: String = "="
  }

  case object EqualEqual extends KeywordLexeme {
    override val lexeme: String = "=="
  }

  case object Greater extends KeywordLexeme {
    override val lexeme: String = ">"
  }

  case object GreaterEqual extends KeywordLexeme {
    override val lexeme: String = ">="
  }

  case object Less extends KeywordLexeme {
    override val lexeme: String = "<"
  }

  case object LessEqual extends KeywordLexeme {
    override val lexeme: String = "<="
  }

  case object Slash extends KeywordLexeme {
    override val lexeme: String = "/"
  }

  case object Invalid extends TokenType
}

sealed trait TokenType {
  //  case object SLASH extends KeywordLexeme {
  //    override val lexeme: String = "/"
  //  }

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

