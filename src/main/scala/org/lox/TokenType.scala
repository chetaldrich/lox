package org.lox

sealed trait ConstantLexeme extends TokenType {
  val lexeme: String
}

object TokenType {
  case object LeftParen extends ConstantLexeme {
    override val lexeme: String = "("
  }

  case object RightParen extends ConstantLexeme {
    override val lexeme: String = ")"
  }

  case object LeftBrace extends ConstantLexeme {
    override val lexeme: String = "{"
  }

  case object RightBrace extends ConstantLexeme {
    override val lexeme: String = "}"
  }

  case object Comma extends ConstantLexeme {
    override val lexeme: String = ","
  }

  case object Dot extends ConstantLexeme {
    override val lexeme: String = "."
  }

  case object Minus extends ConstantLexeme {
    override val lexeme: String = "-"
  }

  case object Plus extends ConstantLexeme {
    override val lexeme: String = "+"
  }

  case object Semicolon extends ConstantLexeme {
    override val lexeme: String = ";"
  }

  case object Star extends ConstantLexeme {
    override val lexeme: String = "*"
  }

  case object Bang extends ConstantLexeme {
    override val lexeme: String = "!"
  }

  case object BangEqual extends ConstantLexeme {
    override val lexeme: String = "!="
  }

  case object Equal extends ConstantLexeme {
    override val lexeme: String = "="
  }

  case object EqualEqual extends ConstantLexeme {
    override val lexeme: String = "=="
  }

  case object Greater extends ConstantLexeme {
    override val lexeme: String = ">"
  }

  case object GreaterEqual extends ConstantLexeme {
    override val lexeme: String = ">="
  }

  case object Less extends ConstantLexeme {
    override val lexeme: String = "<"
  }

  case object LessEqual extends ConstantLexeme {
    override val lexeme: String = "<="
  }

  case object Slash extends ConstantLexeme {
    override val lexeme: String = "/"
  }

  case object String extends TokenType

  case object Number extends TokenType

  case object Identifier extends TokenType

  case object Invalid extends TokenType
}

sealed trait TokenType {
  // Literals.
  // IDENTIFIER,
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

