package org.lox.lexer

import scala.util.matching.Regex

sealed trait ConstantLexeme extends TokenType {
  val lexeme: String
}

sealed trait RegexLexeme extends TokenType {
  val lexeme: Regex
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

  case object QuestionMark extends ConstantLexeme {
    override val lexeme: String = "?"
  }

  case object Colon extends ConstantLexeme {
    override val lexeme: String = ":"
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

  case object And extends RegexLexeme {
    override val lexeme: Regex = "and\\b".r
  }

  case object Break extends RegexLexeme {
    override val lexeme: Regex = "break\\b".r
  }

  case object Class extends RegexLexeme {
    override val lexeme: Regex = "class\\b".r
  }

  case object Else extends RegexLexeme {
    override val lexeme: Regex = "else\\b".r
  }

  case object False extends RegexLexeme {
    override val lexeme: Regex = "false\\b".r
  }

  case object Fun extends RegexLexeme {
    override val lexeme: Regex = "fun\\b".r
  }

  case object For extends RegexLexeme {
    override val lexeme: Regex = "for\\b".r
  }

  case object If extends RegexLexeme {
    override val lexeme: Regex = "if\\b".r
  }

  case object Nil extends RegexLexeme {
    override val lexeme: Regex = "nil\\b".r
  }

  case object Or extends RegexLexeme {
    override val lexeme: Regex = "or\\b".r
  }

  case object Print extends RegexLexeme {
    override val lexeme: Regex = "print\\b".r
  }

  case object Return extends RegexLexeme {
    override val lexeme: Regex = "return\\b".r
  }

  case object Super extends RegexLexeme {
    override val lexeme: Regex = "super\\b".r
  }

  case object This extends RegexLexeme {
    override val lexeme: Regex = "this\\b".r
  }

  case object True extends RegexLexeme {
    override val lexeme: Regex = "true\\b".r
  }

  case object Var extends RegexLexeme {
    override val lexeme: Regex = "var\\b".r
  }

  case object While extends RegexLexeme {
    override val lexeme: Regex = "while\\b".r
  }

  case object EOF extends TokenType
}

sealed trait TokenType

