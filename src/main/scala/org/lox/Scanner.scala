package org.lox

import scala.util.matching.Regex
import scala.util.parsing.combinator._

object Scanner extends RegexParsers {
  override val whiteSpace: Regex = """(\s+|//.*)+""".r

  // A list of all the keywords in the language.
  private val constantLexemes = List(
    TokenType.LeftParen,
    TokenType.RightParen,
    TokenType.LeftBrace,
    TokenType.RightBrace,
    TokenType.Comma,
    TokenType.Dot,
    TokenType.Minus,
    TokenType.Plus,
    TokenType.Semicolon,
    TokenType.Star,
    TokenType.BangEqual,
    TokenType.Bang,
    TokenType.EqualEqual,
    TokenType.Equal,
    TokenType.GreaterEqual,
    TokenType.Greater,
    TokenType.LessEqual,
    TokenType.Less,
    TokenType.Slash,
  )

  val constants: Parser[Token] = constantLexemes
    // converts the list of keyword lexemes to a token if the keyword lexeme matches the input
    .map(kwl => kwl.lexeme ^^ { _ => Token(kwl, kwl.lexeme, null, 0) })
    // converts it to a parser that checks for any of the tokens in constantLexemes
    .reduce(_ | _)

  private val invalid: Parser[Token] = ".+".r ^^ { result => Token(TokenType.Invalid, result, null, 0) }

  // creates a combinator that searches for multiple tokens
  val parser: Parser[List[Token]] = rep(constants | invalid)
}

class Scanner(val source: String) {
  // generates a sequence of tokens from the source code using the parser combinator defined above.
  def apply(): Seq[Token] = {
    Scanner.parse(Scanner.parser, source) match {
      case Scanner.Success(matched, _) =>
        if (matched.exists(_.tokenType == TokenType.Invalid)) {
          val invalidToken = matched.find(_.tokenType == TokenType.Invalid).get.lexeme
          throw new Exception(s"Invalid token found: $invalidToken")
        } else matched
      case Scanner.Failure(msg, _) => throw new Exception(msg)
      case Scanner.Error(msg, _) => throw new Exception(msg)
      case _ => throw new Exception("Unknown error")
    }
  }
}
