package org.lox

import scala.util.parsing.combinator._

object Scanner extends RegexParsers {
  // A list of all the keywords in the language.
  private val keywordLexemes = List(
    LeftParen,
    RightParen
  )

  def constantLexemes: Parser[Token] = keywordLexemes
    // converts the list of keyword lexemes to a token if the keyword lexeme matches the input
    .map(kwl => kwl.lexeme ^^ { _ => Token(kwl, kwl.lexeme, null, 0) })
    // converts it to a parser that checks for any of the tokens in keywordLexemes
    .reduce(_ | _)

  // creates a parser that searches for multiple tokens in the input of the input parser, which in
  // this case is the constantLexemes parser
  val parser: Parser[List[Token]] = rep(constantLexemes)
}

class Scanner(val source: String) {
  // generates a sequence of tokens from the source code using the parser combinator defined above.
  def apply(): Seq[Token] = {
    Scanner.parse(Scanner.parser, source) match {
      case Scanner.Success(matched, _) => matched
      case Scanner.Failure(msg, _) => throw new Exception(msg)
      case Scanner.Error(msg, _) => throw new Exception(msg)
      case _ => throw new Exception("Unknown error")
    }
  }
}
