package org.lox

import org.lox.TokenType._

// https://www.scala-lang.org/api/2.12.8/scala-parser-combinators/scala/util/parsing/combinator/Parsers.html
import scala.util.parsing.combinator._

object SimpleParser extends RegexParsers {
  def word: Parser[String] = """[a-z]+""".r ^^ {
    _.toString
  }
}

class Scanner(val source: String) {
  private val whitespace = "\\s+".r

  def apply(): Seq[Token] = {
    SimpleParser.parse(SimpleParser.word, source) match {
      case SimpleParser.Success(result, _) => Seq(Token(IDENTIFIER, result, null, 1))
      case SimpleParser.NoSuccess(msg, _) => throw new Exception(msg)
      case _ => throw new Exception("Unknown error")
    }
  }
}
