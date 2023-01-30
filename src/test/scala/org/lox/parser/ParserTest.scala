package org.lox.parser

import org.lox.{Token, TokenType}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ParserTest extends AnyFlatSpec with should.Matchers{

  it should "accept an equality expression" in {
    val tokens = List(
      Token(TokenType.Number, "1", "1".toDouble, 0),
      Token(TokenType.EqualEqual, "==", null, 0),
      Token(TokenType.Number, "1", "1".toDouble, 0),
      Token(TokenType.EOF, null, null, 0)
    )

    val expression = new Parser(tokens).parse
    expression should be(Binary(Literal(1), Token(TokenType.EqualEqual, "==", null, 0), Literal(1)))
  }

}
