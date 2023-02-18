package org.lox.parser

import org.lox.{Token, TokenType}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ParserTest extends AnyFlatSpec with should.Matchers{

  it should "accept an equality expression" in {
    val tokens = List(
      Token(TokenType.Number, "1", 1d, 0),
      Token(TokenType.EqualEqual, "==", null, 0),
      Token(TokenType.Number, "1", 1d, 0),
      Token(TokenType.EOF, null, null, 0)
    )

    val expression = new Parser(tokens).parse
    expression should be(Binary(Literal(1), Token(TokenType.EqualEqual, "==", null, 0), Literal(1)))
  }

  it should "accept a ternary expression" in {
    val tokens = List(
      Token(TokenType.True, "true", true, 0),
      Token(TokenType.QuestionMark, "?", null, 0),
      Token(TokenType.Number, "1", 1d, 0),
      Token(TokenType.Colon, ":", null, 0),
      Token(TokenType.Number, "2", 2d, 0),
      Token(TokenType.EOF, null, null, 0)
    )

    val expression = new Parser(tokens).parse
    expression should be(Ternary(Literal(true), Literal(1d), Literal(2d)))
  }

  it should "accept a ternary expression with an equality expression as input" in {
    val equals = Token(TokenType.EqualEqual, "==", null, 0)
    val tokens = List(
      Token(TokenType.True, "true", true, 0),
      equals,
      Token(TokenType.False, "false", false, 0),
      Token(TokenType.QuestionMark, "?", null, 0),
      Token(TokenType.Number, "1", 1d, 0),
      Token(TokenType.Colon, ":", null, 0),
      Token(TokenType.Number, "2", 2d, 0),
      Token(TokenType.EOF, null, null, 0)
    )

    val expression = new Parser(tokens).parse
    expression should be(Ternary(Binary(Literal(true), equals, Literal(false)), Literal(1d), Literal(2d)))
  }
}
