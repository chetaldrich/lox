package org.lox.parser

import org.lox.lexer.{Token, TokenType}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class AstPrinterTest extends AnyFlatSpec with should.Matchers {

  it should "pretty print an expression successfully" in {
    // -123 * 45.67
    val expression = Binary(
      Unary(Token(TokenType.Minus, "-", null, 1), Literal(123)),
      Token(TokenType.Star, "*", null, 1),
      Literal(45.67)
    )

    val printer = new AstPrinter
    printer.print(expression) should be("(* (- 123) 45.67)")
  }

  it should "pretty print a variable expression successfully" in {
    val variableExpression = Variable(Token(TokenType.Identifier, "someVariableName", null, 0))
    val printer = new AstPrinter
    printer.print(variableExpression) should be("(var someVariableName)")
  }
}
