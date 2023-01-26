package org.lox.parser

import org.lox.{Token, TokenType}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class AstPrinterTest extends AnyFlatSpec with should.Matchers {

  it should "pretty print an expression successfully" in {
    val expression = Binary(
      Unary(Token(TokenType.Minus, "-", null, 1), Literal(123)),
      Token(TokenType.Star, "*", null, 1),
      Grouping(Literal(45.67))
    )

    val printer = new AstPrinter
    printer.print(expression) should be("(* (- 123) (group 45.67))")
  }

}
