package org.lox

import org.scalatest._
import flatspec._
import org.scalatest.matchers.should

class ScannerTest extends AnyFlatSpec with should.Matchers {
  it should "be able to scan an open parenthesis" in {
    val scanner = new Scanner("(")
    scanner.apply should be (Seq(Token(LeftParen, "(", null, 0)))
  }

  it should  "be able to scan a close parenthesis" in {
    val scanner = new Scanner(")")
    scanner.apply should be (Seq(Token(RightParen, ")", null, 0)))
  }
}
