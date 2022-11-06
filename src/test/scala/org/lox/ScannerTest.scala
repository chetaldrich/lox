package org.lox

import org.scalatest._
import flatspec._
import org.scalatest.matchers.should
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor2}

class ScannerTest extends AnyFlatSpec with should.Matchers with TableDrivenPropertyChecks {

  val scannerTests: TableFor2[String, Seq[Token]] = Table(
    ("input", "expected"),
    ("(", Seq(Token(LeftParen, "(", null, 0))),
    (")", Seq(Token(RightParen, ")", null, 0))),
    ("(    )", Seq(Token(LeftParen, "(", null, 0), Token(RightParen, ")", null, 0))),
    ("{}", Seq(Token(LeftBrace, "{", null, 0), Token(RightBrace, "}", null, 0))),
    (",", Seq(Token(Comma, ",", null, 0))),
    (".", Seq(Token(Dot, ".", null, 0))),
    ("-", Seq(Token(Minus, "-", null, 0))),
    ("+", Seq(Token(Plus, "+", null, 0))),
    (";", Seq(Token(Semicolon, ";", null, 0))),
    ("*", Seq(Token(Star, "*", null, 0))),
  )

  forAll(scannerTests) { (input, expected) =>
    it should s"scan $input successfully" in {
      val scanner = new Scanner(input)
      scanner.apply should be(expected)
    }
  }
}
