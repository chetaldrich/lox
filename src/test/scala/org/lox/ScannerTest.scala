package org.lox

import org.scalatest._
import flatspec._
import org.scalatest.matchers.should
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor2}

class ScannerTest extends AnyFlatSpec with should.Matchers with TableDrivenPropertyChecks {

  val scannerTests: TableFor2[String, Seq[Token]] = Table(
    ("input", "expected"),
    ("(", Seq(Token(TokenType.LeftParen, "(", null, 0))),
    (")", Seq(Token(TokenType.RightParen, ")", null, 0))),
    ("(    )", Seq(Token(TokenType.LeftParen, "(", null, 0), Token(TokenType.RightParen, ")", null, 0))),
    ("{}", Seq(Token(TokenType.LeftBrace, "{", null, 0), Token(TokenType.RightBrace, "}", null, 0))),
    (",", Seq(Token(TokenType.Comma, ",", null, 0))),
    (".", Seq(Token(TokenType.Dot, ".", null, 0))),
    ("-", Seq(Token(TokenType.Minus, "-", null, 0))),
    ("+", Seq(Token(TokenType.Plus, "+", null, 0))),
    (";", Seq(Token(TokenType.Semicolon, ";", null, 0))),
    ("*", Seq(Token(TokenType.Star, "*", null, 0))),
    ("!=", Seq(Token(TokenType.BangEqual, "!=", null, 0))),
    ("!", Seq(Token(TokenType.Bang, "!", null, 0))),
    ("==", Seq(Token(TokenType.EqualEqual, "==", null, 0))),
    ("=", Seq(Token(TokenType.Equal, "=", null, 0))),
    (">", Seq(Token(TokenType.Greater, ">", null, 0))),
    ("<", Seq(Token(TokenType.Less, "<", null, 0))),
    (">=", Seq(Token(TokenType.GreaterEqual, ">=", null, 0))),
    ("<=", Seq(Token(TokenType.LessEqual, "<=", null, 0))),
    ("/", Seq(Token(TokenType.Slash, "/", null, 0))),
  )

  forAll(scannerTests) { (input, expected) =>
    it should s"scan $input successfully" in {
      val scanner = new Scanner(input)
      scanner.apply should be(expected)
    }
  }

  it should "error on input 2@=" in {
    val scanner = new Scanner("2@=")
    assertThrows[Exception] {
      scanner.apply()
    }
  }
}
