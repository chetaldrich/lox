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
    ("// this is a comment.\n", Seq()),
    ("// this is a comment.\n*", Seq(Token(TokenType.Star, "*", null, 0))),
    ("\t \r \n", Seq()),
    ("\"this is a string literal 123\"", Seq(Token(TokenType.String, "\"this is a string literal 123\"", "this is a string literal 123", 0))),
    ("123.45", Seq(Token(TokenType.Number, "123.45", 123.45, 0))),
    ("12", Seq(Token(TokenType.Number, "12", 12.toDouble, 0))),
    ("1.2", Seq(Token(TokenType.Number, "1.2", 1.2, 0))),
    ("orchid = 3", Seq(Token(TokenType.Identifier, "orchid", null, 0), Token(TokenType.Equal, "=", null, 0), Token(TokenType.Number, "3", 3.0, 0))),
    ("or orchid", Seq(Token(TokenType.Or, "or", null, 0), Token(TokenType.Identifier, "orchid", null, 0))),
    ("and andover", Seq(Token(TokenType.And, "and", null, 0), Token(TokenType.Identifier, "andover", null, 0))),
    ("class classes", Seq(Token(TokenType.Class, "class", null, 0), Token(TokenType.Identifier, "classes", null, 0))),
    ("false falsehood", Seq(Token(TokenType.False, "false", null, 0), Token(TokenType.Identifier, "falsehood", null, 0))),
    ("else elsevier", Seq(Token(TokenType.Else, "else", null, 0), Token(TokenType.Identifier, "elsevier", null, 0))),
    ("fun function", Seq(Token(TokenType.Fun, "fun", null, 0), Token(TokenType.Identifier, "function", null, 0))),
    ("for fortune", Seq(Token(TokenType.For, "for", null, 0), Token(TokenType.Identifier, "fortune", null, 0))),
    ("if iframe", Seq(Token(TokenType.If, "if", null, 0), Token(TokenType.Identifier, "iframe", null, 0))),
    ("nil nilable", Seq(Token(TokenType.Nil, "nil", null, 0), Token(TokenType.Identifier, "nilable", null, 0))),
    ("print printing", Seq(Token(TokenType.Print, "print", null, 0), Token(TokenType.Identifier, "printing", null, 0))),
    ("return returnee", Seq(Token(TokenType.Return, "return", null, 0), Token(TokenType.Identifier, "returnee", null, 0))),
    ("super superlative", Seq(Token(TokenType.Super, "super", null, 0), Token(TokenType.Identifier, "superlative", null, 0))),
    ("this thistle", Seq(Token(TokenType.This, "this", null, 0), Token(TokenType.Identifier, "thistle", null, 0))),
    ("true trueness", Seq(Token(TokenType.True, "true", null, 0), Token(TokenType.Identifier, "trueness", null, 0))),
    ("var variable", Seq(Token(TokenType.Var, "var", null, 0), Token(TokenType.Identifier, "variable", null, 0))),
    ("while whiles", Seq(Token(TokenType.While, "while", null, 0), Token(TokenType.Identifier, "whiles", null, 0))),
  )

  forAll(scannerTests) { (input, expected) =>
    it should s"scan \"$input\" successfully" in {
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
