package org.lox

import org.scalatest._
import flatspec._
import org.lox.TokenType.EOF
import org.scalatest.matchers.should
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor2}

class ScannerTest extends AnyFlatSpec with should.Matchers with TableDrivenPropertyChecks {
  val eof: Token = Token(EOF, null, null, 0)
  def stringLiteral(str: String): Token = {
    Token(TokenType.String, s"\"$str\"", str, 0)
  }

  val scannerTests: TableFor2[String, Seq[Token]] = Table(
    ("input", "expected"),
    ("(", Seq(Token(TokenType.LeftParen, "(", null, 0), eof)),
    (")", Seq(Token(TokenType.RightParen, ")", null, 0), eof)),
    ("(    )", Seq(Token(TokenType.LeftParen, "(", null, 0), Token(TokenType.RightParen, ")", null, 0), eof)),
    ("{}", Seq(Token(TokenType.LeftBrace, "{", null, 0), Token(TokenType.RightBrace, "}", null, 0), eof)),
    (",", Seq(Token(TokenType.Comma, ",", null, 0), eof)),
    (".", Seq(Token(TokenType.Dot, ".", null, 0), eof)),
    ("-", Seq(Token(TokenType.Minus, "-", null, 0), eof)),
    ("+", Seq(Token(TokenType.Plus, "+", null, 0), eof)),
    (";", Seq(Token(TokenType.Semicolon, ";", null, 0), eof)),
    (":", Seq(Token(TokenType.Colon, ":", null, 0), eof)),
    ("?", Seq(Token(TokenType.QuestionMark, "?", null, 0), eof)),
    ("*", Seq(Token(TokenType.Star, "*", null, 0), eof)),
    ("!=", Seq(Token(TokenType.BangEqual, "!=", null, 0), eof)),
    ("!", Seq(Token(TokenType.Bang, "!", null, 0), eof)),
    ("==", Seq(Token(TokenType.EqualEqual, "==", null, 0), eof)),
    ("=", Seq(Token(TokenType.Equal, "=", null, 0), eof)),
    (">", Seq(Token(TokenType.Greater, ">", null, 0), eof)),
    ("<", Seq(Token(TokenType.Less, "<", null, 0), eof)),
    (">=", Seq(Token(TokenType.GreaterEqual, ">=", null, 0), eof)),
    ("<=", Seq(Token(TokenType.LessEqual, "<=", null, 0), eof)),
    ("/", Seq(Token(TokenType.Slash, "/", null, 0), eof)),
    ("// this is a comment.\n", Seq(eof)),
    ("// this is a comment.\n*", Seq(Token(TokenType.Star, "*", null, 0), eof)),
    ("\t \r \n", Seq(eof)),
    ("\"this is a string literal 123\"", Seq(stringLiteral("this is a string literal 123"), eof)),
    ("\"hello\" + \"world\"", Seq(stringLiteral("hello"), Token(TokenType.Plus, "+", null, 0), stringLiteral("world"), eof)),
    ("123.45", Seq(Token(TokenType.Number, "123.45", 123.45, 0), eof)),
    ("12", Seq(Token(TokenType.Number, "12", 12.toDouble, 0), eof)),
    ("1.2", Seq(Token(TokenType.Number, "1.2", 1.2, 0), eof)),
    ("orchid = 3", Seq(Token(TokenType.Identifier, "orchid", null, 0), Token(TokenType.Equal, "=", null, 0), Token(TokenType.Number, "3", 3.0, 0), eof)),
    ("or orchid", Seq(Token(TokenType.Or, "or", null, 0), Token(TokenType.Identifier, "orchid", null, 0), eof)),
    ("and andover", Seq(Token(TokenType.And, "and", null, 0), Token(TokenType.Identifier, "andover", null, 0), eof)),
    ("class classes", Seq(Token(TokenType.Class, "class", null, 0), Token(TokenType.Identifier, "classes", null, 0), eof)),
    ("false falsehood", Seq(Token(TokenType.False, "false", null, 0), Token(TokenType.Identifier, "falsehood", null, 0), eof)),
    ("else elsevier", Seq(Token(TokenType.Else, "else", null, 0), Token(TokenType.Identifier, "elsevier", null, 0), eof)),
    ("fun function", Seq(Token(TokenType.Fun, "fun", null, 0), Token(TokenType.Identifier, "function", null, 0), eof)),
    ("for fortune", Seq(Token(TokenType.For, "for", null, 0), Token(TokenType.Identifier, "fortune", null, 0), eof)),
    ("if iframe", Seq(Token(TokenType.If, "if", null, 0), Token(TokenType.Identifier, "iframe", null, 0), eof)),
    ("nil nilable", Seq(Token(TokenType.Nil, "nil", null, 0), Token(TokenType.Identifier, "nilable", null, 0), eof)),
    ("print printing", Seq(Token(TokenType.Print, "print", null, 0), Token(TokenType.Identifier, "printing", null, 0), eof)),
    ("return returnee", Seq(Token(TokenType.Return, "return", null, 0), Token(TokenType.Identifier, "returnee", null, 0), eof)),
    ("super superlative", Seq(Token(TokenType.Super, "super", null, 0), Token(TokenType.Identifier, "superlative", null, 0), eof)),
    ("this thistle", Seq(Token(TokenType.This, "this", null, 0), Token(TokenType.Identifier, "thistle", null, 0), eof)),
    ("true trueness", Seq(Token(TokenType.True, "true", null, 0), Token(TokenType.Identifier, "trueness", null, 0), eof)),
    ("var variable", Seq(Token(TokenType.Var, "var", null, 0), Token(TokenType.Identifier, "variable", null, 0), eof)),
    ("while whiles", Seq(Token(TokenType.While, "while", null, 0), Token(TokenType.Identifier, "whiles", null, 0), eof)),
  )

  forAll(scannerTests) { (input, expected) =>
    it should s"scan \"$input\" successfully" in {
      val scanner = new Scanner(input)
      scanner.apply.get should be(expected)
    }
  }

  it should "error on input 2@=" in {
    val scanner = new Scanner("2@=")
    assertThrows[Exception] {
      scanner.apply
    }
  }
}
