package org.lox

import org.scalatest._
import flatspec._
import org.lox.lexer.TokenType.EOF
import org.lox.lexer.{Scanner, Token, TokenType}
import org.scalatest.matchers.should
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor2}

class ScannerTest extends AnyFlatSpec with should.Matchers with TableDrivenPropertyChecks {
  val eof: Token = Token(EOF, null, null, 0)
  def stringLiteral(str: String): Token = {
    Token(TokenType.String, s"\"$str\"", str, 0)
  }

  val scannerTests: TableFor2[String, Seq[Token]] = Table(
    ("input", "expected"),
    ("(", Seq(Token(TokenType.LeftParen, "("), eof)),
    (")", Seq(Token(TokenType.RightParen, ")"), eof)),
    ("(    )", Seq(Token(TokenType.LeftParen, "("), Token(TokenType.RightParen, ")"), eof)),
    ("{}", Seq(Token(TokenType.LeftBrace, "{"), Token(TokenType.RightBrace, "}"), eof)),
    (",", Seq(Token(TokenType.Comma, ","), eof)),
    (".", Seq(Token(TokenType.Dot, "."), eof)),
    ("-", Seq(Token(TokenType.Minus, "-"), eof)),
    ("+", Seq(Token(TokenType.Plus, "+"), eof)),
    (";", Seq(Token(TokenType.Semicolon, ";"), eof)),
    (":", Seq(Token(TokenType.Colon, ":"), eof)),
    ("?", Seq(Token(TokenType.QuestionMark, "?"), eof)),
    ("*", Seq(Token(TokenType.Star, "*"), eof)),
    ("!=", Seq(Token(TokenType.BangEqual, "!="), eof)),
    ("!", Seq(Token(TokenType.Bang, "!"), eof)),
    ("==", Seq(Token(TokenType.EqualEqual, "=="), eof)),
    ("=", Seq(Token(TokenType.Equal, "="), eof)),
    (">", Seq(Token(TokenType.Greater, ">"), eof)),
    ("<", Seq(Token(TokenType.Less, "<"), eof)),
    (">=", Seq(Token(TokenType.GreaterEqual, ">="), eof)),
    ("<=", Seq(Token(TokenType.LessEqual, "<="), eof)),
    ("/", Seq(Token(TokenType.Slash, "/"), eof)),
    ("// this is a comment.\n", Seq(eof)),
    ("// this is a comment.\n*", Seq(Token(TokenType.Star, "*"), eof)),
    ("\t \r \n", Seq(eof)),
    ("\"this is a string literal 123\"", Seq(stringLiteral("this is a string literal 123"), eof)),
    ("\"hello\" + \"world\"", Seq(stringLiteral("hello"), Token(TokenType.Plus, "+"), stringLiteral("world"), eof)),
    ("123.45", Seq(Token(TokenType.Number, "123.45", 123.45), eof)),
    ("12", Seq(Token(TokenType.Number, "12", 12.toDouble), eof)),
    ("1.2", Seq(Token(TokenType.Number, "1.2", 1.2), eof)),
    ("orchid = 3", Seq(Token(TokenType.Identifier, "orchid"), Token(TokenType.Equal, "="), Token(TokenType.Number, "3", 3.0, 0), eof)),
    ("or orchid", Seq(Token(TokenType.Or, "or"), Token(TokenType.Identifier, "orchid"), eof)),
    ("and andover", Seq(Token(TokenType.And, "and"), Token(TokenType.Identifier, "andover"), eof)),
    ("break breakout", Seq(Token(TokenType.Break, "break"), Token(TokenType.Identifier, "breakout"), eof)),
    ("class classes", Seq(Token(TokenType.Class, "class"), Token(TokenType.Identifier, "classes"), eof)),
    ("false falsehood", Seq(Token(TokenType.False, "false"), Token(TokenType.Identifier, "falsehood"), eof)),
    ("else elsevier", Seq(Token(TokenType.Else, "else"), Token(TokenType.Identifier, "elsevier"), eof)),
    ("fun function", Seq(Token(TokenType.Fun, "fun"), Token(TokenType.Identifier, "function"), eof)),
    ("for fortune", Seq(Token(TokenType.For, "for"), Token(TokenType.Identifier, "fortune"), eof)),
    ("if iframe", Seq(Token(TokenType.If, "if"), Token(TokenType.Identifier, "iframe"), eof)),
    ("nil nilable", Seq(Token(TokenType.Nil, "nil"), Token(TokenType.Identifier, "nilable"), eof)),
    ("print printing", Seq(Token(TokenType.Print, "print"), Token(TokenType.Identifier, "printing"), eof)),
    ("return returnee", Seq(Token(TokenType.Return, "return"), Token(TokenType.Identifier, "returnee"), eof)),
    ("super superlative", Seq(Token(TokenType.Super, "super"), Token(TokenType.Identifier, "superlative"), eof)),
    ("this thistle", Seq(Token(TokenType.This, "this"), Token(TokenType.Identifier, "thistle"), eof)),
    ("true trueness", Seq(Token(TokenType.True, "true"), Token(TokenType.Identifier, "trueness"), eof)),
    ("var variable", Seq(Token(TokenType.Var, "var"), Token(TokenType.Identifier, "variable"), eof)),
    ("while whiles", Seq(Token(TokenType.While, "while"), Token(TokenType.Identifier, "whiles"), eof)),
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
