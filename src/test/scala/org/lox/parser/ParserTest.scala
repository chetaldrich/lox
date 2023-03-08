package org.lox.parser

import org.lox.lexer.{Token, TokenType}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ParserTest extends AnyFlatSpec with should.Matchers {

  val eof: Token = Token(TokenType.EOF, null)
  val semicolon: Token = Token(TokenType.Semicolon, null)
  val eos: Seq[Token] = List(semicolon, eof)

  def exprStmt(expr: Expr): List[ExpressionStmt] = List(ExpressionStmt(expr))

  it should "accept an equality expression statement" in {
    val tokens = List(
      Token(TokenType.Number, "1", 1d),
      Token(TokenType.EqualEqual, "=="),
      Token(TokenType.Number, "1", 1d),
    ) ++ eos

    val statements = new Parser(tokens).parse
    statements.get should be(exprStmt(Binary(Literal(1), Token(TokenType.EqualEqual, "==", null, 0), Literal(1))))
  }

  it should "accept a ternary expression statement" in {
    val tokens = List(
      Token(TokenType.True, "true", true),
      Token(TokenType.QuestionMark, "?"),
      Token(TokenType.Number, "1", 1d),
      Token(TokenType.Colon, ":"),
      Token(TokenType.Number, "2", 2d),
    ) ++ eos

    val statements = new Parser(tokens).parse
    statements.get should be(exprStmt(Ternary(Literal(true), Literal(1d), Literal(2d))))
  }

  it should "accept a declaration" in {
    val tokens = List(
      Token(TokenType.Var, "var"),
      Token(TokenType.Identifier, "a"),
      Token(TokenType.Equal, "="),
      Token(TokenType.String, "\"global a\"", "global a")
    ) ++ eos

    val statements = new Parser(tokens).parse
    statements.get should be(List(VarStmt(Token(TokenType.Identifier, "a"), Some(Literal("global a")))))
  }

  it should "accept a block" in {
    val tokens = List(
      Token(TokenType.LeftBrace, "{"),
      Token(TokenType.Var, "var"),
      Token(TokenType.Identifier, "a"),
      Token(TokenType.Equal, "="),
      Token(TokenType.String, "\"global a\"", "global a"),
      semicolon,
      Token(TokenType.RightBrace, "}"),
      eof
    )

    val statements = new Parser(tokens).parse
    statements.get should be(List(Block(List(VarStmt(Token(TokenType.Identifier, "a"), Some(Literal("global a")))))))
  }

  it should "accept a ternary expression with an equality expression as input" in {
    val equals = Token(TokenType.EqualEqual, "==")
    val tokens = List(
      Token(TokenType.True, "true", true),
      equals,
      Token(TokenType.False, "false", false),
      Token(TokenType.QuestionMark, "?"),
      Token(TokenType.Number, "1", 1d),
      Token(TokenType.Colon, ":"),
      Token(TokenType.Number, "2", 2d),
    ) ++ eos

    val statements = new Parser(tokens).parse
    statements.get should be(exprStmt(Ternary(Binary(Literal(true), equals, Literal(false)), Literal(1d), Literal(2d))))
  }
}
