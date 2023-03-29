package org.lox.parser

import org.lox.lexer.{Token, TokenType}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ParserTest extends AnyFlatSpec with should.Matchers {

  val eof: Token = Token(TokenType.EOF, null)
  val semicolon: Token = Token(TokenType.Semicolon, null)
  val eos: Seq[Token] = List(semicolon, eof)

  def exprStmt(expr: Expr): List[Stmt.Expression] = List(Stmt.Expression(expr))

  it should "accept a break statement inside a while loop" in {
    val tokens = List(
      Token(TokenType.While, "while"),
      Token(TokenType.LeftParen, "("),
      Token(TokenType.True, "true"),
      Token(TokenType.RightParen, ")"),
      Token(TokenType.LeftBrace, "{"),
      Token(TokenType.Break, "break"),
      semicolon,
      Token(TokenType.RightBrace, "}"),
      eof
    )

    val statements = new Parser(tokens).parse
    statements.get should be(List(Stmt.While(Expr.Literal(true), Stmt.Block(List(Stmt.Break())))))
  }

  it should "accept an equality expression statement" in {
    val tokens = List(
      Token(TokenType.Number, "1", 1d),
      Token(TokenType.EqualEqual, "=="),
      Token(TokenType.Number, "1", 1d),
    ) ++ eos

    val statements = new Parser(tokens).parse
    statements.get should be(exprStmt(Expr.Binary(Expr.Literal(1), Token(TokenType.EqualEqual, "=="), Expr.Literal(1))))
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
    statements.get should be(exprStmt(Expr.Ternary(Expr.Literal(true), Expr.Literal(1d), Expr.Literal(2d))))
  }

  it should "accept a declaration" in {
    val tokens = List(
      Token(TokenType.Var, "var"),
      Token(TokenType.Identifier, "a"),
      Token(TokenType.Equal, "="),
      Token(TokenType.String, "\"global a\"", "global a")
    ) ++ eos

    val statements = new Parser(tokens).parse
    statements.get should be(List(Stmt.Var(Token(TokenType.Identifier, "a"), Some(Expr.Literal("global a")))))
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
    statements.get should be(List(Stmt.Block(List(Stmt.Var(Token(TokenType.Identifier, "a"), Some(Expr.Literal("global a")))))))
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
    statements.get should be(exprStmt(Expr.Ternary(Expr.Binary(Expr.Literal(true), equals, Expr.Literal(false)), Expr.Literal(1d), Expr.Literal(2d))))
  }
}
