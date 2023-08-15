package org.lox

import org.lox.lexer.Token
import org.lox.lexer.TokenType._
import org.lox.parser.Stmt
import org.lox.parser.Expr
import org.lox.runtime.Interpreter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class InterpreterTest extends AnyFlatSpec with should.Matchers {

  def printStmt(expr: Expr): List[Stmt] = List(Stmt.Print(expr))

  def testInterpreterOutput(expected: String, expression: Expr): Unit = {
    val output = new java.io.ByteArrayOutputStream()
    val interpreter = Interpreter()
    Console.withOut(output) {
      interpreter.interpret(printStmt(expression))
    }
    output.toString.trim should be(expected)
  }

  it should "evaluate an addition binary expression statement correctly" in {
    val expr = Expr.Binary(Expr.Literal(1d), Token(Plus, "+", null, 0), Expr.Literal(2d))
    testInterpreterOutput("3", expr)
  }

  it should "evaluate an addition binary expression statement with strings correctly" in {
    val left = Expr.Literal("hello")
    val right = Expr.Literal(" world")
    val expr = Expr.Binary(left, Token(Plus, "+", null, 0), right)
    testInterpreterOutput("hello world", expr)
  }

  it should "evaluate a greater binary expression correctly" in {
    val left = Expr.Literal(1d)
    val right = Expr.Literal(2d)
    val expr = Expr.Binary(left, Token(Greater, ">", null, 0), right)
    testInterpreterOutput("false", expr)
  }

  it should "evaluate a GreaterEqual binary expression correctly" in {
    val falseExpr = Expr.Binary(Expr.Literal(1d), Token(GreaterEqual, ">=", null, 0), Expr.Literal(2d))
    val trueExpr = Expr.Binary(Expr.Literal(1d), Token(GreaterEqual, ">=", null, 0), Expr.Literal(1d))
    testInterpreterOutput("false", falseExpr)
    testInterpreterOutput("true", trueExpr)
  }

  it should "evaluate a LessEqual binary expression correctly" in {
    val expr1 = Expr.Binary(Expr.Literal(1d), Token(LessEqual, "<=", null, 0), Expr.Literal(2d))
    val expr2 = Expr.Binary(Expr.Literal(1d), Token(LessEqual, "<=", null, 0), Expr.Literal(1d))
    val expr3 = Expr.Binary(Expr.Literal(1d), Token(LessEqual, "<=", null, 0), Expr.Literal(0d))
    testInterpreterOutput("true", expr1)
    testInterpreterOutput("true", expr2)
    testInterpreterOutput("false", expr3)
  }

}
