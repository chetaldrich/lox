package org.lox

import org.lox.lexer.Token
import org.lox.lexer.TokenType.{Greater, GreaterEqual, LessEqual, Plus}
import org.lox.parser.{Binary, Expr, ExpressionStmt, Literal, PrintStmt, Stmt}
import org.lox.runtime.Interpreter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class InterpreterTest extends AnyFlatSpec with should.Matchers {

  def printStmt(expr: Expr): List[Stmt] = List(PrintStmt(expr))

  def testInterpreterOutput(expected: String, expression: Expr): Unit = {
    val output = new java.io.ByteArrayOutputStream()
    val interpreter = new Interpreter
    Console.withOut(output) {
      interpreter.interpret(printStmt(expression))
    }
    output.toString.trim should be(expected)
  }

  it should "evaluate an addition binary expression statement correctly" in {
    val expr = Binary(Literal(1d), Token(Plus, "+", null, 0), Literal(2d))
    testInterpreterOutput("3", expr)
  }

  it should "evaluate an addition binary expression statement with strings correctly" in {
    val left = Literal("hello")
    val right = Literal(" world")
    val expr = Binary(left, Token(Plus, "+", null, 0), right)
    testInterpreterOutput("hello world", expr)
  }

  it should "evaluate a greater binary expression correctly" in {
    val left = Literal(1d)
    val right = Literal(2d)
    val expr = Binary(left, Token(Greater, ">", null, 0), right)
    testInterpreterOutput("false", expr)
  }

  it should "evaluate a GreaterEqual binary expression correctly" in {
    val falseExpr = Binary(Literal(1d), Token(GreaterEqual, ">=", null, 0), Literal(2d))
    val trueExpr = Binary(Literal(1d), Token(GreaterEqual, ">=", null, 0), Literal(1d))
    testInterpreterOutput("false", falseExpr)
    testInterpreterOutput("true", trueExpr)
  }

  it should "evaluate a LessEqual binary expression correctly" in {
    val expr1 = Binary(Literal(1d), Token(LessEqual, "<=", null, 0), Literal(2d))
    val expr2 = Binary(Literal(1d), Token(LessEqual, "<=", null, 0), Literal(1d))
    val expr3 = Binary(Literal(1d), Token(LessEqual, "<=", null, 0), Literal(0d))
    testInterpreterOutput("true", expr1)
    testInterpreterOutput("true", expr2)
    testInterpreterOutput("false", expr3)
  }

}
