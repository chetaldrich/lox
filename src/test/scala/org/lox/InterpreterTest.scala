package org.lox

import org.lox.TokenType.{Greater, GreaterEqual, LessEqual, Plus}
import org.lox.parser.{Binary, Literal}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class InterpreterTest extends AnyFlatSpec with should.Matchers {

  it should "evaluate an addition binary expression correctly" in {
    val left = Literal(1d)
    val right = Literal(2d)
    val expr = Binary(left, Token(Plus, "+", null, 0), right)
    val interpreter = new Interpreter
    interpreter.interpret(expr).get should be("3")
  }

  it should "evaluate an addition binary expression with strings correctly" in {
    val left = Literal("hello")
    val right = Literal(" world")
    val expr = Binary(left, Token(Plus, "+", null, 0), right)
    val interpreter = new Interpreter
    interpreter.interpret(expr).get should be("hello world")
  }

  it should "evaluate a greater binary expression correctly" in {
    val left = Literal(1d)
    val right = Literal(2d)
    val expr = Binary(left, Token(Greater, ">", null, 0), right)
    val interpreter = new Interpreter
    interpreter.interpret(expr).get should be("false")
  }

  it should "evaluate a GreaterEqual binary expression correctly" in {
    val falseExpr = Binary(Literal(1d), Token(GreaterEqual, ">=", null, 0), Literal(2d))
    val trueExpr = Binary(Literal(1d), Token(GreaterEqual, ">=", null, 0), Literal(1d))
    val interpreter = new Interpreter
    interpreter.interpret(falseExpr).get should be("false")
    interpreter.interpret(trueExpr).get should be("true")
  }

  it should "evaluate a LessEqual binary expression correctly" in {
    val expr1 = Binary(Literal(1d), Token(LessEqual, "<=", null, 0), Literal(2d))
    val expr2 = Binary(Literal(1d), Token(LessEqual, "<=", null, 0), Literal(1d))
    val expr3 = Binary(Literal(1d), Token(LessEqual, "<=", null, 0), Literal(0d))
    val interpreter = new Interpreter
    interpreter.interpret(expr1).get should be("true")
    interpreter.interpret(expr2).get should be("true")
    interpreter.interpret(expr3).get should be("false")
  }

}
