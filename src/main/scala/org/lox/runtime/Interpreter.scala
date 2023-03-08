package org.lox.runtime

import org.lox.lexer.TokenType._
import org.lox.parser._
import org.lox.RuntimeError
import org.lox.lexer.Token

import scala.util.Try

class Interpreter extends Visitor[Any] with StmtVisitor[Unit] {
  private var environment: Environment = new Environment

  def interpret(statements: Seq[Stmt]): Try[Unit] = Try {
    statements.foreach(execute)
  }

  def execute(statement: Stmt): Unit = statement.accept(this)

  private def stringify(value: Any): String = value match {
    case null => "nil"
    case d: Double =>
      val pattern = """^(\d+)(\..*)?$""".r
      pattern.findAllIn(d.toString).group(1)
    case v => v.toString
  }

  override def visitPrintStmt(printStmt: PrintStmt): Unit = println(stringify(evaluate(printStmt.expr)))

  override def visitExpressionStmt(expressionStmt: ExpressionStmt): Unit = evaluate(expressionStmt.expr)

  override def visitVarExpr(expr: Variable): Any = environment.get(expr.name).get

  override def visitVarStmt(varStmt: VarStmt): Unit = {
    val value: Option[Any] = varStmt.initializer.map(evaluate)
    environment.define(varStmt.name, value)
  }

  override def visitAssignmentExpression(assign: Assign): Any = {
    val value = evaluate(assign.value)
    environment.assign(assign.name, value)
  }

  override def visitBlockStmt(block: Block): Unit = {
    executeBlock(block.stmts, new Environment(Some(this.environment)))
  }

  def executeBlock(statements: List[Stmt], environment: Environment): Unit = {
    val previous = environment
    try {
      this.environment = environment
      statements.foreach(execute)
    } finally {
      this.environment = previous
    }
  }

  override def visitBinaryExpr(expr: Binary): Any = {
    val left = evaluate(expr.left)
    val right = evaluate(expr.right)
    expr.operator.tokenType match {
      case Minus => operate(left, right, expr.operator, (a: Double, b: Double) => a - b)
      case Slash => operate(left, right, expr.operator, (a: Double, b: Double) => a / b)
      case Star => operate(left, right, expr.operator, (a: Double, b: Double) => a * b)
      case Greater => operate(left, right, expr.operator, (a: Double, b: Double) => a > b)
      case GreaterEqual => operate(left, right, expr.operator, (a: Double, b: Double) => a >= b)
      case LessEqual => operate(left, right, expr.operator, (a: Double, b: Double) => a <= b)
      case Less => operate(left, right, expr.operator, (a: Double, b: Double) => a < b)
      case BangEqual => !isEqual(left, right)
      case Plus => add(left, right, expr.operator)
      case _ => null
    }
  }

  override def visitGroupingExpr(expr: Grouping): Any = expr.accept(this)

  override def visitLiteralExpr(expr: Literal): Any = expr.value

  override def visitTernaryExpr(expr: Ternary): Any = {
    if (isTruthy(evaluate(expr.condition))) evaluate(expr.`then`)
    else evaluate(expr.otherwise)
  }

  override def visitUnaryExpr(expr: Unary): Any = {
    val right = evaluate(expr.right)
    expr.operator.tokenType match {
      case Minus =>
        checkNumberOperands(expr.operator, right)
        -right.asInstanceOf[Double]
      case Bang => !isTruthy(right)
      case _ => null
    }
  }

  def operate[T](left: Any, right: Any, token: Token, operation: (Double, Double) => T): T = {
    checkNumberOperands(token, left, right)
    operation(left.asInstanceOf[Double], right.asInstanceOf[Double])
  }

  def checkNumberOperands(operator: Token, operands: Any*): Unit = {
    if (operands.forall(_.isInstanceOf[Double])) return
    throw new RuntimeError(operator, "Operands must be numbers")
  }

  def isEqual(left: Any, right: Any): Boolean = (left, right) match {
    case (null, null) => true
    case (null, _) => false
    case (left, right) => left == right
  }

  def add(left: Any, right: Any, operator: Token): Any = (left, right) match {
    case (left: Double, right: Double) => left + right
    case (left: String, right: String) => left + right
    case _ => throw new RuntimeError(operator, "Operands must be both Numbers or both Strings")
  }

  def evaluate(expr: Expr): Any = expr.accept(this)

  def isTruthy(value: Any): Boolean = value match {
    case null | false => false
    case _ => true
  }

}