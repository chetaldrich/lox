package org.lox.runtime

import org.lox.lexer.Token
import org.lox.lexer.TokenType._
import org.lox.parser._
import org.lox.runtime.functions.LoxFunction
import org.lox.runtime.functions.builtins.Clock

import scala.util.Try

object Interpreter {
  def apply(): Interpreter = {
    val globals = new Environment
    globals.defineGlobal("clock", Some(Clock()))
    new Interpreter(globals)
  }
}

class Interpreter(val globals: Environment) extends Visitor[Any] with StmtVisitor[Unit] {
  private var environment: Environment = globals

  def interpret(statements: Seq[Stmt]): Try[Unit] = Try {
    statements.foreach(execute)
  }

  def interpretExpression(expr: Expr): Try[Any] = Try(expr.accept(this))

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

  override def visitBlockStmt(block: BlockStmt): Unit = {
    executeBlock(block.stmts, new Environment(Some(this.environment)))
  }

  def executeBlock(statements: List[Stmt], environment: Environment): Unit = {
    val previous = this.environment
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
      case EqualEqual => isEqual(left, right)
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
    throw RuntimeError(operator, "Operands must be numbers")
  }

  def isEqual(left: Any, right: Any): Boolean = (left, right) match {
    case (null, null) => true
    case (null, _) => false
    case (left, right) => left == right
  }

  def add(left: Any, right: Any, operator: Token): Any = (left, right) match {
    case (left: Double, right: Double) => left + right
    case (left: String, right: String) => left + right
    case _ => throw RuntimeError(operator, "Operands must be both Numbers or both Strings")
  }

  private def evaluate(expr: Expr): Any = expr.accept(this)

  private def isTruthy(value: Any): Boolean = value match {
    case null | false => false
    case _ => true
  }

  override def visitIfStmt(ifStmt: IfStmt): Unit = {
    if (isTruthy(evaluate(ifStmt.condition))) execute(ifStmt.thenBranch)
    else if (ifStmt.elseBranch != null) execute(ifStmt.elseBranch)
  }

  override def visitLogicalExpr(expr: Logical): Any = {
    val left = evaluate(expr.left)
    if (expr.operator.tokenType == Or && isTruthy(left)) left
    else if (!isTruthy(left)) left
    else evaluate(expr.right)
  }

  override def visitWhileStmt(stmt: WhileStmt): Unit = {
    try {
      while (isTruthy(evaluate(stmt.condition))) {
        execute(stmt.body)
      }
    } catch {
      case _: BreakError =>
    }
  }

  override def visitBreakStmt(stmt: BreakStmt): Unit = {
    throw new BreakError()
  }

  override def visitCallExpr(expr: Call): Any = {
    val callee = evaluate(expr.callee)
    val arguments = expr.arguments.map(evaluate)
    callee match {
      case c: LoxCallable =>
        if (arguments.size != c.arity) {
          throw RuntimeError(expr.paren, s"Expected ${c.arity} arguments but got ${arguments.size}.")
        }
        c.call(this, arguments)
      case _ => throw RuntimeError(expr.paren, "Can only call functions and classes.")
    }
  }

  override def visitFunctionStmt(stmt: FunctionStmt): Unit = environment.define(stmt.name, Some(LoxFunction(stmt, environment)))

  override def visitReturnStmt(stmt: ReturnStmt): Unit = {
    throw FunctionReturn(Option(stmt.value).map(evaluate).orNull)
  }

  override def visitLambdaExpr(expr: Lambda): Any = {
    LoxFunction(expr, environment)
  }
}
