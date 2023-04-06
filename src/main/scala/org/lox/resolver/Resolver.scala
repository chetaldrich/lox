package org.lox.resolver

import org.lox.Lox
import org.lox.lexer.Token
import org.lox.parser.{Expr, Stmt}
import org.lox.runtime.Interpreter

import scala.collection.mutable
import scala.util.Try

case class Resolver(interpreter: Interpreter) extends Expr.Visitor[Unit] with Stmt.Visitor[Unit] {

  val scopes: mutable.Stack[mutable.Map[String, Boolean]] = new mutable.Stack

  def resolve(stmts: List[Stmt]): Unit = stmts.foreach(resolve)

  private def resolve(stmt: Stmt): Unit = stmt.accept(this)

  private def resolve(exprs: Expr*): Unit = exprs.foreach(_.accept(this))

  private def scoped(fn: => Unit): Unit = {
    scopes.push(mutable.Map())
    fn
    scopes.pop()
  }

  private def declare(name: Token): Unit = {
    if (scopes.isEmpty) return
    scopes.top.put(name.lexeme, false)
  }

  private def define(name: Token): Unit = {
    if (scopes.isEmpty) return
    scopes.top.put(name.lexeme, true)
  }


  override def visitLogicalExpr(expr: Expr.Logical): Unit = resolve(expr.left, expr.right)

  override def visitAssignExpr(assign: Expr.Assign): Unit = {
    resolve(assign.value)
    resolveLocal(assign, assign.name)
  }

  override def visitTernaryExpr(expr: Expr.Ternary): Unit = resolve(expr.condition, expr.`then`, expr.otherwise)


  override def visitBinaryExpr(expr: Expr.Binary): Unit = resolve(expr.left, expr.right)

  override def visitGroupingExpr(expr: Expr.Grouping): Unit = resolve(expr.expression)

  override def visitLiteralExpr(expr: Expr.Literal): Unit = {}

  override def visitUnaryExpr(expr: Expr.Unary): Unit = resolve(expr.right)

  override def visitVarExpr(expr: Expr.Variable): Unit = {
    if (scopes.nonEmpty && !scopes.top(expr.name.lexeme)) {
      Lox.error(expr.name, "Can't read local variable in its own initializer.")
    }
    resolveLocal(expr, expr.name)
  }

  private def resolveLocal(expression: Expr, name: Token): Unit = {
    scopes.zipWithIndex.reverse.foreach { case (scope, i) =>
      if (scope.contains(name.lexeme)) {
        interpreter.resolve(expression, scopes.size - 1 - i)
        return
      }
    }
  }

  override def visitCallExpr(expr: Expr.Call): Unit = {
    resolve(expr.callee)
    expr.arguments.foreach { arg =>
      resolve(arg)
    }
  }

  override def visitLambdaExpr(expr: Expr.Lambda): Unit = {
    expr.params.foreach { param =>
      declare(param)
      define(param)
    }
    resolve(expr.body)
  }

  override def visitBlockStmt(block: Stmt.Block): Unit = scoped {
    resolve(block.stmts)
  }

  override def visitPrintStmt(stmt: Stmt.Print): Unit = if (stmt.expr != null) resolve(stmt.expr)

  override def visitExpressionStmt(stmt: Stmt.Expression): Unit = resolve(stmt.expr)


  override def visitIfStmt(stmt: Stmt.If): Unit = {
    resolve(stmt.condition)
    resolve(stmt.thenBranch)
    if (stmt.elseBranch != null) resolve(stmt.elseBranch)
  }


  override def visitVarStmt(stmt: Stmt.Var): Unit = {
    declare(stmt.name)
    stmt.initializer.foreach(resolve(_))
    define(stmt.name)
  }

  override def visitWhileStmt(stmt: Stmt.While): Unit = {
    resolve(stmt.condition)
    resolve(stmt.body)
  }

  override def visitBreakStmt(stmt: Stmt.Break): Unit = {}

  private def resolveFunction(function: Stmt.Function): Unit = scoped {
    function.params.foreach { param =>
      declare(param)
      define(param)
    }
    resolve(function.body)
  }

  override def visitFunctionStmt(stmt: Stmt.Function): Unit = {
    declare(stmt.name)
    define(stmt.name)
    resolveFunction(stmt)
  }

  override def visitReturnStmt(stmt: Stmt.Return): Unit = if (stmt.value != null) resolve(stmt.value)
}
