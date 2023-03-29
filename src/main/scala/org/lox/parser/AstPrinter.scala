package org.lox.parser

class AstPrinter extends Expr.Visitor[String] {
  override def visitBinaryExpr(expr: Expr.Binary): String = parenthesize(expr.operator.lexeme, expr.left, expr.right)

  override def visitTernaryExpr(expr: Expr.Ternary): String = {
    parenthesize("ternary", expr.condition, expr.`then`, expr.otherwise)
  }

  override def visitGroupingExpr(expr: Expr.Grouping): String = parenthesize("group", expr.expression)

  override def visitLiteralExpr(expr: Expr.Literal): String = {
    if (expr.value == null) "nil"
    else expr.value.toString
  }

  override def visitUnaryExpr(expr: Expr.Unary): String = parenthesize(expr.operator.lexeme, expr.right)

  def print(expr: Expr): String = expr.accept(this)

  private def parenthesize(name: String, exprs: Expr*): String = {
    s"($name ${exprs.map(_.accept(this)).mkString(" ")})"
  }

  override def visitVarExpr(variable: Expr.Variable): String = s"(var ${variable.name.lexeme})"

  override def visitAssignmentExpression(assign: Expr.Assign): String = parenthesize(assign.name.lexeme, assign.value)

  override def visitLogicalExpr(expr: Expr.Logical): String = parenthesize(expr.operator.lexeme, expr.left, expr.right)

  override def visitCallExpr(expr: Expr.Call): String = {
    val all = expr.callee :: expr.arguments
    parenthesize("fn", all: _*)
  }

  override def visitLambdaExpr(expr: Expr.Lambda): String = parenthesize(expr.fName)
}
