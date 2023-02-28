package org.lox.parser

class AstPrinter extends Visitor[String] {
  override def visitBinaryExpr(expr: Binary): String = parenthesize(expr.operator.lexeme, expr.left, expr.right)

  override def visitTernaryExpr(expr: Ternary): String = {
    parenthesize("ternary", expr.condition, expr.`then`, expr.otherwise)
  }

  override def visitGroupingExpr(expr: Grouping): String = parenthesize("group", expr.expression)

  override def visitLiteralExpr(expr: Literal): String = {
    if (expr.value == null) "nil"
    else expr.value.toString
  }

  override def visitUnaryExpr(expr: Unary): String = parenthesize(expr.operator.lexeme, expr.right)

  def print(expr: Expr): String = expr.accept(this)

  private def parenthesize(name: String, exprs: Expr*): String = {
    s"($name ${exprs.map(_.accept(this)).mkString(" ")})"
  }

  override def visitVarExpr(variable: Variable): String = s"(var ${variable.name.lexeme})"
}
