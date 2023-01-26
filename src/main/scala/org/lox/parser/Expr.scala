package org.lox.parser

import org.lox.Token

sealed trait Expr {
  def accept[R](visitor: Visitor[R]): R
}

trait Visitor[R] {
  def visitBinaryExpr(expr: Binary): R

  def visitGroupingExpr(expr: Grouping): R

  def visitLiteralExpr(expr: Literal): R

  def visitUnaryExpr(expr: Unary): R
}

case class Binary(left: Expr, operator: Token, right: Expr) extends Expr {
  def accept[R](visitor: Visitor[R]): R = visitor.visitBinaryExpr(this)
}

case class Grouping(expression: Expr) extends Expr {
  def accept[R](visitor: Visitor[R]): R = visitor.visitGroupingExpr(this)
}

case class Literal(value: Any) extends Expr {
  def accept[R](visitor: Visitor[R]): R = visitor.visitLiteralExpr(this)
}

case class Unary(operator: Token, right: Expr) extends Expr {
  def accept[R](visitor: Visitor[R]): R = visitor.visitUnaryExpr(this)
}
