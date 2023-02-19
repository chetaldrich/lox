package org.lox.parser

sealed trait Stmt {
  def accept[R](visitor: StmtVisitor[R]): R
}

trait StmtVisitor[R] {
  def visitPrintStmt(expr: Expr): R
  def visitExpressionStmt(expr: Expr): R
}

case class PrintStmt(expr: Expr) extends Stmt {
  override def accept[R](visitor: StmtVisitor[R]): R = visitor.visitPrintStmt(expr)
}

case class ExpressionStmt(expr: Expr) extends Stmt {
  override def accept[R](visitor: StmtVisitor[R]): R = visitor.visitExpressionStmt(expr)
}

