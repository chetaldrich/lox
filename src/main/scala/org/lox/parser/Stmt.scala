package org.lox.parser

import org.lox.lexer.Token

sealed trait Stmt {
  def accept[R](visitor: StmtVisitor[R]): R
}

trait StmtVisitor[R] {
  def visitBlockStmt(block: Block): R

  def visitPrintStmt(printStmt: PrintStmt): R

  def visitExpressionStmt(exprStmt: ExpressionStmt): R

  def visitVarStmt(varStmt: VarStmt): R
}

case class PrintStmt(expr: Expr) extends Stmt {
  override def accept[R](visitor: StmtVisitor[R]): R = visitor.visitPrintStmt(this)
}

case class VarStmt(name: Token, initializer: Option[Expr]) extends Stmt {
  override def accept[R](visitor: StmtVisitor[R]): R = visitor.visitVarStmt(this)
}

case class ExpressionStmt(expr: Expr) extends Stmt {
  override def accept[R](visitor: StmtVisitor[R]): R = visitor.visitExpressionStmt(this)
}

case class Block(stmts: List[Stmt]) extends Stmt {
  override def accept[R](visitor: StmtVisitor[R]): R = visitor.visitBlockStmt(this)
}

