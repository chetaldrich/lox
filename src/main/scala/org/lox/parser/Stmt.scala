package org.lox.parser

import org.lox.lexer.Token

sealed trait Stmt {
  def accept[R](visitor: StmtVisitor[R]): R
}

trait StmtVisitor[R] {
  def visitBlockStmt(block: BlockStmt): R

  def visitPrintStmt(printStmt: PrintStmt): R

  def visitExpressionStmt(exprStmt: ExpressionStmt): R

  def visitVarStmt(varStmt: VarStmt): R

  def visitIfStmt(ifStmt: IfStmt): R

  def visitWhileStmt(stmt: WhileStmt): R

  def visitBreakStmt(stmt: BreakStmt): R
}

case class BreakStmt() extends Stmt {
  override def accept[R](visitor: StmtVisitor[R]): R = visitor.visitBreakStmt(this)
}

case class WhileStmt(condition: Expr, body: Stmt) extends Stmt {
  override def accept[R](visitor: StmtVisitor[R]): R = visitor.visitWhileStmt(this)
}

case class IfStmt(condition: Expr, thenBranch: Stmt, elseBranch: Stmt) extends Stmt {
  override def accept[R](visitor: StmtVisitor[R]): R = visitor.visitIfStmt(this)
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

case class BlockStmt(stmts: List[Stmt]) extends Stmt {
  override def accept[R](visitor: StmtVisitor[R]): R = visitor.visitBlockStmt(this)
}

