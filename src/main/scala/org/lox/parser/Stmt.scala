package org.lox.parser

import org.lox.lexer.Token

sealed trait Stmt {
  def accept[R](visitor: Stmt.Visitor[R]): R
}

object Stmt {
  trait Visitor[R] {
    def visitClassStmt(clazz: Class): R

    def visitBlockStmt(block: Block): R

    def visitPrintStmt(printStmt: Print): R

    def visitExpressionStmt(exprStmt: Expression): R

    def visitVarStmt(varStmt: Var): R

    def visitIfStmt(ifStmt: If): R

    def visitWhileStmt(stmt: While): R

    def visitBreakStmt(stmt: Break): R

    def visitFunctionStmt(stmt: Function): R

    def visitReturnStmt(stmt: Return): R
  }

  case class Break() extends Stmt {
    override def accept[R](visitor: Stmt.Visitor[R]): R = visitor.visitBreakStmt(this)
  }

  case class While(condition: Expr, body: Stmt) extends Stmt {
    override def accept[R](visitor: Stmt.Visitor[R]): R = visitor.visitWhileStmt(this)
  }

  case class If(condition: Expr, thenBranch: Stmt, elseBranch: Stmt) extends Stmt {
    override def accept[R](visitor: Stmt.Visitor[R]): R = visitor.visitIfStmt(this)
  }

  case class Print(expr: Expr) extends Stmt {
    override def accept[R](visitor: Stmt.Visitor[R]): R = visitor.visitPrintStmt(this)
  }

  case class Var(name: Token, initializer: Option[Expr]) extends Stmt {
    override def accept[R](visitor: Stmt.Visitor[R]): R = visitor.visitVarStmt(this)
  }

  case class Expression(expr: Expr) extends Stmt {
    override def accept[R](visitor: Stmt.Visitor[R]): R = visitor.visitExpressionStmt(this)
  }

  case class Block(stmts: List[Stmt]) extends Stmt {
    override def accept[R](visitor: Stmt.Visitor[R]): R = visitor.visitBlockStmt(this)
  }

  case class Function(name: Token, params: List[Token], body: List[Stmt]) extends Stmt with ParsedFunction {
    override def accept[R](visitor: Stmt.Visitor[R]): R = visitor.visitFunctionStmt(this)

    override def fName: String = name.lexeme
  }

  case class Return(keyword: Token, value: Expr) extends Stmt {
    override def accept[R](visitor: Stmt.Visitor[R]): R = visitor.visitReturnStmt(this)
  }

  case class Class(name: Token, superclass: Option[Expr.Variable], methods: List[Stmt.Function]) extends Stmt {
    override def accept[R](visitor: Visitor[R]): R = visitor.visitClassStmt(this)
  }
}



