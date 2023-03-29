package org.lox.parser

import org.lox.lexer.Token

object Expr {
  trait Visitor[R] {

    def visitLogicalExpr(expr: Logical): R

    def visitAssignmentExpression(assign: Assign): R

    def visitTernaryExpr(expr: Ternary): R

    def visitBinaryExpr(expr: Binary): R

    def visitGroupingExpr(expr: Grouping): R

    def visitLiteralExpr(expr: Literal): R

    def visitUnaryExpr(expr: Unary): R

    def visitVarExpr(expr: Variable): R

    def visitCallExpr(expr: Call): R

    def visitLambdaExpr(expr: Lambda): R
  }


  case class Call(callee: Expr, paren: Token, arguments: List[Expr]) extends Expr {
    override def accept[R](visitor: Expr.Visitor[R]): R = visitor.visitCallExpr(this)
  }

  case class Logical(left: Expr, operator: Token, right: Expr) extends Expr {
    override def accept[R](visitor: Expr.Visitor[R]): R = visitor.visitLogicalExpr(this)
  }

  case class Ternary(condition: Expr, `then`: Expr, otherwise: Expr) extends Expr {
    def accept[R](visitor: Expr.Visitor[R]): R = visitor.visitTernaryExpr(this)
  }

  case class Binary(left: Expr, operator: Token, right: Expr) extends Expr {
    def accept[R](visitor: Expr.Visitor[R]): R = visitor.visitBinaryExpr(this)
  }

  case class Grouping(expression: Expr) extends Expr {
    def accept[R](visitor: Expr.Visitor[R]): R = visitor.visitGroupingExpr(this)
  }

  case class Literal(value: Any) extends Expr {
    def accept[R](visitor: Expr.Visitor[R]): R = visitor.visitLiteralExpr(this)
  }

  case class Unary(operator: Token, right: Expr) extends Expr {
    def accept[R](visitor: Expr.Visitor[R]): R = visitor.visitUnaryExpr(this)
  }

  case class Variable(name: Token) extends Expr {
    override def accept[R](visitor: Expr.Visitor[R]): R = visitor.visitVarExpr(this)
  }

  case class Assign(name: Token, value: Expr) extends Expr {
    override def accept[R](visitor: Expr.Visitor[R]): R = visitor.visitAssignmentExpression(this)
  }

  case class Lambda(params: List[Token], body: List[Stmt]) extends Expr with ParsedFunction {
    override def accept[R](visitor: Expr.Visitor[R]): R = visitor.visitLambdaExpr(this)

    override def fName: String = "anon_function"
  }
}

sealed trait Expr {
  def accept[R](visitor: Expr.Visitor[R]): R
}

