package org.lox.parser

import org.lox.lexer.{Token, TokenType}
import org.lox.lexer.TokenType._
import org.lox.parser.Parser.ParseError
import org.lox.Lox

import scala.annotation.unused
import scala.collection.mutable.ListBuffer
import scala.util.Try

object Parser {
  case class ParseError() extends RuntimeException
}

class Parser(tokens: Seq[Token], private var current: Int = 0, val shouldLog: Boolean = true) {

  def parse: Try[List[Stmt]] = Try {
    val statements: ListBuffer[Stmt] = ListBuffer()
    while (!isAtEnd) {
      statements += declaration
    }
    statements.toList
  }

  def parseExpression: Try[Expr] = Try(expression)

  private def declaration: Stmt = {
    try {
      if (`match`(Var)) varDeclaration
      else statement
    } catch {
      case _: ParseError =>
        synchronize()
        null
    }
  }

  private def varDeclaration: Stmt = {
    val name: Token = consume(Identifier, "Expected variable name.")
    val initializer: Option[Expr] = if (`match`(Equal)) Some(expression) else None
    consume(Semicolon, "Expected ';' after variable declaration.")
    VarStmt(name, initializer)
  }

  private def statement: Stmt = {
    if (`match`(Print)) printStatement
    else if (`match`(If)) ifStatement
    else if (`match`(For)) forStatement
    else if (`match`(While)) whileStatement
    else if (`match`(LeftBrace)) BlockStmt(block)
    else expressionStatement
  }

  private def block: List[Stmt] = {
    val statements: ListBuffer[Stmt] = ListBuffer()
    while (!check(RightBrace) && !isAtEnd) {
      statements += declaration
    }
    consume(RightBrace, "Expected '}' after block.")
    statements.toList
  }

  private def forStatement: Stmt = {
    consume(LeftParen, "Expect '(' after 'for'.")

    val initializer = if (`match`(Semicolon)) None
    else if (`match`(Var)) Some(varDeclaration)
    else Some(expressionStatement)

    val condition: Option[Expr] = if (!check(Semicolon)) Some(expression) else None
    consume(Semicolon, "Expect ';' after loop condition.")

    val increment = if (!check(RightParen)) Some(expression) else None
    consume(RightParen, "Expect ')' after after for clauses.")

    val initialBody = Some(statement)

    initialBody.map { (body: Stmt) =>
      increment match {
        case Some(i) => BlockStmt(List(body, ExpressionStmt(i)))
        case None => body
      }
    }.map { body =>
      condition match {
        case Some(c) => WhileStmt(c, body)
        case None => WhileStmt(Literal(true), body)
      }
    }.map { body =>
      initializer match {
        case Some(i) => BlockStmt(List(i, body))
        case None => body
      }
    }.get
  }

  private def whileStatement: Stmt = {
    consume(LeftParen, "Expect '(' after 'while'.")
    val condition = expression
    consume(RightParen, "Expect ')' after condition.")
    val body = statement
    WhileStmt(condition, body)
  }

  private def ifStatement: Stmt = {
    consume(LeftParen, "Expect '(' after 'if'")
    val condition = expression
    consume(RightParen, "Expect ')' after 'if'")
    val thenBranch = statement
    val elseBranch = if (`match`(Else)) statement else null
    IfStmt(condition, thenBranch = thenBranch, elseBranch = elseBranch)
  }

  private def printStatement: Stmt = {
    val expr = expression
    consume(Semicolon, "Expect ';' after value")
    PrintStmt(expr)
  }

  private def expressionStatement: Stmt = {
    val expr = expression
    consume(Semicolon, "Expect ';' after expression")
    ExpressionStmt(expr)
  }

  private def expression: Expr = assignment

  private def assignment: Expr = {
    val expr = or

    if (`match`(Equal)) {
      val equals = previous
      val value = assignment

      expr match {
        case variable: Variable => Assign(variable.name, value)
        case _ => throw error(equals, "Invalid assignment target.")
      }
    } else expr
  }

  private def or: Expr = matchWhile(and, Or)(Logical)


  private def and: Expr = matchWhile(ternary, And)(Logical)


  private def ternary: Expr = {
    val expr = equality
    if (`match`(QuestionMark)) {
      val `then` = equality
      consume(Colon, "Expected colon after ? in ternary expression.")
      val otherwise = equality
      Ternary(expr, `then`, otherwise)
    } else expr
  }

  private def equality: Expr = matchWhile(comparison, BangEqual, EqualEqual)()

  private def comparison: Expr = matchWhile(term, Greater, GreaterEqual, Less, LessEqual)()

  private def term: Expr = matchWhile(factor, Plus, Minus)()

  private def factor: Expr = matchWhile(unary, Slash, Star)()

  private def unary: Expr = if (`match`(Bang, Minus)) Unary(previous, unary) else primary

  private def primary: Expr = {
    if (`match`(False)) Literal(false)
    else if (`match`(True)) Literal(true)
    else if (`match`(Nil)) Literal(null)
    else if (`match`(Number, String)) Literal(previous.literal)
    else if (`match`(Identifier)) Variable(previous)
    else if (`match`(LeftParen)) {
      val expr = expression
      consume(RightParen, "expected ')' after expression")
      Grouping(expr)
    } else throw error(peek, "Expected expression.")
  }

  def consume(`type`: TokenType, message: String): Token = {
    if (check(`type`)) {
      advance
    } else throw error(peek, message)
  }

  private def matchWhile(rule: => Expr, tokens: TokenType*)(toExpr: (Expr, Token, Expr) => Expr = Binary): Expr = {
    var expr = rule
    while (`match`(tokens: _*)) {
      expr = toExpr(expr, previous, rule)
    }
    expr
  }

  private def `match`(types: TokenType*): Boolean = {
    if (types.exists(check)) {
      advance
      true
    } else false
  }

  private def advance: Token = {
    if (!isAtEnd) {
      current += 1
    }
    previous
  }

  // maybe needs EOF check, who knows
  private def check(`type`: TokenType): Boolean = {
    if (isAtEnd) return false
    peek.tokenType == `type`
  }

  private def isAtEnd: Boolean = peek.tokenType == EOF

  private def peek: Token = tokens(current)

  private def previous = tokens(current - 1)

  private def error(token: Token, message: String): ParseError = {
    if (shouldLog) {
      Lox.error(token, message)
    }
    ParseError()
  }

  @unused
  private def synchronize(): Unit = {
    advance
    while (!isAtEnd) {
      if (previous.tokenType == Semicolon) return
      peek.tokenType match {
        case Class | Fun | Var | For | If | While | Print | Return => return
        case _ => advance
      }
    }
  }
}
