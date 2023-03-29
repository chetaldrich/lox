package org.lox.parser

import org.lox.Lox
import org.lox.lexer.TokenType._
import org.lox.lexer.{Token, TokenType}
import org.lox.parser.Parser.ParseError

import scala.annotation.unused
import scala.collection.mutable.ListBuffer
import scala.util.Try
import scala.util.control.Breaks.{break, breakable}

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
      else if (`match`(Fun)) function("function")
      else statement
    } catch {
      case _: ParseError =>
        synchronize()
        null
    }
  }

  private def function(kind: String): Stmt = {
    if (check(LeftParen)) {
      // This situation is when you have an anonymous function that doesn't have a name.
      // Backtrack so you can get the `fun` keyword and attempt to parse as an expression statement.
      current -= 1
      return expressionStatement
    }
    val name: Token = consume(Identifier, s"Expect $kind name.")
    consume(LeftParen, s"Expect '(' after $kind name.")
    val args = arguments(() => consume(Identifier, "Expect parameter name."))
    consume(RightParen, "Expect ')' after arguments.")
    consume(LeftBrace, s"Expect '{' before $kind body.")
    val body = block
    FunctionStmt(name, args, body)
  }

  private def varDeclaration: Stmt = {
    val name: Token = consume(Identifier, "Expected variable name.")
    val initializer: Option[Expr] = if (`match`(Equal)) Some(expression) else None
    consume(Semicolon, "Expected ';' after variable declaration.")
    VarStmt(name, initializer)
  }

  private def statement: Stmt = {
    if (`match`(Print)) printStatement
    else if (`match`(Return)) returnStatement
    else if (`match`(If)) ifStatement
    else if (`match`(For)) forStatement
    else if (`match`(While)) whileStatement
    else if (`match`(LeftBrace)) BlockStmt(block)
    else if (`match`(Break)) breakStatement
    else expressionStatement
  }

  private def returnStatement: Stmt = {
    val keyword = previous
    val value = if (!check(Semicolon)) expression else null
    consume(Semicolon, "Expect ';' after return value.")
    ReturnStmt(keyword, value)
  }

  private def block: List[Stmt] = {
    val statements: ListBuffer[Stmt] = ListBuffer()
    while (!check(RightBrace) && !isAtEnd) {
      statements += declaration
    }
    consume(RightBrace, "Expected '}' after block.")
    statements.toList
  }

  private def breakStatement: Stmt = {
    consume(Semicolon, "Expect ';' after 'break'.")
    BreakStmt()
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
        case None => WhileStmt(Expr.Literal(true), body)
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
        case variable: Expr.Variable => Expr.Assign(variable.name, value)
        case _ => throw error(equals, "Invalid assignment target.")
      }
    } else expr
  }

  private def or: Expr = matchWhile(and, Or)(Expr.Logical)


  private def and: Expr = matchWhile(ternary, And)(Expr.Logical)


  private def ternary: Expr = {
    val expr = equality
    if (`match`(QuestionMark)) {
      val `then` = equality
      consume(Colon, "Expected colon after ? in ternary expression.")
      val otherwise = equality
      Expr.Ternary(expr, `then`, otherwise)
    } else expr
  }

  private def equality: Expr = matchWhile(comparison, BangEqual, EqualEqual)()

  private def comparison: Expr = matchWhile(term, Greater, GreaterEqual, Less, LessEqual)()

  private def term: Expr = matchWhile(factor, Plus, Minus)()

  private def factor: Expr = matchWhile(unary, Slash, Star)()

  private def unary: Expr = if (`match`(Bang, Minus)) Expr.Unary(previous, unary) else call

  private def call: Expr = {
    var expr = primary
    breakable {
      while (true) {
        if (`match`(LeftParen)) {
          expr = finishCall(expr)
        } else {
          break
        }
      }
    }
    expr
  }

  private def arguments[T](fn: () => T): List[T] = {
    val arguments: ListBuffer[T] = ListBuffer()
    if (!check(RightParen)) {
      do {
        if (arguments.size >= 255) {
          error(peek, "Can't have more than 255 arguments.")
        }
        arguments.addOne(fn.apply())
      } while (`match`(Comma))
    }
    arguments.toList
  }

  private def finishCall(callee: Expr) = {
    val args = arguments(() => expression)
    val paren = consume(RightParen, "Expect ')' after arguments")
    Expr.Call(callee, paren, args)
  }

  private def primary: Expr = {
    if (`match`(False)) Expr.Literal(false)
    else if (`match`(True)) Expr.Literal(true)
    else if (`match`(Fun)) lambda
    else if (`match`(Nil)) Expr.Literal(null)
    else if (`match`(Number, String)) Expr.Literal(previous.literal)
    else if (`match`(Identifier)) Expr.Variable(previous)
    else if (`match`(LeftParen)) {
      val expr = expression
      consume(RightParen, "expected ')' after expression")
      Expr.Grouping(expr)
    } else throw error(peek, "Expected expression.")
  }

  private def lambda: Expr = {
    consume(LeftParen, s"Expect '(' after anonymous function declaration.")
    val params = arguments(() => consume(Identifier, "Expect parameter name."))
    consume(RightParen, "Expect ')' after arguments.")
    consume(LeftBrace, s"Expect '{' before anonymous function body.")
    val body = block
    Expr.Lambda(params, body)
  }

  def consume(`type`: TokenType, message: String): Token = {
    if (check(`type`)) {
      advance
    } else throw error(peek, message)
  }

  private def matchWhile(rule: => Expr, tokens: TokenType*)(toExpr: (Expr, Token, Expr) => Expr = Expr.Binary): Expr = {
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
