package org.lox.parser

import org.lox.TokenType._
import org.lox.parser.Parser.ParseError
import org.lox.{Lox, Token, TokenType}

import scala.annotation.unused
import scala.util.Try

object Parser {
  case class ParseError() extends RuntimeException
}

class Parser(tokens: Seq[Token], private var current: Int = 0) {

  def parse: Try[Expr] = Try(expression)

  private def expression: Expr = ternary

  private def ternary: Expr = {
    val expr = equality
    if (`match`(QuestionMark)) {
      val `then` = equality
      if (`match`(Colon)) {
        val otherwise = equality
        Ternary(expr, `then`, otherwise)
      } else expr
    } else expr
  }

  private def equality: Expr = matchWhile(comparison, BangEqual, EqualEqual)

  private def comparison: Expr = matchWhile(term, Greater, GreaterEqual, Less, LessEqual)

  private def term: Expr = matchWhile(factor, Plus, Minus)

  private def factor: Expr = matchWhile(unary, Slash, Star)

  private def unary: Expr = if (`match`(Bang, Minus)) Unary(previous, unary) else primary

  private def primary: Expr = {
    if (`match`(False)) return Literal(false)
    if (`match`(True)) return Literal(true)
    if (`match`(Nil)) return Literal(null)

    if (`match`(Number, String)) return Literal(previous.literal)

    if (`match`(LeftParen)) {
      val expr = expression
      consume(RightParen, "expected ')' after expression")
      return Grouping(expr)
    }

    throw error(peek, "Expected expression.")
  }

  def consume(`type`: TokenType, message: String): Token = {
    if (check(`type`)) {
      advance
    } else throw error(peek, message)
  }

  private def matchWhile(rule: => Expr, tokens: TokenType*): Expr = {
    var expr = rule
    while (`match`(tokens: _*)) {
      expr = Binary(expr, previous, rule)
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
    Lox.error(token, message)
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
