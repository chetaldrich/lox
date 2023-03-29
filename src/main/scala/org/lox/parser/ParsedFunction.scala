package org.lox.parser

import org.lox.lexer.Token

trait ParsedFunction {
  def fName: String
  val params: List[Token]
  val body: List[Stmt]
}
