package org.lox.runtime

import org.lox.lexer.Token

case class RuntimeError(token: Token, message: String) extends RuntimeException {
  override def getMessage: String = message + "\n[line " + token.line + "]"
}
