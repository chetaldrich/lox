package org.lox.runtime

import org.lox.lexer.Token

class RuntimeError(val token: Token, val message: String) extends RuntimeException {
  override def getMessage: String = message + "\n[line " + token.line + "]"
}
