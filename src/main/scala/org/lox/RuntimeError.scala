package org.lox

class RuntimeError(val token: Token, val message: String) extends RuntimeException {
  override def getMessage: String = message + "\n[line " + token.line + "]"
}
