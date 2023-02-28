package org.lox.runtime

import org.lox.RuntimeError
import org.lox.lexer.Token

import scala.collection.mutable
import scala.util.Try

class Environment {
  private val values: mutable.Map[String, Any] = mutable.HashMap()
  def define(name: Token, value: Option[Any]): Unit = values.put(name.lexeme, value.orNull)
  def get(name: Token): Try[Any] = Try {
    values.getOrElse(name.lexeme, throw new RuntimeError(name, s"Undefined variable '${name.lexeme}'."))
  }
}
