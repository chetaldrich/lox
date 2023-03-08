package org.lox.runtime

import org.lox.RuntimeError
import org.lox.lexer.Token

import scala.collection.mutable
import scala.util.Try

class Environment(private val enclosing: Option[Environment] = None) {
  private val values: mutable.Map[String, Any] = mutable.HashMap()

  def assign(name: Token, value: Any): Any = {
    val containsValue = values.contains(name.lexeme)
    (containsValue, enclosing) match {
      case (true, _) => values.put(name.lexeme, value)
      case (false, Some(env)) => env.assign(name, value)
      case _ => throw new RuntimeError(name, s"Undefined variable '${name.lexeme}' referenced before declaration")
    }
  }

  def define(name: Token, value: Option[Any]): Unit = values.put(name.lexeme, value.orNull)

  def get(name: Token): Try[Any] = Try {
    val value = values.get(name.lexeme)
    (value, enclosing) match {
      case (Some(v), _) => v
      case (None, Some(enclosingEnv)) => enclosingEnv.get(name).get
      case _ => throw new RuntimeError(name, s"Undefined variable '${name.lexeme}'.")
    }
  }
}
