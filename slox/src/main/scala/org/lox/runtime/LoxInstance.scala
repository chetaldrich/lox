package org.lox.runtime

import org.lox.lexer.Token
import scala.collection.mutable

case class LoxInstance(klass: LoxClass) {

  private val fields: mutable.Map[String, Any] = mutable.Map()

  def set(name: Token, value: Any): Unit = fields.put(name.lexeme, value)

  def get(name: Token): Any = {
    if (fields.contains(name.lexeme)) {
      return fields.get(name.lexeme)
    }

    val method = klass.findMethod(name.lexeme)
    if (method != null) return method.bind(this)

    throw RuntimeError(name, s"Undefined property '${name.lexeme}'.'")
  }

  override def toString: String = klass.name + " instance"
}
