package org.lox.runtime

import org.lox.lexer.Token
import org.lox.lexer.TokenType.Identifier
import org.scalactic.TimesOnInt.convertIntToRepeater

import scala.collection.mutable
import scala.util.Try

class Environment(private val enclosing: Option[Environment] = None) {

  private val values: mutable.Map[String, Any] = mutable.HashMap()

  def assign(name: Token, value: Any): Any = {
    val containsValue = values.contains(name.lexeme)
    (containsValue, enclosing) match {
      case (true, _) => values.put(name.lexeme, value)
      case (false, Some(env)) => env.assign(name, value)
      case _ => throw RuntimeError(name, s"Undefined variable '${name.lexeme}' referenced before declaration")
    }
  }

  def define(name: Token, value: Option[Any]): Unit = values.put(name.lexeme, value.orNull)

  def defineStr(name: String, value: Option[Any]): Unit = values.put(name, value.orNull)

  def defineGlobal(name: String, value: Option[Any]): Unit = values.put(name, value.orNull)

  def get(name: Token): Try[Any] = Try {
    val value = values.get(name.lexeme)
    (value, enclosing) match {
      case (Some(v), _) => v
      case (None, Some(enclosingEnv)) => enclosingEnv.get(name).get
      case _ => throw RuntimeError(name, s"Undefined variable '${name.lexeme}'.")
    }
  }

  def assignAt(distance: Int, name: Token, value: Any): Unit = ancestor(distance).values.put(name.lexeme, value)

  def getAt(distance: Int, name: Token): Any = ancestor(distance).values.get(name.lexeme)

  def getAtStr(distance: Int, name: String): Any = ancestor(distance).values.get(name)

  private def ancestor(distance: Int): Environment = {
    var environment = this
    distance.times {
      environment = environment.enclosing.get
    }
    environment
  }

  // used for debugging
  def getKey(key: String): Any = get(Token(Identifier, "key"))
}
