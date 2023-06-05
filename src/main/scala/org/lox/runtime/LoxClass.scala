package org.lox.runtime

import org.lox.runtime.functions.LoxFunction

case class LoxClass(name: String, superclass: LoxClass, methods: Map[String, LoxFunction]) extends LoxCallable {
  def findMethod(name: String): LoxFunction = {
    val method = methods.getOrElse(name, null)
    if (method != null) {
      return method
    }

    if (superclass != null) superclass.findMethod(name)
    else null
  }

  override def toString: String = name

  override def arity: Int = {
    val initializer = findMethod("init")
    if (initializer == null) 0
    else initializer.arity
  }

  override def call(interpreter: Interpreter, arguments: List[Any]): Any = {
    val instance = LoxInstance(this)
    val initializer = findMethod("init")
    if (initializer != null) {
      initializer.bind(instance).call(interpreter, arguments)
    }

    instance
  }
}
