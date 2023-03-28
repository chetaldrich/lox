package org.lox.runtime.functions.builtins

import org.lox.runtime.{Interpreter, LoxCallable}

case class Clock() extends LoxCallable {
  override def arity: Int = 0

  override def call(interpreter: Interpreter, arguments: List[Any]): Any = System.currentTimeMillis() / 1000.0

  override def toString: String = "<native fn>"
}
