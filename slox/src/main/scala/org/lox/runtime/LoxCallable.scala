package org.lox.runtime

trait LoxCallable {
  def arity: Int
  def call(interpreter: Interpreter, arguments: List[Any]): Any
}
