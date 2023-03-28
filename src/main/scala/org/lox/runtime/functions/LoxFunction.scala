package org.lox.runtime.functions

import org.lox.parser.FunctionStmt
import org.lox.runtime.{Environment, Interpreter, LoxCallable}

case class LoxFunction(declaration: FunctionStmt) extends LoxCallable {
  override def arity: Int = declaration.params.size

  override def call(interpreter: Interpreter, arguments: List[Any]): Any = {
    val environment = new Environment(Some(interpreter.globals))
    declaration.params.zip(arguments).foreach { case (parameter, argument) =>
      environment.define(parameter, Some(argument))
    }
    interpreter.executeBlock(declaration.body, environment)
  }

  override def toString: String = s"<fn ${declaration.name.lexeme}>"
}
