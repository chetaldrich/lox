package org.lox.runtime.functions

import org.lox.parser.ParsedFunction
import org.lox.runtime.{Environment, FunctionReturn, Interpreter, LoxCallable}

import scala.util.Try

case class LoxFunction(declaration: ParsedFunction, closure: Environment) extends LoxCallable {

  override def arity: Int = declaration.params.size

  override def call(interpreter: Interpreter, arguments: List[Any]): Any = {
    val environment = new Environment(Some(closure))
    declaration.params.zip(arguments).foreach { case (parameter, argument) =>
      environment.define(parameter, Some(argument))
    }
    Try(interpreter.executeBlock(declaration.body, environment))
      .recover { case r: FunctionReturn => r.value }
      .getOrElse(null)
  }

  override def toString: String = s"<fn ${declaration.fName}>"
}
