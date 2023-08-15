package org.lox.runtime.functions

import org.lox.parser.ParsedFunction
import org.lox.runtime.{Environment, FunctionReturn, Interpreter, LoxCallable, LoxInstance}

import scala.util.Try

case class LoxFunction(declaration: ParsedFunction, closure: Environment, isInitializer: Boolean) extends LoxCallable {


  def bind(instance: LoxInstance): LoxFunction = {
    val environment = new Environment(Some(closure))
    environment.defineStr("this", Some(instance))
    LoxFunction(declaration, environment, isInitializer)
  }

  override def arity: Int = declaration.params.size

  override def call(interpreter: Interpreter, arguments: List[Any]): Any = {
    val environment = new Environment(Some(closure))
    declaration.params.zip(arguments).foreach { case (parameter, argument) =>
      environment.define(parameter, Some(argument))
    }
    Try(interpreter.executeBlock(declaration.body, environment))
      .recover { case r: FunctionReturn =>
        if (isInitializer) closure.getAtStr(0, "this")
        else r.value
      }
      .getOrElse(null)

    if (isInitializer) return closure.getAtStr(0, "this")
  }

  override def toString: String = s"<fn ${declaration.fName}>"
}
