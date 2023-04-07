package org.lox.resolver

sealed trait FunctionType

object FunctionType {
  case object None extends FunctionType
  case object Function extends FunctionType
}
