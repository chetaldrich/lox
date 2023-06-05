package org.lox.resolver

sealed trait ClassType

object ClassType {
  case object None extends ClassType
  case object Class extends ClassType

  case object SubClass extends ClassType
}

