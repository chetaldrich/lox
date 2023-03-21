package org.lox.runtime

class BreakError extends RuntimeException {
  val message: String = "Break found outside loop"

  override def getMessage: String = message
}
