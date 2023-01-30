package org.lox

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class LoxTest extends AnyFlatSpec with should.Matchers {
  it should "print an AST when given an addition expression" in {
    val output = Lox.run("1 + 1")
    output should be("(+ 1.0 1.0)")
  }
}
