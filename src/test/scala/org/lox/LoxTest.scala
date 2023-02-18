package org.lox

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class LoxTest extends AnyFlatSpec with should.Matchers {
  it should "print 2 when given 1 + 1" in {
    val output = Lox.run("1 + 1")
    output should be("2")
  }

  it should "print false when given 1 > 2" in {
    val output = Lox.run("1 > 2")
    output should be("false")
  }

  it should "add strings together when given them" in {
    val output = Lox.run("\"hello\" + \" world\"")
    output should be("hello world")
  }
}
