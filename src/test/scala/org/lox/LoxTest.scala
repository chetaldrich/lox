package org.lox

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class LoxTest extends AnyFlatSpec with should.Matchers {
  it should "print 2 when given 1 + 1" in {
    val output = new java.io.ByteArrayOutputStream()
    Console.withOut(output) {
      Lox.run("print 1 + 1;")
    }
    output.toString.trim should be("2")

  }

  it should "print false when given 1 > 2" in {
    val output = new java.io.ByteArrayOutputStream()
    Console.withOut(output) {
      Lox.run("print 1 > 2;")
    }
    output.toString.trim should be("false")
  }

  it should "add strings together when given them" in {
    val output = new java.io.ByteArrayOutputStream()
    Console.withOut(output) {
      Lox.run("print \"hello\" + \" world\";")
    }
    output.toString.trim should be("hello world")
  }
}
