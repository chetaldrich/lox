package org.lox

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.io.Source

class LoxTest extends AnyFlatSpec with should.Matchers {
  it should "print 2 when given 1 + 1" in {
    val output = new java.io.ByteArrayOutputStream()
    Console.withOut(output) {
      Lox.run("print 1 + 1;")
    }
    output.toString.trim should be("2")
  }

  it should "not modify things done in function calls" in {
    val output = new java.io.ByteArrayOutputStream()
    Console.withOut(output) {
      val in = Source.fromFile("example_files/test_issue.lox")
      Lox.run(in.getLines().mkString("\n"))
      in.close()
    }
    output.toString.trim should be(List(0, 2).mkString("\n"))
  }

  it should "print the first 5 fibonacci numbers" in {
    val output = new java.io.ByteArrayOutputStream()
    Console.withOut(output) {
      val in = Source.fromFile("example_files/fib_recurse.lox")
      Lox.run(in.getLines().mkString("\n"))
      in.close()
    }
    output.toString.trim should be(List(0, 1, 1, 2, 3).mkString("\n"))
  }

  it should "run the repl on a full print statement without logging expression errors" in {
    val errOutput = new java.io.ByteArrayOutputStream()
    Console.withErr(errOutput) {
      Lox.runRepl("print 1 + 1;")
    }
    errOutput.toString should be("")
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
