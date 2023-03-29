package org.lox

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.io.Source

class LoxTest extends AnyFlatSpec with should.Matchers {

  def testLoxFile(fileName: String, expectedOutput: String, expectedErrorOutput: String = ""): Unit = {
    val output = new java.io.ByteArrayOutputStream()
    val errorOutput = new java.io.ByteArrayOutputStream()
    Console.withOut(output) {
      Console.withErr(errorOutput) {
        val in = Source.fromFile(fileName)
        Lox.run(in.getLines().mkString("\n"))
        in.close()
      }
    }
    output.toString.trim should be(expectedOutput)
    errorOutput.toString.trim should be(expectedErrorOutput)
  }

  def testLoxProgram(program: String, expectedOutput: String, expectedErrorOutput: String = ""): Unit = {
    val out = new java.io.ByteArrayOutputStream()
    val err = new java.io.ByteArrayOutputStream()
    Console.withOut(out) {
      Console.withErr(err) {
        Lox.run(program)
      }
    }
    out.toString.trim should be(expectedOutput.trim)
    err.toString.trim should be(expectedErrorOutput.trim)
  }

  it should "print 2 when given 1 + 1" in {
    testLoxProgram("print 1 + 1;", "2")
  }

  it should "print false when given 1 > 2" in {
    testLoxProgram("print 1 > 2;", "false")
  }

  it should "add strings together when given them" in {
    testLoxProgram("print \"hello\" + \" world\";", "hello world")
  }

  it should "run the repl on a full print statement without logging expression errors" in {
    testLoxProgram("print 1 + 1;", "2", "")
  }

  it should "correctly execute the blocks.lox example" in {
    testLoxFile("example_files/blocks.lox", "outer a\nouter b\nglobal c\nglobal a\nglobal b\nglobal c")
  }

  it should "correctly execute the break.lox example" in {
    testLoxFile("example_files/break.lox", "hello before break")
  }

  it should "correctly execute the counter.lox example" in {
    testLoxFile("example_files/counter.lox", "1\n2")
  }

  it should "correctly execute the empty.lox example" in {
    testLoxFile("example_files/empty.lox", "")
  }

  it should "correctly execute the equality.lox example" in {
    testLoxFile("example_files/equality.lox", "false")
  }

  it should "correctly execute the fib.lox example" in {
    testLoxFile("example_files/fib.lox", "0\n1\n1\n2\n3\n5\n8\n13\n21\n34\n55\n89\n144\n233\n377\n610\n987\n1597\n2584\n4181\n6765")
  }

  it should "correctly execute the fib_recurse.lox example" in {
    testLoxFile("example_files/fib_recurse.lox", "0\n1\n1\n2\n3")
  }

  it should "correctly execute the test_issue.lox example" in {
    testLoxFile("example_files/test_issue.lox", "0\n2")
  }

  it should "correctly execute the anon_functions.lox example" in {
    testLoxFile("example_files/anon_functions.lox", "1\n2\n3")
  }

  it should "correctly execute the anon_function_expression.lox example" in {
    testLoxFile("example_files/anon_function_expression.lox", "")
  }
}
