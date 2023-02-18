package org.lox

import org.lox.TokenType.EOF
import org.lox.parser.{Expr, Parser}

import scala.io.Source.fromFile
import scala.io.StdIn.readLine
import scala.util.{Failure, Success, Try}

object Lox {
  var hadError = false
  val interpreter = new Interpreter

  def main(args: Array[String]): Unit = {
    args match {
      case Array(fileName) => runFile(fileName)
      case Array(_, _*) => println(s"Usage: slox [script]")
      case _ => runPrompt()
    }
  }

  private def runFile(path: String): Unit = {
    val source = fromFile(path)
    println(run(try source.mkString finally source.close()))
  }

  private def runPrompt(): Unit = {
    var continue = true
    while (continue) {
      print("slox> ")
      val line = readLine()
      if (line == null) continue = false
      else println(run(line, shouldError = false))
    }
  }

  def run(source: String, shouldError: Boolean = true): String = {
    val tokens: Seq[Token] = Scanner(source).apply
    val expression: Expr = new Parser(tokens).parse
    if (hadError && shouldError) System.exit(65)
    val result = interpreter.interpret(expression)
    handle(result)
  }

  def error(line: Int, message: String): Unit = {
    report(line, "", message)
  }

  def error(token: Token, message: String): Unit = token.tokenType match {
    case EOF => report(token.line, " at end", message)
    case _ => report(token.line, s" at '${token.lexeme}'", message)
  }

  def handle(result: Try[String]): String = result match {
    case Success(value) => value
    case Failure(error) =>
      System.err.println(error.getMessage)
      ""
  }

  private def report(line: Int, where: String, message: String): Unit = {
    System.err.println("[line " + line + "] Error" + where + ": " + message)
    hadError = true
  }
}
