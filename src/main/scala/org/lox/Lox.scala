package org.lox

import org.lox.TokenType.EOF
import org.lox.parser.{Expr, Parser, Stmt}

import scala.io.Source.fromFile
import scala.io.StdIn.readLine
import scala.util.{Failure, Success, Try}

object Lox {
  val interpreter = new Interpreter

  def main(args: Array[String]): Unit = args match {
    case Array(fileName) => runFile(fileName)
    case Array(_, _*) => println(s"Usage: slox [script]")
    case _ => runPrompt()
  }

  private def runFile(path: String): Unit = {
    val source = fromFile(path)
    run(try source.mkString finally source.close())
  }

  private def runPrompt(): Unit = {
    var continue = true
    while (continue) {
      print("slox> ")
      val line = readLine()
      if (line == null) continue = false
      else run(line, shouldError = false)
    }
  }

  def run(source: String, shouldError: Boolean = true): Option[Unit] = for {
    tokens <- handle(Scanner(source).apply, 65, shouldError)
    statements <- handle(new Parser(tokens).parse, 65, shouldError)
    result <- handle(interpreter.interpret(statements), 65, shouldError)
  } yield result

  def error(line: Int, message: String): Unit = report(line, "", message)

  def error(token: Token, message: String): Unit = token.tokenType match {
    case EOF => report(token.line, " at end", message)
    case _ => report(token.line, s" at '${token.lexeme}'", message)
  }

  def handle[T](result: Try[T], code: Int, shouldExit: Boolean): Option[T] = result match {
    case Success(value) => Some(value)
    case Failure(error) =>
      System.err.println(error.getMessage)
      if (shouldExit) System.exit(code)
      None
  }

  private def report(line: Int, where: String, message: String): Unit = {
    System.err.println("[line " + line + "] Error" + where + ": " + message)
  }
}
