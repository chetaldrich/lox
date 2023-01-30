package org.lox

import org.lox.parser.{AstPrinter, Expr, Parser}
import org.lox.TokenType.EOF

import scala.io.StdIn.readLine
import scala.io.Source.fromFile

object Lox {
  var hadError = false

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
    new AstPrinter().print(expression)
  }

  def error(line: Int, message: String): Unit = {
    report(line, "", message)
  }

  def error(token: Token, message: String): Unit = token.tokenType match {
    case EOF => report(token.line, " at end", message)
    case _ => report(token.line, s" at '${token.lexeme}'", message)
  }

  private def report(line: Int, where: String, message: String): Unit = {
    System.err.println("[line " + line + "] Error" + where + ": " + message)
    hadError = true
  }
}
