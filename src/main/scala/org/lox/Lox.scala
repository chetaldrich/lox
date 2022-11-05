package org.lox

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

  private def run(source: String, shouldError: Boolean = true): Unit = {
    val tokens = source.split("\\s+")
    tokens.foreach(println)
    if (hadError && shouldError) System.exit(65)
  }

  private def error(line: Int, message: String): Unit = {
    report(line, "", message)
  }

  private def report(line: Int, where: String, message: String): Unit = {
    System.err.println("[line " + line + "] Error" + where + ": " + message)
    hadError = true
  }
}