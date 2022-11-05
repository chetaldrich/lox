package org.lox

import scala.io.StdIn.readLine
import scala.io.Source.fromFile

object Main {
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
      else run(line)
    }
  }

  private def run(source: String): Unit = {
    println(source)
  }
}