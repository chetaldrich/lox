package org.lox

import java.io.IOException
import java.nio.charset.Charset
import java.nio.file.Files
import java.nio.file.Paths

object Main {
  def main(args: Array[String]): Unit = {
    args match {
      case Array(fileName) => runFile(fileName)
      case Array(_, _*) => println(s"Usage: slox [script]")
      case _ => println("slox>")
    }
  }

  @throws[IOException]
  private def runFile(path: String): Unit = {
    val bytes = Files.readAllBytes(Paths.get(path))
    run(new String(bytes, Charset.defaultCharset))
  }

  private def run(source: String): Unit = {
    println(source)
  }
}