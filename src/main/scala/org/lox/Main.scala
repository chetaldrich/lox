package org.lox

object Main {
  def main(args: Array[String]): Unit = {
    args match {
      case Array(_, _*) => println(s"Usage: slox [script]")
      case Array(fileName) => println(s"Running file $fileName!")
      case _ => println("slox>")
    }
  }
}