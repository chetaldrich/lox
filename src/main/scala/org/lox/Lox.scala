package org.lox

import org.jline.reader.impl.DefaultParser
import org.jline.reader.{LineReader, LineReaderBuilder, UserInterruptException}
import org.jline.terminal.Terminal.Signal
import org.jline.terminal.{Size, TerminalBuilder}
import org.lox.lexer.TokenType.EOF
import org.lox.lexer.{Scanner, Token}
import org.lox.parser.{Expr, Parser, Stmt}
import org.lox.runtime.Interpreter

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
    val code = try source.mkString finally source.close()
    run(code)
  }

  private def prompt: LineReader = {
    val terminal = TerminalBuilder.builder.build
    val parser = new DefaultParser
    LineReaderBuilder
      .builder
      .terminal(terminal)
      .parser(parser)
      .build
  }

  private def runPrompt(): Unit = {
    var continue = true
    val reader = prompt
    while (continue) {
      try {
        val line = reader.readLine("slox> ")
        if (line == null) continue = false
        else runRepl(line)
      } catch {
        case _: UserInterruptException => System.exit(0)
      }
    }
  }

  def runRepl(code: String): Unit = {
    val expressionParse: Try[Expr] = parse(code, tokens => new Parser(tokens, shouldLog = false).parseExpression)
    if (expressionParse.isSuccess) {
      expressionParse.flatMap(expr => interpreter.interpretExpression(expr)).foreach(println)
      return
    }
    val fullParse: Try[List[Stmt]] = parse(code, tokens => new Parser(tokens).parse)
    fullParse match {
      case Success(value: Seq[Stmt]) => interpreter.interpret(value)
      case Failure(e) => System.err.println(e)
    }
  }

  def parse[T](source: String, parseFn: Seq[Token] => Try[T]): Try[T] = for {
    tokens: Seq[Token] <- Scanner(source).apply
    parseOutput <- parseFn.apply(tokens)
  } yield parseOutput

  def run(source: String, shouldError: Boolean = true): Option[Unit] = for {
    statements <- handle(parse(source, tokens => new Parser(tokens).parse), 65, shouldError)
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
