package org.lox.lexer

import scala.util.matching.Regex
import scala.util.parsing.combinator._
import scala.util.{Success, Try}

object Scanner extends RegexParsers {
  override val whiteSpace: Regex = """(\s+|//.*)+""".r

  // A list of all the keywords in the language.
  private val constantLexemes: Seq[ConstantLexeme] = List(
    TokenType.LeftParen,
    TokenType.RightParen,
    TokenType.LeftBrace,
    TokenType.RightBrace,
    TokenType.Comma,
    TokenType.Dot,
    TokenType.Minus,
    TokenType.Plus,
    TokenType.Semicolon,
    TokenType.Star,
    TokenType.BangEqual,
    TokenType.Bang,
    TokenType.QuestionMark,
    TokenType.Colon,
    TokenType.EqualEqual,
    TokenType.Equal,
    TokenType.GreaterEqual,
    TokenType.Greater,
    TokenType.LessEqual,
    TokenType.Less,
    TokenType.Slash,
  )

  val keywordLexemes: Seq[RegexLexeme] = List(
    TokenType.Or,
    TokenType.And,
    TokenType.Class,
    TokenType.Else,
    TokenType.False,
    TokenType.Fun,
    TokenType.For,
    TokenType.If,
    TokenType.Nil,
    TokenType.Print,
    TokenType.Return,
    TokenType.Super,
    TokenType.This,
    TokenType.True,
    TokenType.Var,
    TokenType.While,
  )

  private val constants: Parser[Token] = constantLexemes
    // converts the list of keyword lexemes to a token if the keyword lexeme matches the input
    .map(kwl => kwl.lexeme ^^ { _ => Token(kwl, kwl.lexeme, null, 0) })
    // converts it to a parser that checks for any of the tokens in constantLexemes
    .reduce(_ | _)

  private val keywords: Parser[Token] = keywordLexemes
    // converts the list of keyword lexemes to a token if the keyword lexeme matches the input
    .map(kwl => kwl.lexeme ^^ { s => Token(kwl, s, null, 0) })
    // converts it to a parser that checks for any of the tokens in constantLexemes
    .reduce(_ | _)

  private val stringLiteral: Parser[Token] = "\".*?\"".r ^^ {
    s => Token(TokenType.String, s, s.substring(1, s.length - 1), 0)
  }

  private val numberLiteral: Parser[Token] =
    """\d+(\.\d+)?""".r ^^ {
      s => Token(TokenType.Number, s, s.toDouble, 0)
    }

  private val identifier: Parser[Token] =
    not(keywords) ~> """[a-zA-Z_][a-zA-Z_0-9]*""".r ^^ {
      s => Token(TokenType.Identifier, s, null, 0)
    }

  private val invalid: Parser[Token] = ".+".r ^^ { result => Token(TokenType.Invalid, result, null, 0) }

  // creates a combinator that generates multiple tokens
  val tokenizer: Parser[List[Token]] = rep(constants | keywords | identifier | stringLiteral | numberLiteral | invalid)
}

case class Scanner(source: String) {
  // generates a sequence of tokens from the source code using the parser combinator defined above.
  def apply: Try[Seq[Token]] = Scanner.parse(Scanner.tokenizer, source) match {
    case Scanner.Success(matched: Seq[Token], _) =>
      if (matched.exists(_.tokenType == TokenType.Invalid)) {
        val invalidToken = matched.find(_.tokenType == TokenType.Invalid).get.lexeme
        throw new Exception(s"Invalid token found: $invalidToken")
      } else {
        Success(matched :+ Token(TokenType.EOF, null, null, 0))
      }
    case Scanner.Failure(msg, _) => throw new Exception(msg)
    case Scanner.Error(msg, _) => throw new Exception(msg)
    case _ => throw new Exception("Unknown error")
  }
}
