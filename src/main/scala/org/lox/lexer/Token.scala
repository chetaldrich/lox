package org.lox.lexer

case class Token(tokenType: TokenType, lexeme: String, literal: Any, line: Int)
