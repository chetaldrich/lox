package org.lox

import org.lox.TokenType.TokenType

case class Token(tokenType: TokenType, lexeme: String, literal: Any, line: Int)