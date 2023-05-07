package org.debian.people.eugmes.lox

type LiteralValue = Null | String | Double | Boolean

final class Token(val tokenType: TokenType, val lexeme: String, val literal: LiteralValue, val line: Int):
  override def toString: String = s"$tokenType $lexeme $literal"
end Token
