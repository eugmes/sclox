package org.debian.people.eugen.lox

type LiteralValue = Null | String | Double | Boolean

final case class Token(tokenType: TokenType, lexeme: String, literal: LiteralValue, line: Int):
  override def toString: String = s"$tokenType $lexeme $literal"
end Token
