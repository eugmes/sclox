package org.debian.people.eugmes.lox

import scala.collection.immutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.util.control.NonLocalReturns.{returning, throwReturn}

private val keywords: HashMap[String, TokenType] = HashMap(
  "and" -> TokenType.AND,
  "class" -> TokenType.CLASS,
  "else" -> TokenType.ELSE,
  "false" -> TokenType.FALSE,
  "for" -> TokenType.FOR,
  "fun" -> TokenType.FUN,
  "if" -> TokenType.IF,
  "nil" -> TokenType.NIL,
  "or" -> TokenType.OR,
  "print" -> TokenType.PRINT,
  "return" -> TokenType.RETURN,
  "super" -> TokenType.SUPER,
  "this" -> TokenType.THIS,
  "true" -> TokenType.TRUE,
  "var" -> TokenType.VAR,
  "while" -> TokenType.WHILE,
)

class Scanner(source: String):
  private val tokens: ArrayBuffer[Token] = ArrayBuffer()
  private var start = 0
  private var current = 0
  private var line = 1

  def scanTokens(): Seq[Token] = {
    while !isAtEnd do
      start = current
      scanToken()

    tokens.append(Token(TokenType.EOF, "", null, line))
    tokens.toSeq
  }

  private def isAtEnd = current >= source.length

  private def scanToken(): Unit =
    val c = advance()
    c match
      case '(' => addToken(TokenType.LEFT_PAREN)
      case ')' => addToken(TokenType.RIGHT_PAREN)
      case '{' => addToken(TokenType.LEFT_BRACE)
      case '}' => addToken(TokenType.RIGHT_BRACE)
      case ',' => addToken(TokenType.COMMA)
      case '.' => addToken(TokenType.DOT)
      case '-' => addToken(TokenType.MINUS)
      case '+' => addToken(TokenType.PLUS)
      case ';' => addToken(TokenType.SEMICOLON)
      case '*' => addToken(TokenType.STAR)
      case '!' => addToken(if tryMatch('=') then TokenType.BANG_EQUAL else TokenType.BANG)
      case '=' => addToken(if tryMatch('=') then TokenType.EQUAL_EQUAL else TokenType.EQUAL)
      case '<' => addToken(if tryMatch('=') then TokenType.LESS_EQUAL else TokenType.LESS)
      case '>' => addToken(if tryMatch('=') then TokenType.GREATER_EQUAL else TokenType.GREATER)
      case '/' =>
        if tryMatch('/') then
          // A comment goes until the end of the line.
          while !isAtEnd && peek() != '\n' do advance()
        else
          addToken(TokenType.SLASH)
      case ' ' | '\r' | '\t' => // Ignore whitespace
      case '\n' => line += 1
      case '"' => string()
      case _ =>
        if isDigit(c) then
          number()
        else if isAlpha(c) then
          identifier()
        else
          Lox.error(line, "Unexpected character.")

  /** Consumes the next character in the source and returns it. */
  private def advance(): Char =
    val c = source.charAt(current)
    current += 1
    c

  private def addToken(tokenType: TokenType, literal: LiteralValue = null): Unit =
    val text = source.substring(start, current)
    tokens.append(Token(tokenType, text, literal, line))

  private def tryMatch(expected: Char): Boolean = {
    if isAtEnd || source.charAt(current) != expected then
      false
    else
      current += 1
      true
  }

  private def peek(): Char = if (isAtEnd) 0.toChar else source.charAt(current)

  private def peekNext(): Char = if (current + 1 > source.length) 0.toChar else source.charAt(current + 1)

  private def string(): Unit = returning {
    while peek() != '"' && !isAtEnd do
      if peek() == '\n' then line += 1
      advance()

    if isAtEnd then
      Lox.error(line, "Unterminated string.")
      throwReturn(())

    advance() // The closing ".

    // Trim the surrounding quotes.
    val value = source.substring(start + 1, current - 1)
    addToken(TokenType.STRING, value)
  }

  private def isDigit(c: Char) = c >= '0' && c <= '9'

  private def isAlpha(c: Char) = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'

  private def isAlphaNumeric(c: Char) = isAlpha(c) || isDigit(c)

  private def number(): Unit =
    while isDigit(peek()) do advance()

    // Look for a fractional part.
    if peek() == '.' && isDigit(peekNext()) then
      // Consume the "."
      advance()
      while isDigit(peek()) do advance()

    addToken(TokenType.NUMBER, source.substring(start, current).toDouble)

  private def identifier(): Unit =
    while isAlphaNumeric(peek()) do advance()

    val text = source.substring(start, current)
    val tokenType = keywords.getOrElse(text, TokenType.IDENTIFIER)

    addToken(tokenType)
end Scanner
