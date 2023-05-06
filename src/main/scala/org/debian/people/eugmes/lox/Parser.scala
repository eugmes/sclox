package org.debian.people.eugmes.lox

import scala.collection.mutable.ArrayBuffer
import scala.util.control.NonLocalReturns.{returning, throwReturn}

class Parser(tokens: Seq[Token]) {
  private var current = 0

  private class ParseError extends RuntimeException

  private def isAtEnd = tokens(current).tokenType == TokenType.EOF

  private def check(tokenType: TokenType): Boolean = !isAtEnd && peek().tokenType == tokenType

  private def peek(): Token = tokens(current)

  private def previous(): Token = tokens(current - 1)

  private def advance(): Token = {
    if (!isAtEnd) {
      current += 1
    }
    previous()
  }

  private def matchToken(tokenTypes: TokenType*): Boolean = returning {
    for (tokenType <- tokenTypes) {
      if (check(tokenType)) {
        advance()
        throwReturn(true)
      }
    }
    false
  }

  private def consume(tokenType: TokenType, message: String): Token = {
    if (check(tokenType)) {
      advance()
    } else {
      throw error(peek(), message)
    }
  }

  private def error(token: Token, message: String): ParseError = {
    Lox.error(token, message)
    ParseError()
  }

  private def expression(): Expr = assignment()

  private def assignment(): Expr = {
    val expr = or()

    if (matchToken(TokenType.EQUAL)) {
      val equals = previous()
      val value = assignment()

      expr match
        case Expr.Variable(name) => Expr.Assign(name, value)
        case _ => throw error(equals, "Invalid assignment target.")
    } else {
      expr
    }
  }

  private def or(): Expr = {
    var expr = and()

    while (matchToken(TokenType.OR)) {
      val operator = previous()
      val right = and()
      expr = Expr.Logical(expr, operator, right)
    }

    expr
  }

  private def and(): Expr = {
    var expr = equality()

    while (matchToken(TokenType.OR)) {
      val operator = previous()
      val right = equality()
      expr = Expr.Logical(expr, operator, right)
    }

    expr
  }

  private def equality(): Expr = {
    var expr = comparison()
    while (matchToken(TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL)) {
      val operator = previous()
      val right = comparison()
      expr = Expr.Binary(expr, operator, right)
    }

    expr
  }

  private def comparison(): Expr = {
    var expr = term()

    while (matchToken(TokenType.GREATER, TokenType.GREATER_EQUAL, TokenType.LESS, TokenType.LESS_EQUAL)) {
      val operator = previous()
      val right = term()
      expr = Expr.Binary(expr, operator, right)
    }

    expr
  }

  private def term(): Expr = {
    var expr = factor()

    while (matchToken(TokenType.MINUS, TokenType.PLUS)) {
      val operator = previous()
      val right = factor()
      expr = Expr.Binary(expr, operator, right)
    }

    expr
  }

  private def factor(): Expr = {
    var expr = unary()

    while (matchToken(TokenType.SLASH, TokenType.STAR)) {
      val operator = previous()
      val right = unary()
      expr = Expr.Binary(expr, operator, right)
    }
    expr
  }

  private def unary(): Expr = {
    if (matchToken(TokenType.BANG, TokenType.MINUS)) {
      val operator = previous()
      val right = unary()
      Expr.Unary(operator, right)
    } else {
      primary()
    }
  }

  private def primary(): Expr = {
    if (matchToken(TokenType.FALSE)) {
      Expr.Literal(false)
    } else if (matchToken(TokenType.TRUE)) {
      Expr.Literal(true)
    } else if (matchToken(TokenType.NIL)) {
      Expr.Literal(null)
    } else if (matchToken(TokenType.NUMBER, TokenType.STRING)) {
      Expr.Literal(previous().literal)
    } else if (matchToken(TokenType.IDENTIFIER)) {
      Expr.Variable(previous())
    } else if (matchToken(TokenType.LEFT_PAREN)) {
      val expr = expression()
      consume(TokenType.RIGHT_PAREN, "Expect ')' after expression.")
      Expr.Grouping(expr)
    } else {
      throw error(peek(), "Expect expression.")
    }
  }

  private def synchronize(): Unit = returning {
    advance()
    while (!isAtEnd) {
      if (previous().tokenType == TokenType.SEMICOLON) {
        throwReturn(())
      }
      peek().tokenType match
        case TokenType.CLASS | TokenType.FOR | TokenType.FUN | TokenType.IF | TokenType.PRINT | TokenType.RETURN | TokenType.VAR | TokenType.VAR => throwReturn(())
        case _ => advance()
    }
  }

  private def declaration(): Stmt = {
    try
      if (matchToken(TokenType.VAR)) {
        varDeclaration()
      } else {
        statement()
      }
    catch
      case _: ParseError =>
        synchronize()
        null
  }

  private def varDeclaration(): Stmt = {
    val name = consume(TokenType.IDENTIFIER, "Expect variable name.")
    val initializer = if (matchToken(TokenType.EQUAL)) {
      expression()
    } else {
      null
    }
    consume(TokenType.SEMICOLON, "Expect ';' after variable declaration.")
    Stmt.Var(name, initializer)
  }

  private def statement(): Stmt = {
    if (matchToken(TokenType.PRINT)) {
      printStatement()
    } else if (matchToken(TokenType.LEFT_BRACE)) {
      Stmt.Block(block())
    } else if (matchToken(TokenType.IF)) {
      ifStatement()
    } else {
      expressionStatement()
    }
  }

  private def ifStatement(): Stmt = {
    consume(TokenType.LEFT_PAREN, "Expect '(' after 'if'.")
    val condition = expression()
    consume(TokenType.RIGHT_PAREN, "Expect ')' after if condition.")

    val thenBranch = statement()
    val elseBranch = if matchToken(TokenType.ELSE) then statement() else null
    Stmt.If(condition, thenBranch, elseBranch)
  }

  private def block(): Seq[Stmt] = {
    val statements: ArrayBuffer[Stmt] = ArrayBuffer()

    while (!check(TokenType.RIGHT_BRACE) && !isAtEnd) {
      statements.append(declaration())
    }

    consume(TokenType.RIGHT_BRACE, "Expect '}' after block.")
    statements.toSeq
  }

  private def printStatement(): Stmt = {
    val value = expression()
    consume(TokenType.SEMICOLON, "Expect ';' after value.")
    Stmt.Print(value)
  }

  private def expressionStatement(): Stmt = {
    val expr = expression()
    consume(TokenType.SEMICOLON, "Expect ';' after expression.")
    Stmt.Expression(expr)
  }

  def parse(): Seq[Stmt] = {
    val statements: ArrayBuffer[Stmt] = ArrayBuffer()
    while (!isAtEnd) {
      statements.append(declaration())
    }
    statements.toSeq
  }
}
