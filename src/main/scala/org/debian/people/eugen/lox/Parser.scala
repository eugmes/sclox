package org.debian.people.eugen.lox

import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks.{break, breakable}
import scala.util.control.NonLocalReturns.{returning, throwReturn}

final class Parser(tokens: Seq[Token]):
  private var current = 0

  private final class ParseError extends RuntimeException

  private def isAtEnd = tokens(current).tokenType == TokenType.EOF

  private def check(tokenType: TokenType): Boolean = !isAtEnd && peek().tokenType == tokenType

  private def peek(): Token = tokens(current)

  private def previous(): Token = tokens(current - 1)

  private def advance(): Token =
    if !isAtEnd then current += 1
    previous()

  private def matchToken(tokenTypes: TokenType*): Boolean = returning {
    for tokenType <- tokenTypes do
      if check(tokenType) then
        advance()
        throwReturn(true)
    false
  }

  private def consume(tokenType: TokenType, message: String): Token =
    if check(tokenType) then advance() else throw error(peek(), message)

  private def error(token: Token, message: String): ParseError =
    Lox.error(token, message)
    ParseError()

  private def expression(): Expr = assignment()

  private def assignment(): Expr =
    val expr = or()

    if matchToken(TokenType.EQUAL) then
      val equals = previous()
      val value = assignment()

      expr match
        case Expr.Variable(name) => Expr.Assign(name, value)
        case Expr.Get(obj, name) => Expr.Set(obj, name, value)
        case _ => throw error(equals, "Invalid assignment target.")
    else expr

  private def or(): Expr =
    var expr = and()

    while matchToken(TokenType.OR) do
      val operator = previous()
      val right = and()
      expr = Expr.Logical(expr, operator, right)

    expr

  private def and(): Expr =
    var expr = equality()

    while matchToken(TokenType.OR) do
      val operator = previous()
      val right = equality()
      expr = Expr.Logical(expr, operator, right)

    expr

  private def equality(): Expr =
    var expr = comparison()
    while matchToken(TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL) do
      val operator = previous()
      val right = comparison()
      expr = Expr.Binary(expr, operator, right)

    expr

  private def comparison(): Expr =
    var expr = term()

    while matchToken(TokenType.GREATER, TokenType.GREATER_EQUAL, TokenType.LESS, TokenType.LESS_EQUAL) do
      val operator = previous()
      val right = term()
      expr = Expr.Binary(expr, operator, right)

    expr

  private def term(): Expr =
    var expr = factor()

    while matchToken(TokenType.MINUS, TokenType.PLUS) do
      val operator = previous()
      val right = factor()
      expr = Expr.Binary(expr, operator, right)

    expr

  private def factor(): Expr =
    var expr = unary()

    while matchToken(TokenType.SLASH, TokenType.STAR) do
      val operator = previous()
      val right = unary()
      expr = Expr.Binary(expr, operator, right)
    expr

  private def unary(): Expr =
    if matchToken(TokenType.BANG, TokenType.MINUS) then
      val operator = previous()
      val right = unary()
      Expr.Unary(operator, right)
    else
      call()

  private def call(): Expr =
    var expr = primary()

    breakable {
      while true do
        if matchToken(TokenType.LEFT_PAREN) then
          expr = finishCall(expr)
        else if matchToken(TokenType.DOT) then
          val name = consume(TokenType.IDENTIFIER, "Expect property name after '.'.")
          expr = Expr.Get(expr, name)
        else
          break
    }

    expr

  private def finishCall(callee: Expr): Expr =
    val arguments = ArrayBuffer[Expr]()
    if !check(TokenType.RIGHT_PAREN) then
      while
        if arguments.length >= 255 then
          error(peek(), "Can't have more than 255 arguments.")
        arguments.append(expression())
        matchToken(TokenType.COMMA)
      do ()

    val paren = consume(TokenType.RIGHT_PAREN, "Expect ')' after arguments.")
    Expr.Call(callee, paren, arguments.toSeq)

  private def primary(): Expr =
    if matchToken(TokenType.FALSE) then Expr.Literal(false)
    else if matchToken(TokenType.TRUE) then Expr.Literal(true)
    else if matchToken(TokenType.NIL) then Expr.Literal(null)
    else if matchToken(TokenType.NUMBER, TokenType.STRING) then Expr.Literal(previous().literal)
    else if matchToken(TokenType.SUPER) then
      val keyword = previous()
      consume(TokenType.DOT, "Expect '.' after 'super'.")
      val method = consume(TokenType.IDENTIFIER, "Expect superclass method name.")
      Expr.Super(keyword, method)
    else if matchToken(TokenType.THIS) then Expr.This(previous())
    else if matchToken(TokenType.IDENTIFIER) then Expr.Variable(previous())
    else if matchToken(TokenType.LEFT_PAREN) then
      val expr = expression()
      consume(TokenType.RIGHT_PAREN, "Expect ')' after expression.")
      Expr.Grouping(expr)
    else throw error(peek(), "Expect expression.")

  private def synchronize(): Unit = returning {
    advance()
    while !isAtEnd do
      if previous().tokenType == TokenType.SEMICOLON then throwReturn(())
      peek().tokenType match
        case TokenType.CLASS | TokenType.FOR | TokenType.FUN | TokenType.IF | TokenType.PRINT | TokenType.RETURN
             | TokenType.VAR | TokenType.VAR => throwReturn(())
        case _ => advance()
  }

  private def declaration(): Stmt =
    try
      if matchToken(TokenType.VAR) then varDeclaration()
      else if matchToken(TokenType.CLASS) then classDeclaration()
      else if matchToken(TokenType.FUN) then function("function")
      else statement()
    catch
      case _: ParseError =>
        synchronize()
        null

  private def classDeclaration(): Stmt =
    val name = consume(TokenType.IDENTIFIER, "Expect class name.")
    consume(TokenType.LEFT_BRACE, "Expect '{' before class body.")

    val superclass: Option[Expr.Variable] = if matchToken(TokenType.LESS) then
      consume(TokenType.IDENTIFIER, "Expect superclass name.")
      Some(Expr.Variable(previous()))
    else
      None

    val methods = ArrayBuffer[Stmt.Function]()
    while !check(TokenType.RIGHT_BRACE) && !isAtEnd do
      methods.append(function("method"))

    consume(TokenType.RIGHT_BRACE, "Expect '}' after class body.")
    Stmt.Class(name, superclass, methods.toSeq)

  private def function(kind: String): Stmt.Function =
    val name = consume(TokenType.IDENTIFIER, s"Expect $kind name.")
    consume(TokenType.LEFT_PAREN, s"Expect '(' after $kind name.")
    val parameters: ArrayBuffer[Token] = ArrayBuffer()
    if !check(TokenType.RIGHT_PAREN) then
      while
        if parameters.length >= 255 then
          error(peek(), "Can't have more than 255 parameters.")
        parameters.append(consume(TokenType.IDENTIFIER, "Expect parameter name."))
        matchToken(TokenType.COMMA)
      do ()
    consume(TokenType.RIGHT_PAREN, "Expect ')' after parameters.")

    consume(TokenType.LEFT_BRACE, s"Expect '{' before $kind body")
    val body = block()
    Stmt.Function(name, parameters.toSeq, body)

  private def varDeclaration(): Stmt =
    val name = consume(TokenType.IDENTIFIER, "Expect variable name.")
    val initializer = if matchToken(TokenType.EQUAL) then expression() else null
    consume(TokenType.SEMICOLON, "Expect ';' after variable declaration.")
    Stmt.Var(name, initializer)

  private def statement(): Stmt =
    if matchToken(TokenType.PRINT) then printStatement()
    else if matchToken(TokenType.LEFT_BRACE) then Stmt.Block(block())
    else if matchToken(TokenType.IF) then ifStatement()
    else if matchToken(TokenType.WHILE) then whileStatement()
    else if matchToken(TokenType.FOR) then forStatement()
    else if matchToken(TokenType.RETURN) then returnStatement()
    else expressionStatement()

  private def returnStatement(): Stmt =
    val keyword = previous()
    val value = if check(TokenType.SEMICOLON) then null else expression()
    consume(TokenType.SEMICOLON, "Expect ';' after return value.")
    Stmt.Return(keyword, value)

  private def forStatement(): Stmt =
    consume(TokenType.LEFT_PAREN, "Expect '(' after 'for'.")

    val initializer = if matchToken(TokenType.SEMICOLON) then null
    else if matchToken(TokenType.VAR) then varDeclaration()
    else expressionStatement()

    val condition = if check(TokenType.SEMICOLON) then Expr.Literal(true) else expression()
    consume(TokenType.SEMICOLON, "Expect ';' after loop condition.")

    val increment = if check(TokenType.RIGHT_PAREN) then null else expression()
    consume(TokenType.RIGHT_PAREN, "Expect ')' after for clauses.")

    var body = statement()

    if increment != null then
      body = Stmt.Block(Seq(body, Stmt.Expression(increment)))

    body = Stmt.While(condition, body)

    if initializer != null then
      body = Stmt.Block(Seq(initializer, body))

    body

  private def whileStatement(): Stmt =
    consume(TokenType.LEFT_PAREN, "Expect '(' after 'while'.")
    val condition = expression()
    consume(TokenType.RIGHT_PAREN, "Expect ')' after while condition.")
    val body = statement()

    Stmt.While(condition, body)

  private def ifStatement(): Stmt =
    consume(TokenType.LEFT_PAREN, "Expect '(' after 'if'.")
    val condition = expression()
    consume(TokenType.RIGHT_PAREN, "Expect ')' after if condition.")

    val thenBranch = statement()
    val elseBranch = if matchToken(TokenType.ELSE) then statement() else null
    Stmt.If(condition, thenBranch, elseBranch)

  private def block(): Seq[Stmt] =
    val statements = ArrayBuffer[Stmt]()

    while !check(TokenType.RIGHT_BRACE) && !isAtEnd do
      statements.append(declaration())

    consume(TokenType.RIGHT_BRACE, "Expect '}' after block.")
    statements.toSeq

  private def printStatement(): Stmt =
    val value = expression()
    consume(TokenType.SEMICOLON, "Expect ';' after value.")
    Stmt.Print(value)

  private def expressionStatement(): Stmt =
    val expr = expression()
    consume(TokenType.SEMICOLON, "Expect ';' after expression.")
    Stmt.Expression(expr)

  def parse(): Seq[Stmt] =
    val statements  = ArrayBuffer[Stmt]()
    while !isAtEnd do statements.append(declaration())
    statements.toSeq
