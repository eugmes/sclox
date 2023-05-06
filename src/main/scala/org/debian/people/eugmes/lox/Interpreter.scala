package org.debian.people.eugmes.lox

import scala.annotation.tailrec

class Interpreter {
  private var environment = Environment()

  def interpret(statements: Seq[Stmt]): Unit = {
    try
      for statement <- statements do
        execute(statement)
    catch
      case error: RuntimeError => Lox.runtimeError(error)
  }

  private def stringify(value: Any): String = {
    value match
      case null => "nil"
      case d: Double =>
        var text = d.toString
        if (text.endsWith(".0")) {
          text = text.substring(0, text.length - 2)
        }
        text
      case _ => value.toString
  }

  @tailrec
  private def evaluate(expression: Expr): Any = {
    expression match
      case Expr.Grouping(expression) => evaluate(expression)
      case Expr.Unary(operator, right) => evaluateUnary(operator, right)
      case Expr.Binary(left, operator, right) => evaluateBinary(left, operator, right)
      case Expr.Literal(value) => value
      case Expr.Variable(name) => environment.get(name)
      case Expr.Assign(name, value) => evaluateAssign(name, value)
      case Expr.Logical(left, operator, right) => evaluateLogical(left, operator, right)
  }

  private def execute(stmt: Stmt): Unit = {
    stmt match
      case Stmt.Expression(expression) => evaluate(expression)
      case Stmt.Print(expression) =>
        val value = evaluate(expression)
        println(stringify(value))
      case Stmt.Var(name, initializer) =>
        val value = if (initializer == null) {
          null
        } else {
          evaluate(initializer)
        }
        environment.define(name.lexeme, value)
      case Stmt.Block(statements) => executeBlock(statements, Environment(environment))
      case Stmt.If(condition, thenBranch, elseBranch) => executeIf(condition, thenBranch, elseBranch)
      case Stmt.While(condition, body) => executeWhile(condition, body)
  }

  private def executeWhile(condition: Expr, body: Stmt): Unit = {
    while isTruly(evaluate(condition)) do execute(body)
  }

  private def executeBlock(statements: Seq[Stmt], environment: Environment): Unit = {
    val previous = this.environment
    try
      this.environment = environment
      for statement <- statements do execute(statement)
    finally
      this.environment = previous

  }

  private def executeIf(condition: Expr, thenBranch: Stmt, elseBranch: Stmt): Unit = {
    if (isTruly(evaluate(condition))) {
      execute(thenBranch)
    } else if (elseBranch != null) {
      execute(elseBranch)
    }
  }

  private def checkNumberOperand(token: Token, right: Any): Double = {
    right match
      case d: Double => d
      case _ => throw RuntimeError(token, "Right operand must be a number.")
  }

  private def checkNumberOperands(token: Token, left: Any, right: Any): (Double, Double) = {
    val leftValue = left match
      case d: Double => d
      case _ => throw RuntimeError(token, "Left operand must be a number.")
    val rightValue = right match
      case d: Double => d
      case _ => throw RuntimeError(token, "Right operand must be a number.")
    (leftValue, rightValue)
  }

  private def evaluateAssign(name: Token, expr: Expr): Any = {
    val value = evaluate(expr)
    environment.assign(name, value)
    value
  }

  private def evaluateUnary(token: Token, right: Expr): Any = {
    val rightValue = evaluate(right)

    token.tokenType match
      case TokenType.MINUS => checkNumberOperand(token, rightValue)
      case TokenType.BANG => !isTruly(rightValue)
      case _ => assert(false)
  }

  private def evaluateBinary(left: Expr, token: Token, right: Expr): Any = {
    val leftValue = evaluate(left)
    val rightValue = evaluate(right)

    token.tokenType match
      case TokenType.GREATER =>
        val (l, r) = checkNumberOperands(token, leftValue, rightValue)
        l > r
      case TokenType.GREATER_EQUAL =>
        val (l, r) = checkNumberOperands(token, leftValue, rightValue)
        l >= r
      case TokenType.LESS =>
        val (l, r) = checkNumberOperands(token, leftValue, rightValue)
        l < r
      case TokenType.LESS_EQUAL =>
        val (l, r) = checkNumberOperands(token, leftValue, rightValue)
        l <= r
      case TokenType.BANG_EQUAL => !isEqual(leftValue, rightValue)
      case TokenType.EQUAL_EQUAL => isEqual(leftValue, rightValue)
      case TokenType.PLUS =>
        (leftValue, rightValue) match
          case (l: Double, r: Double) => l + r
          case (l: String, r: String) => l + r
          case _ => throw RuntimeError(token, "Operands must be two numbers or two strings")
      case TokenType.MINUS =>
        val (l, r) = checkNumberOperands(token, leftValue, rightValue)
        l + r
      case TokenType.SLASH =>
        val (l, r) = checkNumberOperands(token, leftValue, rightValue)
        l / r
      case TokenType.STAR =>
        val (l, r) = checkNumberOperands(token, leftValue, rightValue)
        l * r
      case _ => assert(false)
  }

  private def evaluateLogical(left: Expr, operator: Token, right: Expr): Any = {
    val l = evaluate(left)

    if (operator.tokenType == TokenType.OR) {
      if isTruly(l) then l else evaluate(right)
    } else {
      assert(operator.tokenType == TokenType.AND)
      if !isTruly(l) then l else evaluate(right)
    }
  }

  private def isTruly(value: Any): Boolean = {
    value match
      case null => false
      case b: Boolean => b
      case _ => true
  }

  private def isEqual(left: Any, right: Any): Boolean = {
    if (left == null && right == null) {
      true
    } else if (left == null) {
      false
    } else {
      left == right
    }
  }
}
