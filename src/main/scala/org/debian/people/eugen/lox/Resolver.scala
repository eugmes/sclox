package org.debian.people.eugen.lox

import scala.collection.mutable
import scala.util.control.NonLocalReturns.{returning, throwReturn}

final class Resolver(interpreter: Interpreter):
  private val scopes = mutable.Stack[mutable.Map[String, Boolean]]()

  private enum FunctionType:
    case NONE, FUNCTION, METHOD, INITIALIZER
  end FunctionType

  private enum ClassType:
    case NONE, CLASS, SUBCLASS
  end ClassType

  private var currentFunction = FunctionType.NONE
  private var currentClass = ClassType.NONE

  private def beginScope(): Unit = scopes.push(mutable.HashMap())

  private def endScope(): Unit = scopes.pop()

  def resolve(statements: Seq[Stmt]): Unit =
    for statement <- statements do resolve(statement)

  private def resolve(statement: Stmt): Unit =
    statement match
      case Stmt.Expression(expression) => resolve(expression)
      case Stmt.Print(expression) => resolve(expression)
      case Stmt.Var(name, initializer) =>
        declare(name)
        if initializer != null then resolve(initializer)
        define(name)
      case Stmt.Block(statements) =>
        beginScope()
        resolve(statements)
        endScope()
      case Stmt.If(condition, thenBranch, elseBranch) =>
        resolve(condition)
        resolve(thenBranch)
        if elseBranch != null then resolve(elseBranch)
      case Stmt.While(condition, body) =>
        resolve(condition)
        resolve(body)
      case Stmt.Function(name, _, _) =>
        declare(name)
        define(name)
        resolveFunction(statement.asInstanceOf[Stmt.Function], FunctionType.FUNCTION)
      case Stmt.Return(keyword, value) =>
        if currentFunction == FunctionType.NONE then
          Lox.error(keyword, "Can't return from top-level code.")
        if value != null then
          if currentFunction == FunctionType.INITIALIZER then
            Lox.error(keyword, "Can't return a value from initializer.")
          resolve(value)
      case Stmt.Class(name, superclass, methods) =>
        val enclosingClass = currentClass
        currentClass = ClassType.CLASS
        declare(name)
        define(name)

        superclass.foreach(superclass =>
          if name.lexeme == superclass.name.lexeme then
            Lox.error(superclass.name, "A class cannot inherit from itself.")
          currentClass = ClassType.SUBCLASS
          resolve(superclass)
          beginScope()
          scopes.top.put("super", true)
        )

        beginScope()
        scopes.top.put("this", true)

        for method <- methods do
          val declaration = if method.name.lexeme == "init" then FunctionType.INITIALIZER else FunctionType.METHOD
          resolveFunction(method, declaration)

        endScope()

        superclass.foreach(_ => endScope())

        currentClass = enclosingClass

  private def resolve(expression: Expr): Unit =
    expression match
      case Expr.Binary(left, _, right) =>
        resolve(left)
        resolve(right)
      case Expr.Grouping(expression) => resolve(expression)
      case Expr.Literal(_) =>
      case Expr.Unary(_, right) => resolve(right)
      case Expr.Variable(name) =>
        if scopes.nonEmpty && scopes.top.get(name.lexeme).contains(false) then
          Lox.error(name, "Can't read local variable in its own initializer.")
        resolveLocal(expression, name)
      case Expr.Assign(name, value) =>
        resolve(value)
        resolveLocal(expression, name)
      case Expr.Logical(left, _, right) =>
        resolve(left)
        resolve(right)
      case Expr.Call(callee, _, arguments) =>
        resolve(callee)
        for argument <- arguments do resolve(argument)
      case Expr.Get(obj, _) =>
        resolve(obj)
      case Expr.Set(obj, _, value) =>
        resolve(value)
        resolve(obj)
      case Expr.This(keyword) =>
        if currentClass == ClassType.NONE then
          Lox.error(keyword, "Can't use 'this' outside of a class.")
        else
          resolveLocal(expression, keyword)
      case Expr.Super(keyword, _) =>
        if currentClass == ClassType.NONE then
          Lox.error(keyword, "Can't use 'super' outside of a class.")
        else if currentClass != ClassType.SUBCLASS then
          Lox.error(keyword, "Can't use 'super' in a class with no superclass.")
        resolveLocal(expression, keyword)

  private def resolveLocal(expression: Expr, name: Token): Unit = returning {
    for i <- scopes.indices do
      if scopes(i).contains(name.lexeme) then
        interpreter.resolve(expression, i)
        throwReturn(())
  }

  private def resolveFunction(function: Stmt.Function, functionType: FunctionType): Unit =
    val enclosingFunction = currentFunction
    currentFunction = functionType

    beginScope()
    for param <- function.params do
      declare(param)
      define(param)

    resolve(function.body)
    endScope()

    currentFunction = enclosingFunction

  private def declare(name: Token): Unit =
    if scopes.nonEmpty then
      val scope = scopes.top
      if scope.contains(name.lexeme) then
        Lox.error(name, "Already a variable with this name in this scope.")
      scope.put(name.lexeme, false)

  private def define(name: Token): Unit =
    if scopes.nonEmpty then
      scopes.top.put(name.lexeme, true)
end Resolver
