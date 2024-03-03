package lox

import scala.collection.mutable
import scala.util.boundary
import boundary.break

trait Resolve:
  def resolve(expr: Expr, depth: Int): Unit

final class Resolver(interpreter: Resolve) extends Stmt.Visitor[Unit], Expr.Visitor[Unit]:
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
    statements.foreach(_.visit(this))

  override def visitExpression(node: Stmt.Expression): Unit = node.expression.visit(this)

  override def visitPrint(node: Stmt.Print): Unit = node.expression.visit(this)

  override def visitVar(node: Stmt.Var): Unit =
    declare(node.name)
    if node.initializer != null then node.initializer.visit(this)
    define(node.name)

  override def visitBlock(node: Stmt.Block): Unit =
    beginScope()
    node.statements.foreach(_.visit(this))
    endScope()

  override def visitIf(node: Stmt.If): Unit =
    node.condition.visit(this)
    node.thenBranch.visit(this)
    if node.elseBranch != null then node.elseBranch.visit(this)

  override def visitWhile(node: Stmt.While): Unit =
    node.condition.visit(this)
    node.body.visit(this)

  override def visitFunction(node: Stmt.Function): Unit =
    declare(node.name)
    define(node.name)
    resolveFunction(node, FunctionType.FUNCTION)

  override def visitReturn(node: Stmt.Return): Unit =
    if currentFunction == FunctionType.NONE then
      Lox.error(node.keyword, "Can't return from top-level code.")
    if node.value != null then
      if currentFunction == FunctionType.INITIALIZER then
        Lox.error(node.keyword, "Can't return a value from initializer.")
      node.value.visit(this)

  override def visitClass(node: Stmt.Class): Unit =
    val enclosingClass = currentClass
    currentClass = ClassType.CLASS
    declare(node.name)
    define(node.name)

    node.superclass.foreach: superclass =>
      if node.name.lexeme == superclass.name.lexeme then
        Lox.error(superclass.name, "A class cannot inherit from itself.")
      currentClass = ClassType.SUBCLASS
      superclass.visit(this)
      beginScope()
      scopes.top.put("super", true)

    beginScope()
    scopes.top.put("this", true)

    for method <- node.methods do
      val declaration = if method.name.lexeme == "init" then FunctionType.INITIALIZER else FunctionType.METHOD
      resolveFunction(method, declaration)

    endScope()

    node.superclass.foreach(_ => endScope())

    currentClass = enclosingClass

  override def visitBinary(node: Expr.Binary): Unit =
    node.left.visit(this)
    node.right.visit(this)

  override def visitGrouping(node: Expr.Grouping): Unit = node.expression.visit(this)

  override def visitLiteral(node: Expr.Literal): Unit = ()

  override def visitUnary(node: Expr.Unary): Unit = node.right.visit(this)

  override def visitVariable(node: Expr.Variable): Unit =
    if scopes.nonEmpty && scopes.top.get(node.name.lexeme).contains(false) then
      Lox.error(node.name, "Can't read local variable in its own initializer.")
    resolveLocal(node, node.name)

  override def visitAssign(node: Expr.Assign): Unit =
    node.value.visit(this)
    resolveLocal(node, node.name)

  override def visitLogical(node: Expr.Logical): Unit =
    node.left.visit(this)
    node.right.visit(this)

  override def visitCall(node: Expr.Call): Unit =
    node.callee.visit(this)
    node.arguments.foreach(_.visit(this))

  override def visitGet(node: Expr.Get): Unit = node.obj.visit(this)

  override def visitSet(node: Expr.Set): Unit =
    node.value.visit(this)
    node.obj.visit(this)

  override def visitThis(node: Expr.This): Unit =
    if currentClass == ClassType.NONE then
      Lox.error(node.keyword, "Can't use 'this' outside of a class.")
    else
      resolveLocal(node, node.keyword)

  override def visitSuper(node: Expr.Super): Unit =
    if currentClass == ClassType.NONE then
      Lox.error(node.keyword, "Can't use 'super' outside of a class.")
    else if currentClass != ClassType.SUBCLASS then
      Lox.error(node.keyword, "Can't use 'super' in a class with no superclass.")
    resolveLocal(node, node.keyword)

  private def resolveLocal(expression: Expr, name: Token): Unit = boundary:
    for i <- scopes.indices do
      if scopes(i).contains(name.lexeme) then
        interpreter.resolve(expression, i)
        break(())

  private def resolveFunction(function: Stmt.Function, functionType: FunctionType): Unit =
    val enclosingFunction = currentFunction
    currentFunction = functionType

    beginScope()
    function.params.foreach: param =>
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
