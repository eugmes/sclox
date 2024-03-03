package lox

import java.util
import scala.collection.mutable

final class Interpreter extends Resolve, Stmt.Visitor[Unit], Expr.Visitor[LoxValue]:
  private val globals: Environment =
    val globals = Environment()
    globals.define("clock", new LoxCallable:
      override def arity: Int = 0

      override def call(interpreter: Interpreter, arguments: Seq[LoxValue]): LoxValue =
        System.currentTimeMillis().toDouble / 1000.0

      override def toString: String = "<native fn>"
    )
    globals

  private var environment = globals
  // Map of distances to perform variable lookup for different expressions.
  // Identities must be used here because distances may be different for identical expressions.
  private val locals = util.IdentityHashMap[Expr, Integer]()

  def withEnvironment[T](env: Environment)(op: => T): T =
    val oldEnvironment = environment
    environment = env

    try
      op
    finally
      environment = oldEnvironment

  def interpret(statements: Seq[Stmt]): Unit =
    try
      statements.foreach(_.visit(this))
    catch
      case error: RuntimeError => Lox.runtimeError(error)

  private def stringify(value: LoxValue): String =
    value match
      case null => "nil"
      case d: Double =>
        var text = d.toString
        if text.endsWith(".0") then
          text = text.substring(0, text.length - 2)
        text
      case _ => value.toString

  override def visitExpression(node: Stmt.Expression): Unit = node.expression.visit(this)

  override def visitPrint(node: Stmt.Print): Unit =
    val value = node.expression.visit(this)
    println(stringify(value))

  override def visitVar(node: Stmt.Var): Unit =
    val value = if node.initializer == null then null else node.initializer.visit(this)
    environment.define(node.name.lexeme, value)

  override def visitFunction(node: Stmt.Function): Unit =
    environment.define(node.name.lexeme, LoxFunction(node, environment, false))

  override def visitClass(node: Stmt.Class): Unit =
    val sc = node.superclass.map(superclass =>
      superclass.visit(this) match
        case klass: LoxClass => klass
        case _ => throw RuntimeError(superclass.name, "Superclass must be a class.")
    )

    environment.define(node.name.lexeme, null)

    sc.foreach: superclass =>
      environment = Environment(environment)
      environment.define("super", superclass)

    val methodFunctions = mutable.HashMap[String, LoxFunction]()
    node.methods.foreach: method =>
      val methodName = method.name.lexeme
      val isInitializer = methodName == "init"
      val function = LoxFunction(method, environment, isInitializer)
      methodFunctions.put(methodName, function)

    val klass = LoxClass(node.name.lexeme, sc, methodFunctions.toMap)

    if sc.isDefined then
      environment = environment.enclosing

    environment.assign(node.name, klass)

  override def visitReturn(node: Stmt.Return): Nothing =
    val v = if node.value == null then null else node.value.visit(this)
    throw Return(v)

  override def visitWhile(node: Stmt.While): Unit =
    while isTruly(node.condition.visit(this)) do node.body.visit(this)

  override def visitBlock(node: Stmt.Block): Unit =
    withEnvironment(Environment()):
      node.statements.foreach(_.visit(this))

  override def visitIf(node: Stmt.If): Unit =
    if isTruly(node.condition.visit(this)) then
      node.thenBranch.visit(this)
    else if node.elseBranch != null then
      node.elseBranch.visit(this)

  private def checkNumberOperand(token: Token, right: LoxValue): Double =
    right match
      case d: Double => d
      case _ => throw RuntimeError(token, "Right operand must be a number.")

  private def checkNumberOperands(token: Token, left: LoxValue, right: LoxValue): (Double, Double) =
    (left, right) match
      case (l: Double, r: Double) => (l, r)
      case _ => throw RuntimeError(token, "Both operands must be numbers.")

  override def visitSet(node: Expr.Set): LoxValue =
    node.obj.visit(this) match
      case obj: LoxInstance =>
        val evaluatedValue = node.value.visit(this)
        obj.set(node.name, evaluatedValue)
        evaluatedValue
      case _ => throw RuntimeError(node.name, "Only instances have fields.")

  override def visitGet(node: Expr.Get): LoxValue =
    node.obj.visit(this) match
      case obj: LoxInstance => obj.get(node.name)
      case _ => throw RuntimeError(node.name, "Only instances have properties.")

  override def visitCall(node: Expr.Call): LoxValue =
    node.callee.visit(this) match
      case function: LoxCallable =>
        val evalArgs = node.arguments.map(_.visit(this))
        if evalArgs.length != function.arity then
          throw RuntimeError(node.paren, s"Expected ${function.arity} arguments but got ${evalArgs.length}.")
        function.call(this, evalArgs)
      case _ => throw RuntimeError(node.paren, "Can only call functions and classes.")

  override def visitAssign(node: Expr.Assign): LoxValue =
    val evaluated = node.value.visit(this)
    val distance = locals.get(node)
    if distance == null then globals.assign(node.name, evaluated) else environment.assignAt(distance, node.name, evaluated)
    evaluated

  override def visitUnary(node: Expr.Unary): LoxValue =
    val rightValue = node.right.visit(this)

    node.operator.tokenType match
      case TokenType.MINUS => -checkNumberOperand(node.operator, rightValue)
      case TokenType.BANG => !isTruly(rightValue)
      case _ => assert(false)

  override def visitBinary(node: Expr.Binary): LoxValue =
    val leftValue = node.left.visit(this)
    val rightValue = node.right.visit(this)

    val token = node.operator

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
      case TokenType.BANG_EQUAL => leftValue != rightValue
      case TokenType.EQUAL_EQUAL => leftValue == rightValue
      case TokenType.PLUS =>
        (leftValue, rightValue) match
          case (l: Double, r: Double) => l + r
          case (l: String, r: String) => l + r
          case _ => throw RuntimeError(token, "Operands must be two numbers or two strings")
      case TokenType.MINUS =>
        val (l, r) = checkNumberOperands(token, leftValue, rightValue)
        l - r
      case TokenType.SLASH =>
        val (l, r) = checkNumberOperands(token, leftValue, rightValue)
        l / r
      case TokenType.STAR =>
        val (l, r) = checkNumberOperands(token, leftValue, rightValue)
        l * r
      case _ => assert(false)

  override def visitLogical(node: Expr.Logical): LoxValue =
    val l = node.left.visit(this)

    if node.operator.tokenType == TokenType.OR then
      if isTruly(l) then l else node.right.visit(this)
    else
      assert(node.operator.tokenType == TokenType.AND)
      if !isTruly(l) then l else node.right.visit(this)

  private def isTruly(value: LoxValue): Boolean =
    value match
      case null => false
      case b: Boolean => b
      case _ => true

  override def resolve(expr: Expr, depth: Int): Unit = locals.put(expr, depth)

  private def lookupVariable(name: Token, expr: Expr): LoxValue =
    val distance = locals.get(expr)
    if distance == null then globals.get(name) else environment.getAt(distance, name.lexeme).get

  override def visitGrouping(node: Expr.Grouping): LoxValue = node.expression.visit(this)

  override def visitLiteral(node: Expr.Literal): LoxValue = node.value

  override def visitVariable(node: Expr.Variable): LoxValue = lookupVariable(node.name, node)

  override def visitThis(node: Expr.This): LoxValue = lookupVariable(node.keyword, node)

  override def visitSuper(node: Expr.Super): LoxValue =
    val distance = locals.get(node)
    val superclass = environment.getAt(distance, "super").asInstanceOf[LoxClass]
    val instance = environment.getAt(distance - 1, "this").asInstanceOf[LoxInstance]
    val method = superclass.findMethod(node.method.lexeme)
    method match
      case Some(method) => method.bind(instance)
      case None => throw RuntimeError(node.method, s"Undefined property '${node.method.lexeme}'.")

end Interpreter
