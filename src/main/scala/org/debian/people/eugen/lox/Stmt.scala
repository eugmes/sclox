package org.debian.people.eugen.lox

sealed trait Stmt:
  def visit[T](visitor: Stmt.Visitor[T]): T

object Stmt:
  case class Expression(expression: Expr) extends Stmt:
    override def visit[T](visitor: Visitor[T]): T = visitor.visitExpression(this)

  case class Print(expression: Expr) extends Stmt:
    override def visit[T](visitor: Visitor[T]): T = visitor.visitPrint(this)

  case class Var(name: Token, initializer: Expr) extends Stmt:
    override def visit[T](visitor: Visitor[T]): T = visitor.visitVar(this)

  case class Block(statements: Seq[Stmt]) extends Stmt:
    override def visit[T](visitor: Visitor[T]): T = visitor.visitBlock(this)

  case class If(condition: Expr, thenBranch: Stmt, elseBranch: Stmt) extends Stmt:
    override def visit[T](visitor: Visitor[T]): T = visitor.visitIf(this)

  case class While(condition: Expr, body: Stmt) extends Stmt:
    override def visit[T](visitor: Visitor[T]): T = visitor.visitWhile(this)

  case class Function(name: Token, params: Seq[Token], body: Seq[Stmt]) extends Stmt:
    override def visit[T](visitor: Visitor[T]): T = visitor.visitFunction(this)

  case class Return(keyword: Token, value: Expr) extends Stmt:
    override def visit[T](visitor: Visitor[T]): T = visitor.visitReturn(this)

  case class Class(name: Token, superclass: Option[Expr.Variable], methods: Seq[Stmt.Function]) extends Stmt:
    override def visit[T](visitor: Visitor[T]): T = visitor.visitClass(this)

  trait Visitor[T]:
    def visitExpression(node: Stmt.Expression): T
    def visitPrint(node: Stmt.Print): T
    def visitVar(node: Stmt.Var): T
    def visitBlock(node: Stmt.Block): T
    def visitIf(node: Stmt.If): T
    def visitWhile(node: Stmt.While): T
    def visitFunction(node: Stmt.Function): T
    def visitReturn(node: Stmt.Return): T
    def visitClass(node: Stmt.Class): T
