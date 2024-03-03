package lox

sealed trait Expr:
  def visit[T](visitor: Expr.Visitor[T]): T

object Expr:
  case class Binary(left: Expr, operator: Token, right: Expr) extends Expr:
    override def visit[T](visitor: Visitor[T]): T = visitor.visitBinary(this)

  case class Grouping(expression: Expr) extends Expr:
    override def visit[T](visitor: Visitor[T]): T = visitor.visitGrouping(this)

  case class Literal(value: LiteralValue) extends Expr:
    override def visit[T](visitor: Visitor[T]): T = visitor.visitLiteral(this)

  case class Unary(operator: Token, right: Expr) extends Expr:
    override def visit[T](visitor: Visitor[T]): T = visitor.visitUnary(this)

  case class Variable(name: Token) extends Expr:
    override def visit[T](visitor: Visitor[T]): T = visitor.visitVariable(this)

  case class Assign(name: Token, value: Expr) extends Expr:
    override def visit[T](visitor: Visitor[T]): T = visitor.visitAssign(this)

  case class Logical(left: Expr, operator: Token, right: Expr) extends Expr:
    override def visit[T](visitor: Visitor[T]): T = visitor.visitLogical(this)

  case class Call(callee: Expr, paren: Token, arguments: Seq[Expr]) extends Expr:
    override def visit[T](visitor: Visitor[T]): T = visitor.visitCall(this)

  case class Get(obj: Expr, name: Token) extends Expr:
    override def visit[T](visitor: Visitor[T]): T = visitor.visitGet(this)

  case class Set(obj: Expr, name: Token, value: Expr) extends Expr:
    override def visit[T](visitor: Visitor[T]): T = visitor.visitSet(this)

  case class This(keyword: Token) extends Expr:
    override def visit[T](visitor: Visitor[T]): T = visitor.visitThis(this)

  case class Super(keyword: Token, method: Token) extends Expr:
    override def visit[T](visitor: Visitor[T]): T = visitor.visitSuper(this)

  trait Visitor[T]:
    def visitBinary(node: Expr.Binary): T
    def visitGrouping(node: Expr.Grouping): T
    def visitLiteral(node: Expr.Literal): T
    def visitUnary(node: Expr.Unary): T
    def visitVariable(node: Expr.Variable): T
    def visitAssign(node: Expr.Assign): T
    def visitLogical(node: Expr.Logical): T
    def visitCall(node: Expr.Call): T
    def visitGet(node: Expr.Get): T
    def visitSet(node: Expr.Set): T
    def visitThis(node: Expr.This): T
    def visitSuper(node: Expr.Super): T
