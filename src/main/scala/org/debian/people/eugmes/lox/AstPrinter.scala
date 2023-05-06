package org.debian.people.eugmes.lox

class AstPrinter {
  def print(expr: Expr): String = {
    expr match
      case Expr.Binary(left, operator, right) => parenthesize(operator.lexeme, left, right)
      case Expr.Grouping(expression) => parenthesize("group", expression)
      case Expr.Literal(value) => if (value == null) "nil" else value.toString
      case Expr.Unary(operator, right) => parenthesize(operator.lexeme, right)
  }

  private def parenthesize(name: String, exprs: Expr*): String = {
    val builder = StringBuilder()

    builder.append('(').append(name)
    for (expr <- exprs) {
      builder.append(' ').append(print(expr))
    }
    builder.append(')')

    builder.toString()
  }
}
