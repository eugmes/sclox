package org.debian.people.eugen.lox

enum Expr:
  case Binary(left: Expr, operator: Token, right: Expr)
  case Grouping(expression: Expr)
  case Literal(value: LiteralValue)
  case Unary(operator: Token, right: Expr)
  case Variable(name: Token)
  case Assign(name: Token, value: Expr)
  case Logical(left: Expr, operator: Token, right: Expr)
  case Call(callee: Expr, paren: Token, arguments: Seq[Expr])
  case Get(obj: Expr, name: Token)
  case Set(obj: Expr, name: Token, value: Expr)
  case This(keyword: Token)
end Expr