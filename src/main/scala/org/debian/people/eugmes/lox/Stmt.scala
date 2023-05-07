package org.debian.people.eugmes.lox

enum Stmt:
  case Expression(expression: Expr)
  case Print(expression: Expr)
  case Var(name: Token, initializer: Expr)
  case Block(statements: Seq[Stmt])
  case If(condition: Expr, thenBranch: Stmt, elseBranch: Stmt)
  case While(condition: Expr, body: Stmt)
  case Function(name: Token, params: Seq[Token], body: Seq[Stmt])
  case Return(keyword: Token, value: Expr)
end Stmt
