package org.debian.people.eugmes.lox

enum Stmt:
  case Expression(expression: Expr)
  case Print(expression: Expr)
  case Var(name: Token, initializer: Expr)
  case Block(statements: Seq[Stmt])
  case If(condition: Expr, thenBranch: Stmt, elseBranch: Stmt)
