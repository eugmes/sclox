package org.debian.people.eugmes.lox

sealed trait Stmt

case class ExpressionStmt(expression: Expr) extends Stmt

case class PrintStmt(expression: Expr) extends Stmt

case class VarStmt(name: Token, initializer: Expr) extends Stmt

case class BlockStmt(statements: Seq[Stmt]) extends Stmt

case class IfStmt(condition: Expr, thenBranch: Stmt, elseBranch: Stmt) extends Stmt