package org.debian.people.eugmes.lox

trait LoxCallable:
  def call(interpreter: Interpreter, arguments: Seq[LoxValue]): LoxValue
  def arity: Int
end LoxCallable
