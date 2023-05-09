package org.debian.people.eugen.lox

trait LoxCallable:
  def call(interpreter: Interpreter, arguments: Seq[LoxValue]): LoxValue
  def arity: Int
end LoxCallable
