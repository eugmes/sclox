package org.debian.people.eugmes.lox

trait LoxCallable {
  def call(interpreter: Interpreter, arguments: Seq[Any]): Any
  def arity: Int
}
