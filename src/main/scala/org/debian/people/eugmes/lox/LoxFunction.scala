package org.debian.people.eugmes.lox

class LoxFunction(name: Token, params: Seq[Token], body: Seq[Stmt]) extends LoxCallable {
  override def arity: Int = params.length

  override def call(interpreter: Interpreter, arguments: Seq[Any]): Any = {
    val environment = Environment(interpreter.globals)

    for (param, arg) <- params.zip(arguments) do
      environment.define(param.lexeme, arg)

    try
      interpreter.executeBlock(body, environment)
    catch
      case returnValue: Return => returnValue.value
  }

  override def toString: String = s"<fn ${name.lexeme}>"
}
