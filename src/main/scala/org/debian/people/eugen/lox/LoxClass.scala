package org.debian.people.eugen.lox

final class LoxClass(val name: String, methods: Map[String, LoxFunction]) extends LoxCallable:
  override def call(interpreter: Interpreter, arguments: Seq[LoxValue]): LoxValue =
    val instance = LoxInstance(this)
    findMethod("init").map(initializer => initializer.bind(instance).call(interpreter, arguments))
    instance

  override def arity: Int =
    findMethod("init") match
      case Some(initializer) => initializer.arity
      case None => 0

  override def toString: String = name

  def findMethod(name: String): Option[LoxFunction] = methods.get(name)

end LoxClass
