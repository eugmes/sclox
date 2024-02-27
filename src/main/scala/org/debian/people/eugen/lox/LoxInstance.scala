package org.debian.people.eugen.lox

import scala.collection.mutable
import scala.util.boundary, boundary.break

final class LoxInstance(klass: LoxClass):
  private val fields = mutable.HashMap[String, LoxValue]()

  override def toString: String = s"${klass.name} instance"

  def get(name: Token): LoxValue = boundary:
    fields.get(name.lexeme).map(break)
    klass.findMethod(name.lexeme) match
      case Some(method) => method.bind(this)
      case None => throw RuntimeError(name, s"Undefined property '${name.lexeme}'.'")

  def set(name: Token, value: LoxValue): Unit = fields.put(name.lexeme, value)

end LoxInstance
