package org.debian.people.eugmes.lox

import scala.collection.mutable
import scala.util.control.NonLocalReturns.{returning, throwReturn}

final class LoxInstance(klass: LoxClass):
  private val fields = mutable.HashMap[String, LoxValue]()

  override def toString: String = s"${klass.name} instance"

  def get(name: Token): LoxValue = returning {
    fields.get(name.lexeme).map(throwReturn)
    klass.findMethod(name.lexeme) match
      case Some(method) => method.bind(this)
      case None => throw RuntimeError(name, s"Undefined property '${name.lexeme}'.'")
  }

  def set(name: Token, value: LoxValue): Unit = fields.put(name.lexeme, value)

end LoxInstance
