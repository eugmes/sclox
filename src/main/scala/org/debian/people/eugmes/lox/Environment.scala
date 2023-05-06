package org.debian.people.eugmes.lox

import scala.collection.mutable

class Environment(enclosing: Environment = null) {
  private val values: mutable.HashMap[String, Any] = mutable.HashMap()

  def define(name: String, value: Any): Unit = {
    values.put(name, value)
  }

  def get(name: Token): Any = {
    values.getOrElse(name.lexeme,
      () =>
        if enclosing != null
        then enclosing.get(name)
        else throw RuntimeError(name, s"Undefined variable '${name.lexeme}'."))
  }

  def assign(name: Token, value: Any): Unit = {
    if (values.contains(name.lexeme)) {
      values.put(name.lexeme, value)
    } else if (enclosing != null) {
      enclosing.assign(name, value)
    } else {
      throw RuntimeError(name, s"Undefined variable '${name.lexeme}'.")
    }
  }
}
