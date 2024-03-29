package lox

import scala.annotation.tailrec
import scala.collection.mutable

type LoxValue = LiteralValue | LoxCallable | LoxInstance

final class Environment(val enclosing: Environment = null):
  private val values: mutable.HashMap[String, LoxValue] = mutable.HashMap()

  def define(name: String, value: LoxValue): Unit = values.put(name, value)

  def get(name: Token): LoxValue =
    values.getOrElse(name.lexeme,
      if enclosing != null
      then enclosing.get(name)
      else throw RuntimeError(name, s"Undefined variable '${name.lexeme}'."))

  @tailrec
  def assign(name: Token, value: LoxValue): Unit =
    if values.contains(name.lexeme) then
      values.put(name.lexeme, value)
    else if enclosing != null then
      enclosing.assign(name, value)
    else
      throw RuntimeError(name, s"Undefined variable '${name.lexeme}'.")

  def getAt(distance: Int, name: String): Option[LoxValue] = ancestor(distance).values.get(name)

  def assignAt(distance: Int, name: Token, value: LoxValue): Unit = ancestor(distance).values.put(name.lexeme, value)

  private def ancestor(distance: Int): Environment =
    (1 to distance).foldLeft(this)((env, _) => env.enclosing)

end Environment
