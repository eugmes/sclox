package lox

final class Return(val value: LoxValue) extends RuntimeException(null, null, false, false)