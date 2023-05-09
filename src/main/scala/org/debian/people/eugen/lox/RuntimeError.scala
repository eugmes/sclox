package org.debian.people.eugen.lox

final class RuntimeError(val token: Token, message: String) extends RuntimeException(message)