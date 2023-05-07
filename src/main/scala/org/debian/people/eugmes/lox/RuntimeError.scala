package org.debian.people.eugmes.lox

final class RuntimeError(val token: Token, message: String) extends RuntimeException(message)