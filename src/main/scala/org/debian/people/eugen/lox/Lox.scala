package org.debian.people.eugen.lox

import scala.io.Source
import scala.io.StdIn.readLine
import scala.util.control.NonLocalReturns.{returning, throwReturn}

object Lox:
  private var hadError = false
  private var hadRuntimeError = false

  private val interpreter = Interpreter()

  private def runFile(fileName: String): Unit =
    val source = Source.fromFile(fileName)
    val text = try source.mkString finally source.close()
    run(text)
    if hadError then
      System.exit(65)
    if hadRuntimeError then
      System.exit(70)

  private def runPrompt(): Unit = returning {
    while true do
      print("> ")
      val line = readLine()
      if line == null then throwReturn(())

      run(line)
      hadError = false
  }

  private def run(source: String): Unit = returning {
    val scanner = Scanner(source)
    val tokens = scanner.scanTokens()
    val parser = Parser(tokens)
    val statements = parser.parse()

    if hadError then throwReturn(())

    val resolver = Resolver(interpreter)
    resolver.resolve(statements)

    if hadError then throwReturn(())

    interpreter.interpret(statements)
  }

  def error(line: Int, message: String): Unit = report(line, "", message)

  def error(token: Token, message: String): Unit =
    val where = if token.tokenType == TokenType.EOF then " at end" else s" at '${token.lexeme}'"
    report(token.line, where, message)

  def runtimeError(error: RuntimeError): Unit =
    Console.err.println(s"${error.getMessage}\n[line ${error.token.line}]")
    hadRuntimeError = true

  private def report(line: Int, where: String, message: String): Unit =
    Console.err.println(s"[line $line] Error$where: $message")
    hadError = true

  def main(args: Array[String]): Unit =
    if args.length > 1 then
      println("Usage: sclox [script]")
      System.exit(64)
    else if args.length == 1 then
      runFile(args(0))
    else
      runPrompt()
end Lox
