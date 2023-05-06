package org.debian.people.eugmes.lox

import scala.io.Source
import scala.io.StdIn.readLine

object Lox {
  private var hadError = false

  private def runFile(fileName: String): Unit = {
    val source = Source.fromFile(fileName)
    val text = try source.mkString finally source.close()
    run(text)
    if (hadError) {
      System.exit(65)
    }
  }

  private def runPrompt(): Unit = {
    while (true) {
      print("> ")
      val line = readLine()
      if (line == null) {
        return
      }
      run(line)
      hadError = false
    }
  }

  private def run(source: String): Unit = {
    val scanner = Scanner(source)
    val tokens = scanner.scanTokens()

    for (token <- tokens) {
      println(token)
    }
  }

  def error(line: Int, message: String): Unit = {
    report(line, "", message)
  }

  private def report(line: Int, where: String, message: String): Unit = {
    Console.err.println(s"[line $line] Error$where: $message")
    hadError = true
  }

  def main(args: Array[String]): Unit = {
    if (args.length > 1) {
      println("Usage: sclox [script]")
      System.exit(64)
    } else if (args.length == 1) {
      runFile(args(0))
    } else {
      runPrompt()
    }
  }
}
