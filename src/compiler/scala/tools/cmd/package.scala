/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package tools

package object cmd {
  def returning[T](x: T)(f: T => Unit): T = { f(x) ; x }

  // make some language features in this package compile without warning
  implicit def implicitConversions = scala.language.implicitConversions
  implicit def postfixOps = scala.language.postfixOps

  private[cmd] def debug(msg: String): Unit = println(msg)

  def runAndExit(body: => Unit): Nothing = {
    body
    sys.exit(0)
  }

  def toOpt(s: String): String             = if (s startsWith "--") s else "--" + s
  def fromOpt(s: String): String           = s stripPrefix "--"
  def toArgs(line: String): List[String]   = CommandLineParser tokenize line
  def fromArgs(args: List[String]): String = args mkString " "

  def stripQuotes(s: String): String = {
    def isQuotedBy(c: Char) = s.length > 0 && s.head == c && s.last == c
    if (List('"', '\'') exists isQuotedBy) s.tail.init else s
  }
}
