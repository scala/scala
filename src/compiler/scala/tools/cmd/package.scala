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
  // make some language features in this package compile without warning
  implicit def implicitConversions = scala.language.implicitConversions
  implicit def postfixOps = scala.language.postfixOps

  def toArgs(line: String): List[String]   = CommandLineParser tokenize line
  def fromArgs(args: List[String]): String = args mkString " "
}
