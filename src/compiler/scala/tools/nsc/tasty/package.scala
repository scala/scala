/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc

import annotation.unchecked.uncheckedVariance

package object tasty {

  /** Adds equality operators asserting at compiletime that the RHS is a subtype of the LHS. */
  implicit final class SafeEq[-T](private val t: T @uncheckedVariance) extends AnyVal {
    @inline final def ===(u: T): Boolean = t == u
    @inline final def !==(u: T): Boolean = t != u
  }

  def cyan(str: String): String = Console.CYAN + str + Console.RESET
  def yellow(str: String): String = Console.YELLOW + str + Console.RESET
  def magenta(str: String): String = Console.MAGENTA + str + Console.RESET
  def red(str: String): String = Console.RED + str + Console.RESET
  def green(str: String): String = Console.GREEN + str + Console.RESET
  def blue(str: String): String = Console.BLUE + str + Console.RESET

}
