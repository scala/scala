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

package scala.tools.nsc.interpreter.shell

/** Support for adding to history and retrieving it.
 */
trait History {
  def historicize(text: String): Boolean = false

  def asStrings: List[String]
  def asStrings(from: Int, to: Int): List[String] = asStrings.slice(from, to)
  def index: Int
  def size: Int
}
object NoHistory extends History {
  def asStrings       = Nil
  def index           = 0
  def size            = 0
}
