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

package scala.tools.nsc
package interpreter
package session

/** An implementation-agnostic history interface which makes no
 *  reference to the jline classes.  Very sparse right now.
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
