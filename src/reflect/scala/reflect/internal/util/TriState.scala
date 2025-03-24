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

package scala
package reflect
package internal
package util

import scala.language.implicitConversions

import TriState._

/** A simple true/false/unknown value, for those days when
 *  true and false don't quite partition the space.
 */
final class TriState private (val value: Int) extends AnyVal {
  def isKnown = this != Unknown
  def booleanValue = this match {
    case True  => true
    case False => false
    case _     => throw new IllegalStateException("Not a Boolean value")
  }
}

object TriState {
  implicit def booleanToTriState(b: Boolean): TriState = if (b) True else False

  val Unknown = new TriState(-1)
  val False   = new TriState(0)
  val True    = new TriState(1)
}
