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
import scala.language.implicitConversions

/** Files having to do with the state of a repl session:
 *  lines of text entered, types and terms defined, etc.
 */
package object session {
  type JIterator[T]       = java.util.Iterator[T]
  type JListIterator[T]   = java.util.ListIterator[T]

  private[interpreter] implicit def charSequenceFix(x: CharSequence): String = x.toString
}
