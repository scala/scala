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

package scala.collection
package generic


/** Some precomputed common errors to reduce the generated code size.
  */
private[collection] object CommonErrors {
  /** IndexOutOfBounds exception with a known max index */
  @noinline
  def indexOutOfBounds(index: Int, max: Int): IndexOutOfBoundsException = 
    new IndexOutOfBoundsException(s"$index is out of bounds (min 0, max ${max})")

  /** IndexOutOfBounds exception with an unknown max index. */
  @noinline
  def indexOutOfBounds(index: Int): IndexOutOfBoundsException = 
    new IndexOutOfBoundsException(s"$index is out of bounds (min 0, max unknown)")
}
