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

object AdaptedArrowAssocWorkaround {
  // Factory for arbitrary-sized tuples, to avoid argument list adaptation
  // warnings for `->`. If there comes a point when that warning no longer
  // exists, then it is easy to remove all traces of this workaround using
  // find/replace, which would not be the case for wrapping all of the
  // affected tuples in extra sets of parentheses.
  object Tx {
    def apply[A, B](a:A, b: B): (A, B) = (a, b)
    def apply[A, B, C](a:A, b: B, c: C): (A, B, C) = (a, b, c)
    def apply[A, B, C, D](a:A, b: B, c: C, d: D): (A, B, C, D) = (a, b, c, d)
  }
}
