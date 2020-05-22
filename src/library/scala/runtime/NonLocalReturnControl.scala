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

package scala.runtime

import scala.util.control.ControlThrowable

// remove Unit specialization when binary compatibility permits
@annotation.nowarn
class NonLocalReturnControl[@specialized(Byte, Short, Int, Long, Char, Float, Double, Boolean, Unit) T](val key: AnyRef, val value: T) extends ControlThrowable {
  final override def fillInStackTrace(): Throwable = this

  // Until next re-STARR, suppressing the unit specialization warning (so we can `-Werror) is a bit
  // tricky.  When compiling locally, we're using STARR, so there's no warning.  On CI, we bootstrap,
  // and then the warning appears. The only way I can think of to have no warnings either way is to
  // have the blanket @nowarn, but then also intentionally put something warnable in. So that's
  // what this is doing here.  After next re-STARR, we can remove this, and then also change the
  // @nowarn above to be more specific: @annotation.nowarn("cat=lint-unit-specialization")
  0

}
