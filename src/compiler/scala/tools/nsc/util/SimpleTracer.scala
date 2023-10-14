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
package util

// todo: We should unify this with Tracer. I'd do it but Tracer is
// too complicated for me to understand quickly.
import java.io.PrintStream

/** A simple tracer
 *  @param out The print stream where trace info should be sent
 *  @param enabled A condition that must be true for trace info to be produced.
 */
class SimpleTracer(out: PrintStream, enabled: Boolean = true) {
  def apply[T](msg: => String)(value: T): value.type = {
    if (enabled) out.println(msg+value)
    value
  }
  def when(enabled: Boolean): SimpleTracer = new SimpleTracer(out, enabled)
}
