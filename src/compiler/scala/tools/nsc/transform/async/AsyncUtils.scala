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

package scala.tools.nsc.transform.async

object AsyncUtils {
  private def enabled(level: String) = sys.props.getOrElse(s"scala.async.$level", "false").equalsIgnoreCase("true")

  private[async] val verbose = enabled("debug")
  private[async] val trace   = enabled("trace")

  @inline private[async] def vprintln(s: => Any): Unit = if (verbose) println(s"[async] $s")

  @inline private[async] def trace(s: => Any): Unit = if (trace) println(s"[async] $s")
}
