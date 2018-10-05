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
package runtime

import scala.compat.Platform.EOL

@deprecated("use Throwable#getStackTrace", "2.11.0")
final class RichException(exc: Throwable) {
  def getStackTraceString = exc.getStackTrace().mkString("", EOL, EOL)
}
