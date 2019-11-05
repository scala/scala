/*
 * Zinc - The incremental compiler for Scala.
 * Copyright Lightbend, Inc. and Mark Harrah
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package xsbt

object Log {
  def debug(log: xsbti.Logger, msg: => String) = log.debug(Message(msg))
  def settingsError(log: xsbti.Logger): String => Unit =
    s => log.error(Message(s))
}
