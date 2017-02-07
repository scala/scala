/*
 * Zinc - The incremental compiler for Scala.
 * Copyright 2011 - 2017, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * This software is released under the terms written in LICENSE.
 */

package xsbt

object Log {
  def debug(log: xsbti.Logger, msg: => String) = log.debug(Message(msg))
  def settingsError(log: xsbti.Logger): String => Unit =
    s => log.error(Message(s))
}