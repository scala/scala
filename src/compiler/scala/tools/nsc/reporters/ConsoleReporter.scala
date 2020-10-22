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
package tools.nsc
package reporters

import java.io.{BufferedReader, PrintWriter}
import scala.reflect.internal.util.Position

/** This class implements a Reporter that displays messages on a text console. */
class ConsoleReporter(val settings: Settings, val reader: BufferedReader, val writer: PrintWriter, val echoWriter: PrintWriter) extends FilteringReporter with PrintReporter {
  def this(settings: Settings) = this(settings, Console.in, new PrintWriter(Console.err, true), new PrintWriter(Console.out, true))
  def this(settings: Settings, reader: BufferedReader, writer: PrintWriter) = this(settings, reader, writer, writer)

  def doReport(pos: Position, msg: String, severity: Severity): Unit = display(pos, msg, severity)

  override def finish(): Unit = {
    import reflect.internal.util.StringOps.{countElementsAsString => countAs}
    if (!settings.nowarn && hasWarnings)
      echo(s"${countAs(warningCount, WARNING.toString.toLowerCase)} found")
    if (hasErrors)
      echo(s"${countAs(errorCount, ERROR.toString.toLowerCase)} found")
    super.finish()
  }
}
