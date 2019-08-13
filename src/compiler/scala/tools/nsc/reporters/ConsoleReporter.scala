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
import scala.reflect.internal.{Reporter => InternalReporter}

/** This class implements a Reporter that displays messages on a text console.
 */
class ConsoleReporter(val settings: Settings, val reader: BufferedReader, val writer: PrintWriter, val echoWriter: PrintWriter) extends AbstractReporter with PrintReporter with SummaryReporter {
  def this(settings: Settings) = this(settings, Console.in, new PrintWriter(Console.err, true), new PrintWriter(Console.out, true))
  def this(settings: Settings, reader: BufferedReader, writer: PrintWriter) = this(settings, reader, writer, writer)

  // limit errors and warnings; AbstractReporter counts them because of forced INFO special case
  override def display(pos: Position, msg: String, severity: Severity): Unit = {
    // the count includes the current message
    val ok = severity match {
      case InternalReporter.ERROR   => errorCount   <= settings.maxerrs.value
      case InternalReporter.WARNING => warningCount <= settings.maxwarns.value
      case _       => true
    }
    if (ok) super.display(pos, msg, severity)
  }
}
