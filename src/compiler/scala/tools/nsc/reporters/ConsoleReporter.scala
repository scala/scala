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

package scala
package tools.nsc
package reporters

import java.io.{BufferedReader, PrintWriter}
import scala.reflect.internal.util.{CodeAction, Position}

/** This class implements a Reporter that displays messages on a text console. */
class ConsoleReporter(val settings: Settings, val reader: BufferedReader, val writer: PrintWriter, val echoWriter: PrintWriter) extends FilteringReporter with PrintReporter {
  def this(settings: Settings) = this(settings, Console.in, new PrintWriter(Console.err, true), new PrintWriter(Console.out, true))
  def this(settings: Settings, reader: BufferedReader, writer: PrintWriter) = this(settings, reader, writer, writer)

  override def doReport(pos: Position, msg: String, severity: Severity, actions: List[CodeAction]): Unit = display(pos, msg, severity)

  override def finish(): Unit = {
    import reflect.internal.util.StringOps.countElementsAsString
    if (hasWarnings && !settings.nowarn.value)
      writer.println(countElementsAsString(warningCount, WARNING.toString.toLowerCase))
    if (hasErrors)
      writer.println(countElementsAsString(errorCount, ERROR.toString.toLowerCase))
    writer.flush()
    super.finish()
  }
}
