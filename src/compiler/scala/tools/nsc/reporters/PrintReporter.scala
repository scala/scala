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
import scala.reflect.internal.{Reporter => InternalReporter}
import scala.reflect.internal.util.{Position, StringOps}
import scala.tools.util.SystemExit

import Position.formatMessage
import StringOps.{trimAllTrailingSpace => trimTrailing}

/** Facility for outputting messages, with optional user intervention. */
trait PrintReporter extends InternalReporter {

  def settings: Settings
  def reader: BufferedReader
  def writer: PrintWriter
  def echoWriter: PrintWriter

  /** Whether a short file name should be displayed before errors */
  var shortname: Boolean = false

  private def clabel(severity: Severity): String = severity match {
    case InternalReporter.ERROR   => "error: "
    case InternalReporter.WARNING => "warning: "
    case _       => ""
  }

  /** Prints the warning or error message. */
  private def printMessage(msg: String): Unit = {
    writer.println(trimTrailing(msg))
    writer.flush()
    if (settings.prompt) displayPrompt()
  }

  /** Prints the message to the echoWriter, which is usually stdout. */
  private def echoMessage(msg: String): Unit = {
    echoWriter.println(trimTrailing(msg))
    echoWriter.flush()
  }

  /** Format a message and emit it. */
  protected def display(pos: Position, msg: String, severity: Severity): Unit = {
    val text = formatMessage(pos, s"${clabel(severity)}${Reporter.explanation(msg)}", shortname)
    severity match {
      case InternalReporter.INFO => echoMessage(text)
      case _    => printMessage(text)
    }
  }

  def displayPrompt(): Unit = {
    writer.println()
    writer.print("a)bort, s)tack, r)esume: ")
    writer.flush()
    if (reader != null) {
      Option(reader.readLine).flatMap(_.trim.headOption).getOrElse('r') match {
        case 'a' | 'A' =>
          new Throwable().printStackTrace(writer)
          throw SystemExit(1)
        case 's' | 'S' =>
          new Throwable().printStackTrace(writer)
          writer.println()
          writer.flush()
        case _ =>
      }
    } else writer.println("r")
  }

  override def flush(): Unit = {
    writer.flush()
    if (writer ne echoWriter) echoWriter.flush()
    super.flush()
  }

  def close(): Unit = {
    writer.close()
    if (writer ne echoWriter) echoWriter.close()
  }
}
