/* NSC -- new Scala compiler
 * Copyright 2002-2009 LAMP/EPFL
 * @author Martin Odersky
 */
// $Id$

package scala.tools.nsc
package reporters

import java.io.{BufferedReader, InputStreamReader, IOException, PrintWriter}
import util._

/**
 * This class implements a Reporter that displays messages on a text
 * console.
 */
class ConsoleReporter(val settings: Settings, reader: BufferedReader, writer: PrintWriter) extends AbstractReporter {
  /** Whether a short file name should be displayed before errors */
  var shortname: Boolean = false

  /** maximal number of error messages to be printed */
  final val ERROR_LIMIT = 100

  private def label(severity: Severity): String = severity match {
    case ERROR   => "error"
    case WARNING => "warning"
    case INFO    => null
  }

  private def clabel(severity: Severity): String = {
    val label0 = label(severity)
    if (label0 eq null) "" else label0 + ": "
  }

  def this(settings: Settings) =
    this(settings, Console.in, new PrintWriter(Console.err, true))

  /** Returns the number of errors issued totally as a string.
   *
   *  @param severity ...
   *  @return         ...
   */
  private def getCountString(severity: Severity): String =
    countElementsAsString((severity).count, label(severity))

  /** Prints the message. */
  //def printMessage(msg: String) { writer.println(msg) }  // platform-dependent!
  def printMessage(msg: String) { writer.print(msg + "\n"); writer.flush() }

  /** Prints the message with the given position indication. */
  def printMessage(posIn: Position, msg: String) {
    if (posIn ne null) {
      val pos = posIn.inUltimateSource(posIn.source.getOrElse(null))
      val buf = new StringBuilder(msg)
      if (!pos.source.isEmpty) {
        buf.insert(0, " ")
        buf.insert(0, pos.line.map(ln => ":" + pos.line.get + ":").getOrElse(":"))
      }
      //println(getSource.file)
      pos match {
        case FakePos(msg) =>
          buf.insert(0, msg + " ")
        case _ if !pos.source.isEmpty =>
          val file = pos.source.get.file
          buf.insert(0, if (shortname) file.name else file.path)
        case _ =>
      }
      printMessage(buf.toString())
      if (!pos.line.isEmpty)
        printSourceLine(pos)
    } else
      printMessage(msg)
  }

  def print(pos: Position, msg: String, severity: Severity) {
    printMessage(pos, clabel(severity) + msg)
  }

  /**
   *  @param pos ...
   */
  def printSourceLine(pos: Position) {
    printMessage(pos.lineContent.stripLineEnd)
    printColumnMarker(pos)
  }

  /** Prints the column marker of the given position.
   *
   *  @param pos ...
   */
  def printColumnMarker(pos: Position) = if (!pos.column.isEmpty) {
    val buffer = new StringBuilder(pos.column.get)
    var i = 1
    while (i < pos.column.get) {
      buffer.append(' ')
      i += 1
    }
    if (pos.column.get > 0) buffer.append('^')
    printMessage(buffer.toString())
  }

  /** Prints the number of errors and warnings if their are non-zero. */
  def printSummary() {
    if (WARNING.count > 0) printMessage(getCountString(WARNING) + " found")
    if (  ERROR.count > 0) printMessage(getCountString(ERROR  ) + " found")
  }

  def display(pos: Position, msg: String, severity: Severity) {
    severity.count += 1
    if (severity != ERROR || severity.count <= ERROR_LIMIT)
      print(pos, msg, severity)
  }

  def displayPrompt: Unit = try {
    var continue = true
    while (continue) {
      writer.print("r)esume, a)bort: ")
      writer.flush()
      var line = reader.readLine()
      if (line ne null) {
	line = line.toLowerCase()
	if ("abort" startsWith line)
            throw new Error("user abort")
	if ("resume" startsWith line) continue = false
      }
    }
  } catch {
    case ex: IOException => {
      ex.printStackTrace()
      throw new Error("input read error")
    }
  }

  override def flush() { writer.flush() }
}
