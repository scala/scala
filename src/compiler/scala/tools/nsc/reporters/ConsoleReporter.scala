/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.nsc.reporters

import compat.StringBuilder
import scala.tools.nsc.util.{FakePos, Position}

import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.IOException
import java.io.PrintWriter

/**
 * This class implements a Reporter that displays messages on a text
 * console.
 */
class ConsoleReporter(reader : BufferedReader, writer : PrintWriter) extends AbstractReporter {
  //########################################################################
  // Public Fields

  /** Whether a short file name should be displayed before errors */
  var shortname : Boolean = false

  def label(severity : Severity): String = severity match {
    case ERROR   => "error"
    case WARNING => "warning"
    case INFO    => null
  }
  def clabel(severity : Severity) = {
    val label0 = label(severity)
    if (label0 == null) "" else label0 + ": "
  }



  //########################################################################
  // Public Constructors
  def this() = this(Console.in, new PrintWriter(Console.err, true))

  //########################################################################
  // Public Methods - Count


  /** Returns the number of errors issued totally as a string */
  def getCountString(severity : Severity) : String = getCountString0(count(severity), label(severity))
  /** Returns a string meaning "n elements". */
  def getCountString0(n : Int, elements : String) : String =
    n match {
      case 0 => "no "    + elements + "s"
      case 1 => "one "   + elements
      case 2 => "two "   + elements + "s"
      case 3 => "three " + elements + "s"
      case 4 => "four "  + elements + "s"
      case _ => "" + n + " " + elements + "s"
    }


  //########################################################################
  // Public Methods - Print

  /** Prints the message. */
  def printMessage(msg : String) = writer.println(msg)

  /** Prints the message with the given position indication. */
  def printMessage(posIn : Position, msg : String) : Unit = {
    if (posIn != null) {
      val pos = posIn.inUltimateSource
      val buf = new StringBuilder(msg)
      buf.insert(0, " ")
      if (pos.line != Position.NOLINE)
	buf.insert(0, ":" + pos.line)
      pos match {
        case FakePos(msg) =>
          buf.insert(0,msg)
        case _ =>
          val file = pos.source.file
          buf.insert(0, if (shortname) file.name else file.path)
      }
      printMessage(buf.toString())
      printSourceLine(pos)
    } else printMessage(msg)
  }

  def print(pos : Position, msg : String, severity : Severity) = printMessage(pos, clabel(severity) + msg)

  def printSourceLine(pos : Position) = if (pos != null && pos.offset != Position.NOPOS) {
    printMessage(pos.lineContent.stripLineEnd)
    printColumnMarker(pos)
  }
  /** Prints the column marker of the given position. */
  def printColumnMarker(pos : Position) = if (pos != null) {
    val buffer = new StringBuilder(pos.column)
    var i = 1
    while (i < pos.column) {
      buffer.append(' ')
      i = i + 1
    }
    if (pos.column > 0) buffer.append('^')
    printMessage(buffer.toString())
  }

  /** Prints the number of errors and warnings if their are non-zero. */
  def printSummary() = {
    if (warnings > 0) printMessage(getCountString(WARNING) + " found")
    if (  errors > 0) printMessage(getCountString(ERROR  ) + " found")
  }

  //########################################################################
  // Public Methods - Display
  def display(pos : Position, msg : String, severity : Severity) : Unit = {
    incr(severity)
    print(pos, msg, severity)
  }
  def displayPrompt : Unit = try {
    var continue = true
    while (continue) {
      writer.print("r)esume, a)bort: ")
      writer.flush()
      var line = reader.readLine()
      if (line != null) {
	line = line.toLowerCase()
	if ("abort".startsWith(line))
            throw new Error("user abort")
	if ("resume".startsWith(line)) continue = false
      }
    }
  } catch {
    case ex: IOException => {
      ex.printStackTrace()
      throw new Error("input read error")
    }
  }
}
