/* NSC -- new Scala compiler
 * Copyright 2002-2013 LAMP/EPFL
 * @author Martin Odersky
 */

package scala
package tools.nsc
package reporters

import java.io.{BufferedReader, PrintWriter}
import scala.reflect.internal.util.Position

/** This class implements a Reporter that displays messages on a text console.
 */
class ConsoleReporter(val settings: Settings, val reader: BufferedReader, val writer: PrintWriter, val echoWriter: PrintWriter) extends AbstractReporter with PrintReporter with SummaryReporter {
  def this(settings: Settings) = this(settings, Console.in, new PrintWriter(Console.err, true), new PrintWriter(Console.out, true))
  def this(settings: Settings, reader: BufferedReader, writer: PrintWriter) = this(settings, reader, writer, writer)

  // limit errors and warnings; AbstractReporter counts them because of forced INFO special case
  override def display(pos: Position, msg: String, severity: Severity): Unit = {
    // the count includes the current message
    val ok = severity match {
      case ERROR   => ERROR.count   <= settings.maxerrs.value
      case WARNING => WARNING.count <= settings.maxwarns.value
      case _       => true
    }
    if (ok) super.display(pos, msg, severity)
  }
}
