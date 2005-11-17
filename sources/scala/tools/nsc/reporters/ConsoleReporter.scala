/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.nsc.reporters;
import scala.tools.nsc.util.Position;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.PrintWriter;

/**
 * This class implements a Reporter that displays messages on a text
 * console.
 */
class ConsoleReporter(_reader : BufferedReader, _writer : PrintWriter) extends AbstractReporter {

  //########################################################################
  // Private Fields

  /** The reader to ask for failures on demand */
  private val reader = _reader;
  /** The writer to print messages */
  private val writer = _writer;


  //########################################################################
  // Public Fields

  /** Whether a short file name should be displayed before errors */
  var shortname : Boolean = false;

  //########################################################################
  // Public Constructors
  def this() = this(
    new BufferedReader(new InputStreamReader(System.in)),
    new PrintWriter(System.err, true));

  //########################################################################
  // Public Methods - Count

  /** Returns the number of errors issued totally as a string */
  def getErrorCountString = getCountString(errorsx, "error");

    /** Returns the number of warnings issued totally as a string */
  def getWarningCountString = getCountString(warningsx, "warning");

  /** Returns a string meaning "n elements". */
  def getCountString(n : Int, elements : String) : String =
    n match {
      case 0 => "no "    + elements + "s";
      case 1 => "one "   + elements;
      case 2 => "two "   + elements + "s";
      case 3 => "three " + elements + "s";
      case 4 => "four "  + elements + "s";
      case _ => "" + n + " " + elements + "s";
    }


  //########################################################################
  // Public Methods - Print

  /** Prints the message. */
  def printMessage(msg : String) = writer.println(msg);

  /** Prints the message with the given position indication. */
  def printMessage(pos : Position, msg : String) : Unit = {
    if (pos != null) {
      val buf = new StringBuffer(msg);
      buf.insert(0, " ");
      if (pos.line != Position.NOLINE)
	buf.insert(0, ":" + pos.line);
      val file = pos.source.file;
      buf.insert(0, if (shortname) file.getName() else file.getPath());
      printMessage(buf.toString());
      printSourceLine(pos);
    } else printMessage(msg);
  }

  def printWarning(pos : Position, msg : String) = printMessage(pos, "warning: " + msg);
  def printError  (pos : Position, msg : String) = printMessage(pos,   "error: " + msg);

  def printSourceLine(pos : Position) = if (pos != null && pos.offset != Position.NOPOS) {
    printMessage(pos.lineContent);
    printColumnMarker(pos);
  }
  /** Prints the column marker of the given position. */
  def printColumnMarker(pos : Position) = if (pos != null) {
    val buffer = new StringBuffer(pos.column);
    var i = 1;
    while (i < pos.column) {
      buffer.append(' ');
      i = i + 1;
    }
    if (pos.column > 0) buffer.append('^');
    printMessage(buffer.toString());
  }

  /** Prints the number of errors and warnings if their are non-zero. */
  def printSummary() = {
    if (warningsx > 0) printMessage(getWarningCountString + " found");
    if (  errorsx > 0) printMessage(getErrorCountString   + " found");
  }

  //########################################################################
  // Public Methods - Display
  def displayInfo   (pos : Position, msg : String) : Unit = printMessage(pos, msg);
  def displayWarning(pos : Position, msg : String) : Unit = printWarning(pos, msg);
  def displayError  (pos : Position, msg : String) : Unit = printError  (pos, msg);

  def displayPrompt : Unit = try {
    var continue = true;
    while (continue) {
      writer.print("r)esume, a)bort: ");
      writer.flush();
      var line = reader.readLine();
      if (line != null) {
	line = line.toLowerCase();
	if ("abort".startsWith(line))
            throw new Error("user abort");
	if ("resume".startsWith(line)) continue = false;
      }
    }
  } catch {
    case ex: IOException => {
      ex.printStackTrace();
      throw new Error("input read error");
    }
  }
}
