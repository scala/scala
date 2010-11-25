/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Stepan Koltsov
 */

package scala.tools.nsc
package interpreter

import java.io.File
import jline.{ ConsoleReader, ArgumentCompletor, History => JHistory }
// import scala.tools.util.SignalManager

/** Reads from the console using JLine */
class JLineReader(interpreter: Interpreter) extends InteractiveReader {
  def this() = this(null)

  override lazy val history    = Some(History(consoleReader))
  override lazy val completion = Option(interpreter) map (x => new Completion(x))
  override def init()          = consoleReader.getTerminal().initializeTerminal()

  /** Requires two interrupt signals within three seconds
   *  of one another to initiate exit.
   */
  // SignalManager.requireInterval(3, SignalManager.INT) {
  //   case true   => Console.println("\nPress ctrl-C again to exit.")
  //   case false  => System.exit(1)
  // }

  val consoleReader = {
    val r = new jline.ConsoleReader()
    r setHistory (History().jhistory)
    r setBellEnabled false
    completion foreach { c =>
      r addCompletor c.jline
      r setAutoprintThreshhold 250
    }

    r
  }

  def readOneLine(prompt: String) = consoleReader readLine prompt
  val interactive = true
}

