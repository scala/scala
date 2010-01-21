/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Stepan Koltsov
 */
// $Id$

package scala.tools.nsc
package interpreter

import java.io.File
import jline.{ ConsoleReader, ArgumentCompletor, History => JHistory }

/** Reads from the console using JLine */
class JLineReader(interpreter: Interpreter, intLoop: InterpreterLoop) extends InteractiveReader {
  def this() = this(null, null)
  def this(interpreter: Interpreter) = this(interpreter, null)
  override def history = Some(new History(consoleReader.getHistory))

  val consoleReader = {
    val history =
      try new JHistory(new File(System.getProperty("user.home"), ".scala_history"))
      // do not store history if error
      catch { case _: Exception => new JHistory() }

    val r = new jline.ConsoleReader()
    r setHistory history
    r setBellEnabled false

    if (interpreter != null) {
      // have to specify all delimiters for completion to work nicely
      val delims = new ArgumentCompletor.AbstractArgumentDelimiter {
        val delimChars = "(){}[],`;'\" \t".toArray
        def isDelimiterChar(s: String, pos: Int) = delimChars contains s.charAt(pos)
      }
      val comp = new ArgumentCompletor(new Completion(interpreter, intLoop), delims)
      comp setStrict false
      r addCompletor comp
      // XXX make this use a setting
      r setAutoprintThreshhold 250
    }

    r
  }

  def readOneLine(prompt: String) = consoleReader readLine prompt
  val interactive = true
}

