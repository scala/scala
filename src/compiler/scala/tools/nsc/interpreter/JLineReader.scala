/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author Stepan Koltsov
 */
// $Id$

package scala.tools.nsc
package interpreter

import java.io.File
import jline.{ History, ConsoleReader, ArgumentCompletor }

/** Reads from the console using JLine */
class JLineReader(interpreter: Interpreter) extends InteractiveReader {
  def this() = this(null)
  val consoleReader = {
    val history = try {
      new jline.History(new File(System.getProperty("user.home"), ".scala_history"))
    } catch {
      // do not store history if error
      case _ => new jline.History()
    }
    val r = new jline.ConsoleReader()
    r setHistory history
    r setBellEnabled false

    if (interpreter != null) {
      // have to specify all delimiters for completion to work nicely
      val delims = new ArgumentCompletor.AbstractArgumentDelimiter {
        val delimChars = "(){}[],`;'\" \t".toArray
        def isDelimiterChar(s: String, pos: Int) = delimChars contains s.charAt(pos)
      }
      val comp = new ArgumentCompletor(new Completion(interpreter), delims)
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

