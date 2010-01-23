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
class JLineReader(interpreter: Interpreter) extends InteractiveReader {
  def this() = this(null)

  override lazy val history = Some(History(consoleReader))
  override lazy val completion =
    if (interpreter == null) None
    else Some(new Completion(interpreter))

  val consoleReader = {
    val r = new jline.ConsoleReader()
    r setHistory (History().jhistory)
    r setBellEnabled false

    if (interpreter != null) {
      // have to specify all delimiters for completion to work nicely
      val delims = new ArgumentCompletor.AbstractArgumentDelimiter {
        // val delimChars = "(){}[],`;'\" \t".toArray
        val delimChars = "(){},`; \t".toArray
        def isDelimiterChar(s: String, pos: Int) = delimChars contains s.charAt(pos)
      }
      val comp = new ArgumentCompletor(completion.get, delims)
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

