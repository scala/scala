/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Stepan Koltsov
 */

package scala.tools.nsc
package interpreter

import java.io.{ BufferedReader, PrintWriter }
import session.NoHistory

/** Reads using standard JDK API */
class SimpleReader(
  in: BufferedReader,
  out: PrintWriter,
  val interactive: Boolean)
extends InteractiveReader
{
  val history = NoHistory
  val completion = NoCompletion
  val keyBindings: List[KeyBinding] = Nil

  def init() = ()
  def reset() = ()
  def redrawLine() = ()
  def currentLine = ""
  def readOneLine(prompt: String): String = {
    if (interactive) {
      out.print(prompt)
      out.flush()
    }
    in.readLine()
  }
}

object SimpleReader {
  def defaultIn  = Console.in
  def defaultOut = new PrintWriter(Console.out)

  def apply(in: BufferedReader = defaultIn, out: PrintWriter = defaultOut, interactive: Boolean = true): SimpleReader =
    new SimpleReader(in, out, interactive)
}