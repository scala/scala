/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Stepan Koltsov
 */

package scala.tools.nsc
package interpreter

import java.io.{ BufferedReader }
import session.NoHistory

/** Reads using standard JDK API */
class SimpleReader(
  in: BufferedReader,
  out: JPrintWriter,
  val interactive: Boolean)
extends InteractiveReader
{
  val history = NoHistory
  val completion = NoCompletion

  def init() = ()
  def reset() = ()
  def eraseLine() = ()
  def redrawLine() = ()
  def currentLine = ""
  def readOneLine(prompt: String): String = {
    if (interactive) {
      out.print(prompt)
      out.flush()
    }
    in.readLine()
  }
  def readOneKey(prompt: String)  = sys.error("No char-based input in SimpleReader")
}

object SimpleReader {
  def defaultIn  = Console.in
  def defaultOut = new JPrintWriter(Console.out)

  def apply(in: BufferedReader = defaultIn, out: JPrintWriter = defaultOut, interactive: Boolean = true): SimpleReader =
    new SimpleReader(in, out, interactive)
}