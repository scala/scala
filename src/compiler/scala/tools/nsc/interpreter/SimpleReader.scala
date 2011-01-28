/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Stepan Koltsov
 */

package scala.tools.nsc
package interpreter

import java.io.{ BufferedReader, PrintWriter }
import io.{ Path, File, Directory }

/** Reads using standard JDK API */
class SimpleReader(
  in: BufferedReader,
  out: PrintWriter,
  val interactive: Boolean)
extends InteractiveReader {
  def this() = this(Console.in, new PrintWriter(Console.out), true)
  def this(in: File, out: PrintWriter, interactive: Boolean) = this(in.bufferedReader(), out, interactive)

  lazy val history = History.Empty
  lazy val completion = Completion.Empty

  def init() = ()
  def reset() = ()
  def readOneLine(prompt: String): String = {
    if (interactive) {
      out.print(prompt)
      out.flush()
    }
    in.readLine()
  }
}
