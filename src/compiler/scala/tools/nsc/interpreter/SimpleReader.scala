/* NSC -- new Scala compiler
 * Copyright 2005-2007 LAMP/EPFL
 * @author Stepan Koltsov
 */
// $Id$

package scala.tools.nsc.interpreter
import java.io.{BufferedReader, PrintWriter}


/** Reads using standard JDK API */
class SimpleReader(
  in: BufferedReader,
  out: PrintWriter,
  val interactive: Boolean)
extends InteractiveReader {
  def this() = this(Console.in, new PrintWriter(Console.out), true)

  def readOneLine(prompt: String) = {
    if (interactive) {
      out.print(prompt)
      out.flush()
    }
    in.readLine()
  }
}
