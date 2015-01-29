/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Stepan Koltsov
 */

package scala
package tools.nsc
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

  def reset() = ()
  def redrawLine() = ()

  // InteractiveReader internals
  protected def readOneLine(prompt: String): String = {
    echo(prompt)
    readOneLine()
  }
  protected def readOneKey(prompt: String) = sys.error("No char-based input in SimpleReader")

  protected def readOneLine(): String = in.readLine()
  protected def echo(s: String): Unit = if (interactive) {
    out.print(s)
    out.flush()
  }
}

object SimpleReader {
  def defaultIn  = Console.in
  def defaultOut = new JPrintWriter(Console.out)

  def apply(in: BufferedReader = defaultIn, out: JPrintWriter = defaultOut, interactive: Boolean = true): SimpleReader =
    new SimpleReader(in, out, interactive)
}

// pretend we are a console for verbose purposes
trait EchoReader extends SimpleReader {
  // if there is more input, then maybe echo the prompt and the input
  override def readOneLine(prompt: String) = {
    val input = readOneLine()
    if (input != null) echo(f"$prompt$input%n")
    input
  }
}
