/* NSC -- new Scala compiler
 * Copyright 2005-2017 LAMP/EPFL
 * @author Stepan Koltsov
 */

package scala.tools.nsc.interpreter.shell

import java.io.{BufferedReader, StringReader, PrintWriter => JPrintWriter}

/** Reads using standard JDK API. */
class SimpleReader(in: BufferedReader, out: JPrintWriter, val interactive: Boolean, val verbose: Boolean) extends InteractiveReader {
  val history = NoHistory
  val completion = NoCompletion

  def reset() = ()
  def redrawLine() = ()

  // InteractiveReader internals
  protected def readOneLine(prompt: String): String = {
    echo(prompt)

    val input = readOneLine()

    // pretend we are a console for verbose purposes
    // if there is more input, then echo the prompt and the input
    if (input != null && verbose) echo(f"$prompt$input%n")

    input
  }

  protected def readOneKey(prompt: String) = throw new IllegalStateException("No char-based input in SimpleReader")

  protected def readOneLine(): String = in.readLine()
  protected def echo(s: String): Unit = if (interactive) {
    out.print(s)
    out.flush()
  }
}

object SimpleReader {
  def defaultIn  = Console.in
  def defaultOut = new JPrintWriter(Console.out)

  def apply(in: BufferedReader = defaultIn, out: JPrintWriter = defaultOut, interactive: Boolean = true, verbose: Boolean = false): SimpleReader =
    new SimpleReader(in, out, interactive, verbose)

  // a non-interactive SimpleReader that returns the given text
  def apply(text: String): SimpleReader = apply(
    in  = new BufferedReader(new StringReader(text)),
    out = defaultOut,
    interactive = false
  )
}
