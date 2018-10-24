/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package tools.nsc
package interpreter

import java.io.{BufferedReader, StringReader}
import session.NoHistory

/** Reads using standard JDK API. */
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

  // a non-interactive SimpleReader that returns the given text
  def apply(text: String): SimpleReader = apply(
    in  = new BufferedReader(new StringReader(text)),
    out = defaultOut,
    interactive = false
  )
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
