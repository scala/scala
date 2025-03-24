/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc.interpreter.shell

import java.io.{BufferedReader, StringReader, PrintWriter => JPrintWriter}

/** Reads using standard JDK API. */
class SimpleReader(in: BufferedReader, out: JPrintWriter, val completion: Completion, val interactive: Boolean, val verbose: Boolean) extends InteractiveReader {
  val history = NoHistory
  val accumulator = new Accumulator

  override def reset() = accumulator.reset()
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

  protected def readOneLine(): String = in.readLine()
  protected def echo(s: String): Unit = if (interactive) {
    out.print(s)
    out.flush()
  }

  override def close(): Unit = ()
}

object SimpleReader {
  def defaultIn  = Console.in
  def defaultOut = new JPrintWriter(Console.out)

  def apply(in: BufferedReader = defaultIn, out: JPrintWriter = defaultOut, completion: Completion = NoCompletion, interactive: Boolean = true, verbose: Boolean = false): SimpleReader =
    new SimpleReader(in, out, completion, interactive, verbose)

  // a non-interactive SimpleReader that returns the given text
  def apply(text: String): SimpleReader = apply(
    in  = new BufferedReader(new StringReader(text)),
    out = defaultOut,
    interactive = false
  )
}
