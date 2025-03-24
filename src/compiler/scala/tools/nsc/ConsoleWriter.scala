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

package scala.tools.nsc

import java.io.Writer

/** A Writer that writes onto the Scala Console.
 *
 *  @author  Lex Spoon
 */
class ConsoleWriter extends Writer {
  def close() = flush()

  def flush() = Console.flush()

  def write(cbuf: Array[Char], off: Int, len: Int): Unit = {
    if (len > 0)
      write(new String(cbuf.slice(off, off+len)))
  }

  override def write(str: String): Unit = { Console.print(str) }
}
