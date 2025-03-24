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

package scala.tools.nsc.interactive

import java.io.Writer

class PrettyWriter(wr: Writer) extends Writer {
  protected val indentStep = "  "
  private var indent = 0
  private def newLine(): Unit = {
    wr.write('\n')
    wr.write(indentStep * indent)
  }
  def close() = wr.close()
  def flush() = wr.flush()
  def write(str: Array[Char], off: Int, len: Int): Unit = {
    if (off < str.length && off < len) {
      str(off) match {
        case '{' | '[' | '(' =>
          indent += 1
          wr.write(str(off).toInt)
          newLine()
          wr.write(str, off + 1, len - 1)
        case '}' | ']' | ')' =>
          wr.write(str, off, len)
          indent -= 1
        case ',' =>
          wr.write(',')
          newLine()
          wr.write(str, off + 1, len - 1)
        case ':' =>
          wr.write(':')
          wr.write(' ')
          wr.write(str, off + 1, len - 1)
        case _ =>
          wr.write(str, off, len)
      }
    } else {
      wr.write(str, off, len)
    }
  }
  override def toString = wr.toString
}
