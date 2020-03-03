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

package scala.tools.nsc
package util

import scala.reflect.internal.Chars._

trait CharArrayReaderData {
  /** the last read character */
  var ch: Char = _

  /** The offset one past the last read character */
  var charOffset: Int = 0

  /** The start offset of the current line */
  var lineStartOffset: Int = 0

  /** The start offset of the line before the current one */
  var lastLineStartOffset: Int = 0

  def copyFrom(cd: CharArrayReaderData): this.type = {
    this.ch = cd.ch
    this.charOffset = cd.charOffset
    this.lineStartOffset = cd.lineStartOffset
    this.lastLineStartOffset = cd.lastLineStartOffset
    this
  }
}

abstract class CharArrayReader extends CharArrayReaderData { self =>

  val buf: Array[Char]

  /** Advance one character; reducing CR;LF pairs to just LF */
  final def nextChar(): Unit = {
    if (charOffset >= buf.length) {
      ch = SU
    } else {
      val c = buf(charOffset)
      ch = c
      charOffset += 1
      if (ch < ' ') {
        skipCR()
        potentialLineEnd()
      }
    }
  }

  /** Advance one character, leaving CR;LF pairs intact.
   *  This is for use in multi-line strings, so there are no
   *  "potential line ends" here.
   */
  final def nextRawChar(): Unit = {
    if (charOffset >= buf.length) {
      ch = SU
    } else {
      val c = buf(charOffset)
      ch = c
      charOffset += 1
    }
  }

  /** replace CR;LF by LF */
  private def skipCR() =
    if (ch == CR && charOffset < buf.length)
      buf(charOffset) match {
        case LF =>
          charOffset += 1
          ch = LF
        case _ =>
      }

  /** Handle line ends */
  private def potentialLineEnd(): Unit = {
    if (ch == LF || ch == FF) {
      lastLineStartOffset = lineStartOffset
      lineStartOffset = charOffset
    }
  }

  /** A new reader that takes off at the current character position */
  def lookaheadReader = new CharArrayLookaheadReader

  class CharArrayLookaheadReader extends CharArrayReader {
    val buf = self.buf
    charOffset = self.charOffset
    ch = self.ch
    /** A mystery why CharArrayReader.nextChar() returns Unit */
    def getc() = { nextChar() ; ch }
  }
}
