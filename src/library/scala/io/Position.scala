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

package scala
package io

import annotation.nowarn

/** The object Position provides convenience methods to encode
 *  line and column number in one single integer.  The encoded line
 *  (column) numbers range from 0 to `LINE_MASK` (`COLUMN_MASK`),
 *  where `0` indicates that the line (column) is undefined and
 *  `1` represents the first line (column).
 *
 *  Line (Column) numbers greater than `LINE_MASK` (`COLUMN_MASK`) are
 *  replaced by `LINE_MASK` (`COLUMN_MASK`). Furthermore, if the encoded
 *  line number is `LINE_MASK`, the column number is always set to 0.
 *
 *  The following properties hold:
 *
 *  the undefined position is 0:   `encode(0,0) == 0`
 *  encodings are non-negative :   `encode(line,column) >= 0`
 *  position order is preserved:
 *  {{{
 *  (line1 <= line2) || (line1 == line2 && column1 <= column2)
 *  }}}
 *  implies
 *  {{{
 *  encode(line1,column1) <= encode(line2,column2)
 *  }}}
 */
@deprecated("this class will be removed", "2.10.0")
private[scala] abstract class Position {
  /** Definable behavior for overflow conditions.
   */
  def checkInput(line: Int, column: Int): Unit

  /** Number of bits used to encode the line number */
  final val LINE_BITS   = 20
  /** Number of bits used to encode the column number */
  final val COLUMN_BITS = 31 - LINE_BITS // no negatives => 31
  /** Mask to decode the line number */
  final val LINE_MASK   = (1 << LINE_BITS) - 1
  /** Mask to decode the column number */
  final val COLUMN_MASK = (1 << COLUMN_BITS) - 1

  /** Encodes a position into a single integer. */
  final def encode(line: Int, column: Int): Int = {
    checkInput(line, column)

    if (line >= LINE_MASK)
      LINE_MASK << COLUMN_BITS
    else
      (line << COLUMN_BITS) | scala.math.min(COLUMN_MASK, column)
  }

  /** Returns the line number of the encoded position. */
  final def line(pos: Int): Int = (pos >> COLUMN_BITS) & LINE_MASK

  /** Returns the column number of the encoded position. */
  final def column(pos: Int): Int = pos & COLUMN_MASK

  /** Returns a string representation of the encoded position. */
  def toString(pos: Int): String = line(pos) + ":" + column(pos)
}

@nowarn
private[scala] object Position extends Position {
  def checkInput(line: Int, column: Int): Unit = {
    if (line < 0)
      throw new IllegalArgumentException(s"$line < 0")
    if (line == 0 && column != 0 || column < 0)
      throw new IllegalArgumentException(s"$line,$column not allowed")
  }
}
