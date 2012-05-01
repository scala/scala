/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.runtime

import java.util.Arrays.copyOfRange

final class SeqCharSequence(val xs: collection.IndexedSeq[Char]) extends CharSequence {
  def length: Int                                     = xs.length
  def charAt(index: Int): Char                        = xs(index)
  def subSequence(start: Int, end: Int): CharSequence = new SeqCharSequence(xs.slice(start, end))
  override def toString = xs.mkString("")
}

final class ArrayCharSequence(val xs: Array[Char]) extends CharSequence {
  def length: Int                                     = xs.length
  def charAt(index: Int): Char                        = xs(index)
  def subSequence(start: Int, end: Int): CharSequence = new ArrayCharSequence(copyOfRange(xs, math.max(0, start), math.min(xs.length, end)))
  override def toString = xs.mkString("")
}
