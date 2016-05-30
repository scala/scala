/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package runtime

@deprecated("use Predef.SeqCharSequence", "2.11.0")
final class SeqCharSequence(val xs: scala.collection.IndexedSeq[Char]) extends CharSequence {
  def length: Int                                     = xs.length
  def charAt(index: Int): Char                        = xs(index)
  def subSequence(start: Int, end: Int): CharSequence = new SeqCharSequence(xs.slice(start, end))
  override def toString = xs.mkString("")
}

// Still need this one since the implicit class ArrayCharSequence only converts
// a single argument.
final class ArrayCharSequence(val xs: Array[Char], start: Int, end: Int) extends CharSequence {
  // yikes
  // java.lang.VerifyError: (class: scala/runtime/ArrayCharSequence, method: <init> signature: ([C)V)
  //   Constructor must call super() or this()
  //
  // def this(xs: Array[Char]) = this(xs, 0, xs.length)

  def length: Int = math.max(0, end - start)
  def charAt(index: Int): Char = {
    if (0 <= index && index < length)
      xs(start + index)
    else throw new ArrayIndexOutOfBoundsException(index)
  }
  def subSequence(start0: Int, end0: Int): CharSequence = {
    if (start0 < 0) throw new ArrayIndexOutOfBoundsException(start0)
    else if (end0 > length) throw new ArrayIndexOutOfBoundsException(end0)
    else if (end0 <= start0) new ArrayCharSequence(xs, 0, 0)
    else {
      val newlen = end0 - start0
      val start1 = start + start0
      new ArrayCharSequence(xs, start1, start1 + newlen)
    }
  }
  override def toString = {
    val start = math.max(this.start, 0)
    val end = math.min(xs.length, start + length)

    if (start >= end) "" else new String(xs, start, end - start)
  }
}
