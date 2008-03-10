/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id:


package scala.util.parsing.input

/** An implementation of java.lang.CharSequence over a character array
 *
 * @author Martin Odersky
 */
class CharArraySequence(source: Array[Char], start: Int, end: Int) extends CharSequence {

  def this(source: Array[Char], start: Int) = this(source, start, source.length)

  def this(source: Array[Char]) = this(source, 0)

  if (start < 0) throw new IndexOutOfBoundsException
  if (end > source.length) throw new IndexOutOfBoundsException

  def charAt(index: Int) =
    if (start + index < end) source(index + start)
    else throw new IndexOutOfBoundsException

  def length: Int = if (end < start) 0 else end - start

  def subSequence(_start: Int, _end: Int) =
    new CharArraySequence(source, start + _start, start + _end)

  override def toString = new String(source, start, end - start)
}
