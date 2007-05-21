/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

/** Buffered iterators are iterators which allow to inspect the next
 *  element without discarding it.
 *
 *  @author  Martin Odersky
 *  @version 1.0, 16/07/2003
 */
trait BufferedIterator[+A] extends Iterator[A] {
  /** returns the first <code>sz</code> elements that will be iterated by this iterator,
   *  or fewer if the iterator has less elements left to iterate over
   */
  def peekList(sz : Int) : Seq[A]

  /** Checks what the next available element is.
   *
   *  @return the current element
   */
  def head: A = peek(0)
  /** return the <code>n</code>th element that will be iterated by this iterator */
  def peek(n : Int) : A = {
    var m = n
    val lst = peekList(n + 1)
    if (m == 0 && !lst.isEmpty) return lst(0)
    for (a <- lst) {
      if (m == 0) return a
      m = m - 1
    }
    return defaultPeek
  }
  /** element returned when no element left to iterate over;
   *  throws <code>NoSuchElementException</code> by default
   */
  protected def defaultPeek : A = throw new Predef.NoSuchElementException

  /** iterates over and applies <code>f</code> to the next element
   *  if this iterator has a next element that <code>f</code> is defined for.
   */
  def readIf[T](f : PartialFunction[A,T]) : Option[T] =
    if (hasNext && f.isDefinedAt(head)) Some(f(next))
    else None

  /** iterates over elements as long as <code>f</code> is true
   *  for each element, returns the elements iterated over
   */
  def readWhile(f : A => Boolean) : Seq[A] = {
    import scala.collection.mutable.ArrayBuffer
    var read = new ArrayBuffer[A]
    while (hasNext && f(head))
      read += next
    read
  }
  /** true if elements of <code>seq</code> will be iterated over next in this iterator
   */
  def startsWith(seq : Seq[Any]) : Boolean = {
    var sz = seq.length
    val j = peekList(sz).elements
    val i = seq.elements
    while (i.hasNext && j.hasNext)
      if (i.next != j.next) return false
    return !i.hasNext && !j.hasNext
  }
  override def buffered: BufferedIterator[A] = this
  override def hasNext = !peekList(1).isEmpty
  override def toString = if (hasNext) "peek " + peekList(1).reverse else "empty"
}
