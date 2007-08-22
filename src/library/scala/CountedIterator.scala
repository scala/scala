/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

/** Counted iterators keep track of the number of elements seen so far
 *
 *  @author  Martin Odersky
 *  @version 1.0, 16/07/2003
 */
trait CountedIterator[+A] extends Iterator[A] {
  /** counts the elements in this iterator; counts start at 0
   */
  def count: Int

  override def counted : this.type = this
  override def buffered: BufferedIterator[A] with CountedIterator[A] = new BufferedIterator.Default[A] with CountedIterator[A] {
    protected def fill(sz : Int) = if (CountedIterator.this.hasNext) (CountedIterator.this.next) :: Nil else Nil
    override def count = CountedIterator.this.count - peekList(0).length
    override def counted : this.type = this
    override def buffered : this.type = this
  }
}
