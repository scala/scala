/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala

/** Counted iterators keep track of the number of elements seen so far
 *
 *  @since 2.0
 */
@deprecated("use iterator.zipWithIndex instead")
trait CountedIterator[+A] extends Iterator[A] {
  /** counts the elements in this iterator; counts start at 0
   */
  def count: Int

  override def counted : this.type = this
}
