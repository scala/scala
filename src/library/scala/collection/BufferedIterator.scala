/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.collection

/** Buffered iterators are iterators which provide a method `head`
 *  that inspects the next element without discarding it.
 *
 *  @author  Martin Odersky
 *  @version 2.8
 *  @since   2.8
 */
trait BufferedIterator[+A] extends Iterator[A] {

  /** Returns next element of iterator without advancing beyond it.
   */
  def head: A

  override def buffered: this.type = this
}
