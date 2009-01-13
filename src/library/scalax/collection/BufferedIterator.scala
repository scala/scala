/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: BufferedIterator.scala 12641 2007-08-22 16:01:57Z mcdirmid $


package scalax.collection

/** Buffered iterators are iterators which allow to inspect the next
 *  element without discarding it.
 *
 *  @author  Martin Odersky
 *  @version 2.8
 */
trait BufferedIterator[+A] extends Iterator[A] {

  /** Returns current element of iterator without advancing beyond it.
   */
  def head: A

  override def buffered: this.type = this
}
