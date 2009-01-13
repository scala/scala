/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.jcl;

object MutableIterator {
  class Wrapper[A](val underlying : java.util.Iterator[A]) extends MutableIterator[A] {
    def hasNext = underlying.hasNext;
    def next = underlying.next.asInstanceOf[A];
    def remove = underlying.remove;
  }
}

/** An iterator that supports the remove operation.
 *  These iterators wrap Java iterators, and so have the same fail fast
 *  behavior when dealing with concurrent modifications.
 *
 *  @author Sean McDirmid
 */
trait MutableIterator[A] extends Iterator[A] {
  def remove : Unit;

  /* filter doesnt' support remove yet.
  override def filter(f : A => Boolean) : MutableIterator[A] = {
    val buffered = this.buffered0;
    new buffered.Filter(f);
  }
  */

  override def map[B](f: A => B) : MutableIterator[B] = new Map(f);
  /** A type-safe version of contains.
   **/
  def has(a: A) = exists(b => a == a);

  /** Finds and removes the first instance of "a" through the iterator.
   *  After execution, the iterator's cursor is located where the removed
   *  element existed.
   *
   * @param  a ..
   * @return <code>false</code> if "a" is not encountered in the iterator
   *         and the iterator's cursor is located at the end of its elements.
   */
  def remove(a: A): Boolean = {
    while (hasNext)
      if (next == a) { remove; return true; }
    return false;
  }
  /** Removes all elements in the iterator that predicate "p" returns false on.
   **/
  def retain(p : A => Boolean) : Boolean = {
    var changed = false;
    while (hasNext)
      if (!p(next)) { remove; changed = true; }
    changed;
  }

  /** Standard implementation of a mapped iterator. **/
  class Map[B](f : A => B) extends MutableIterator[B] {
    def hasNext = MutableIterator.this.hasNext
    def next = f(MutableIterator.this.next)
    def remove = MutableIterator.this.remove
  }
}
