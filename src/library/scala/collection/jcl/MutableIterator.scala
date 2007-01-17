/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.jcl;

/** An iterator that supports the remove operation.
 *  These iterators wrap Java iterators, and so have the same fail fast
 *  behavior when dealing with concurrent modifications.
 *
 *  @author Sean McDirmid
 */
trait MutableIterator[A] extends Iterator[A] {
  def remove : Unit;

  override def filter(f : A => Boolean) : MutableIterator[A] = {
    val buffered = this.buffered0;
    new buffered.Filter(f);
  }

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

  private[jcl] def buffered0 : MutableIterator[A]#Buffered = new BufferedImpl;

  /** Standard implementation of a mapped iterator. **/
  class Map[B](f : A => B) extends MutableIterator[B] {
    def hasNext = MutableIterator.this.hasNext;
    def next = f(MutableIterator.this.next);
    def remove = MutableIterator.this.remove;
  }

  private[jcl] trait Buffered extends MutableIterator[A] {
    private[jcl] override def buffered0 : this.type = this;
    private[jcl] def peekNext : A;
    private[jcl] def seekNext(f : A => Boolean) : Option[A] = {
      while (hasNext && !f(peekNext)) next;
      if (hasNext) Some(peekNext) else None;
    }
    class Filter(p : A => Boolean) extends MutableIterator[A] {
      private[jcl] def peekNext = Buffered.this.seekNext(p) match {
      case Some(result) => result;
      case None => throw new NoSuchElementException;
      }
      override def next = {
        val ret = peekNext; val ret0 = Buffered.this.next; assert(ret == ret0); ret;
      }
      override def hasNext : Boolean = seekNext(p) != None;
      override def remove : Unit = Buffered.this.remove;
    }
  }

  private[jcl] class BufferedImpl extends Buffered {
    protected var head : A = _;
    protected def underlying = MutableIterator.this;
    private[jcl] def peekNext = {
      if (head == null) head = underlying.next;
      head;
    }
    override def hasNext = head != null || underlying.hasNext;
    override def next = {
      val ret = peekNext; head = null.asInstanceOf[A]; ret;
    }
    override def remove = {
      if (head != null) throw new NoSuchElementException;
      underlying.remove;
    }
    override def toString = "buffered[" + underlying + "]";
  }
}
