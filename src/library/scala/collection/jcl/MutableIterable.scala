/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.jcl;

/**
 * An iterable collection that supports remove operations.
 * Useful for representing projections of mutable collections that where only
 * the remove operation makes sense.
 *
 * @author Sean McDirmid
 */
trait MutableIterable[A] extends scala.Collection[A] {
  /** @return true if t is in the collection.
   **/
  def has(t : A ) : Boolean = elements.contains(t);

  /** @return true if t was removed from this collection.
   **/
  def remove(t : A ) : Boolean = elements.remove(t);
  /** @return true if any element in that was removed from this collection.
   **/
  def removeAll(that : Iterable[A]) : Boolean = {
    var changed = false;
    that.foreach(t => changed = elements.remove(t) || changed);
    changed;
  }
  /** Operator shortcut for removeAll. */
  def --(that : Iterable[A]) : this.type = {
    removeAll(that); this;
  }

  /** @return the collection that t was removed from.
   */
  def -(t : A) : this.type = { remove(t); this; }
  /** retain only elements in the collection that predicate p is true for.
   */
  def retain(p : A => Boolean) : Unit = elements.retain(p);
  /** retain only elements that are also in that.
   */
  def retainAll(that : Iterable[A]) : Boolean = elements.retain(s => that.exists(t => t == s));

  /** @return the current number of elements in the collection.
   */
  protected def size0 : Int = {
    var count = 0;
    val i = elements;
    while (i.hasNext) { count = count + 1; i.next; }
    count;
  }

  /** clear all elements from the collection.
   */
  def clear(): Unit = {
    val i = elements;
    while (i.hasNext) {
      i.next; i.remove;
    }
  }
  override def projection : MutableIterable.Projection[A] = new MutableIterable.Projection[A] {
    override def elements = MutableIterable.this.elements
    override def size = MutableIterable.this.size
    override def remove(t : A ) : Boolean = MutableIterable.this.remove(t)
    override def filter(p : A => Boolean) : MutableIterable.Projection[A] = super.filter(p)
  }
  /** The default implementation of a map over mutable iterable collections.
   **/
  override def elements : MutableIterator[A];
  protected class Map[B](f : A => B) extends MutableIterable.Projection[B] {
    override def elements = MutableIterable.this.elements.map(f)
    override def size = MutableIterable.this.size
  }
  trait Filter extends MutableIterable.Projection[A] {
    protected def p(a : A) : Boolean
    override def has(a : A) = if (!p(a)) false else MutableIterable.this.has(a);
    override def remove(a : A) = {
      if (!p(a)) throw new IllegalArgumentException;
      MutableIterable.this.remove(a);
    }
    override def filter(p0 : A => Boolean) : MutableIterable.Projection[A] =
      MutableIterable.this.projection.filter(a => p(a) && p0(a));
    def elements = {
      val i = MutableIterable.this.elements.filter(p);
      new MutableIterator[A] {
	def next = i.next
	def hasNext = i.hasNext
	def remove : Unit = throw new Error
      }
    }
    def size = size0;
  }
}

object MutableIterable {
  trait Projection[A] extends MutableIterable[A] with Iterable.Projection[A] {
    override def projection = this
    override def map[B](f : A => B) : Projection[B] = new Map[B](f);
    override def filter(pp : A => Boolean) : Projection[A] = new Filter {
      def p(a : A) = pp(a)
    }
  }
}

