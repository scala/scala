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
trait MutableIterable[A] extends Iterable[A] {
  /** @return true if t is in the collection.
   **/
  def has(t : A ) : Boolean = elements.has(t);
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
   **/
  def -(t : A) : this.type = { remove(t); this; }
  /** retain only elements in the collection that predicate p is true for.
   **/
  def retain(p : A => Boolean) : Unit = elements.retain(p);
  /** retain only elements that are also in that.
   **/
  def retainAll(that : Iterable[A]) : Boolean = elements.retain(s => that.exists(t => t == s));
  /** @return true if the element has no elements.
   **/
  def isEmpty : Boolean = !elements.hasNext;
  /** @return the current number of elements in the collection.
   **/
  def size : Int = {
    var count = 0;
    val i = elements;
    while (i.hasNext) { count = count + 1; i.next; }
    count;
  }
  /** clear all elements from the collection.
   **/
  def clear(): Unit = {
    val i = elements;
    while (i.hasNext) {
      i.next; i.remove;
    }
  }
  /** Creates a non-strict map of this collection. Any removals from the returned
   ** collection will remove from this collection, while any changes to this collection will also be
   ** reflected in the mapped collection.
   ** @return a non-strict map of this collection.
   **/
  def pmap[B](f : A => B) : MutableIterable[B] = new Map[B](f);
  /** The default implementation of a map over mutable iterable collections.
   **/
  protected class Map[B](f : A => B) extends MutableIterable[B] {
    override def elements = MutableIterable.this.elements.map(f);
    override def toString = elements.toList.mkString("{", ", ", "}");
  }
  override def elements : MutableIterator[A];
}
