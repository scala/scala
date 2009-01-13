/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.jcl;

object Collection {
  val DEFAULT_FILTER : Any => Boolean = x => true;
  trait Projection[A] extends Collection[A] with MutableIterable.Projection[A] {
    override def projection = this
  }
}

/** Analogous to a Java collection.
 *
 *  @author Sean McDirmid
 */
trait Collection[A] extends MutableIterable[A] {
  /** Type-safe version of containsAll.
   **
   ** @author Sean McDirmid
   **/
  def hasAll(i: Iterable[A]): Boolean = i.forall(elements.has);

  /** Adds "a" to the collection, return true if "a" is actually added. */
  def add(a: A): Boolean;

  /** Adds all elements in "i" to the collection, return true if any elements are added. */
  def addAll(i: Iterable[A]): Boolean = {
    var changed = false;
    i.foreach(t => changed = add(t) || changed);
    changed;
  }
  /** Operator shortcut for addAll. */
  def ++(that: Iterable[A]): this.type = {
    addAll(that); this;
  }

  /** removes "a" from the collection. */
  def -=(a : A) : Unit = remove(a);

  /** adds "a" from the collection. */
  def +=(t : A) : Unit = add(t);

  /** adds "a" from the collection. Useful for chaining. */
  def +(t : A) : this.type = { add(t); this; }

  /** Transforms each element of the collection in-place according to
   *  <code>f</code>.
   *
   *  @param  f
   *  @return <code>true</code> if the collection is actually updated.
   */
  def transform(f: A => A): Boolean
  override def projection : Collection.Projection[A] = new Collection.Projection[A] {
    override def elements = Collection.this.elements
    override def size = Collection.this.size
    override def add(a: A): Boolean = Collection.this.add(a)
    override def transform(f : A => A) = Collection.this.transform(f);
  }
}
