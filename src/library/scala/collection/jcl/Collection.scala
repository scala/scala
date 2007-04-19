/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.jcl;

object Collection {
  val DEFAULT_FILTER : Any => Boolean = x => true;
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
  def transform(f: A => A): Boolean;

  trait Projection extends super.Projection {
    override def filter(p : A => Boolean) : MutableIterable[A] = new Filter(p);
  }
  override def projection : Projection = new Projection {}


  /** Base implementation of a filtered collection */
  class Filter(p : A => Boolean) extends Collection[A] {
    def transform(f : A => A) =
      Collection.this.transform(a => if (p(a)) f(a) else a);
    override def add(a : A) = {
      if (!p(a)) throw new IllegalArgumentException;
      Collection.this.add(a);
    }
    override def has(a : A) = if (!p(a)) false else Collection.this.has(a);
    override def remove(a : A) = {
      if (!p(a)) throw new IllegalArgumentException;
      Collection.this.remove(a);
    }
    class Projection extends super.Projection {
      override def filter(p0 : A => Boolean) : MutableIterable[A] =
        Collection.this.projection.filter(a => p(a) && p0(a));
    }
    override def projection : Projection = new Projection;
    def elements = Collection.this.elements.filter(p);
    def size = size0;
  }
}
