/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.collection;


/** This trait defines the interface of collections that do not contain
 *  duplicate elements. Trait <code>Set</code> may only be used for
 *  accessing elements from set implementations. Two different extensions
 *  of trait <code>Set</code> in the package <code>scala.collections.mutable</code>
 *  and  <code>scala.collections.immutable</code> provide functionality for
 *  adding new elements to a set. The trait in the first package is implemented
 *  by sets the are modified destructively, whereas the trait in the second
 *  package is used by functional set implementations that rely on immutable
 *  data structures.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 */
trait Set[A] with Function1[A, Boolean] with Iterable[A] {

    /** Returns the number of elements in this set.
    *
    *  @return number of set elements.
    */
    def size: Int;

    /** Checks if this set contains element <code>elem</code>.
     *
     *  @param  elem    the element to check for membership.
     *  @return true, iff <code>elem</code> is contained in this set.
     */
    def contains(elem: A): Boolean;

    /** This method allows sets to be interpreted as predicates.
     *  It returns true, iff this set contains element <code>elem</code>.
     *
     *  @param  elem    the element to check for membership.
     *  @return true, iff <code>elem</code> is contained in this set.
     */
    def apply(elem: A): Boolean = contains(elem);

    /** Checks if this set is empty.
     *
     *  @return true, iff there is no element in the set.
     */
    def isEmpty: Boolean = (size == 0);

    /** Checks if this set is a subset of set <code>that</code>.
     *
     *  @param  that another set.
     *  @return true, iff the other set is a superset of this set.
     */
    def subsetOf(that: Set[A]): Boolean = forall(that.contains);

    /** Execute the statement <code>f</code> for every element in this set.
     *
     *  @param  f   a function that is applied to every element in this set.
     */
    def foreach(f: A => Unit): Unit = elements.foreach(f);

    /** Checks if a given predicate is true for all elements in this set.
     *
     *  @param   p     the predicate
     *  @returns true, iff the predicate yields true for all elements.
     */
    def forall(p: A => Boolean): Boolean = elements.forall(p);

	/** Checks if a given predicate is true for at least one element
	 *  in this set.
     *
     *  @param   p     the predicate
     *  @returns true, iff the predicate yields true for at least one element.
     */
    def exists(p: A => Boolean): Boolean = elements.exists(p);

    /** Transform this set into a list of all elements.
     *
     *  @return  a list which enumerates all elements of this set.
     */
    def toList: List[A] = {
        var res: List[A] = Nil;
        elements.foreach { elem => res = elem :: res; }
        res;
    }

    /** Compares this set with another object and returns true, iff the
     *  other object is also a set which contains the same elements as
     *  this set.
     *
     *  @param  that  the other object
     *  @return true, iff this set and the other set contain the same
     *          elements.
     */
    override def equals(that: Any): Boolean =
        that.isInstanceOf[Set[A]] &&
        { val other = that.asInstanceOf[Set[A]];
          this.size == other.size &&
          this.elements.forall(other.contains) };

    /** Returns a string representation of this set.
     *
     *  @return a string showing all elements of this set.
     */
    override def toString(): String =
        if (size == 0)
            "{}"
        else
            "{" + {
                val iter = elements;
                var res = iter.next.toString();
                while (iter.hasNext) {
                    res = res + ", " + iter.next;
                }
                res;
            } + "}";
}
