/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.collection.mutable;


/** This trait represents mutable sets. Concrete set implementations
 *  just have to provide functionality for the abstract methods in
 *  <code>scala.collection.Set</code> as well as for <code>add</code>,
 *  <code>remove</code>, and <code>clear</code>.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 */
trait Set[A] with scala.collection.Set[A] {

    /** This method adds a new element to the set.
     */
    def +=(elem: A): Unit;

    /** <code>incl</code> can be used to add many elements to the set
     *  at the same time.
     */
    def incl(elems: A*): Unit = {
        val ys = elems.asInstanceOf[List[A]];
        ys foreach { y => +=(y); };
    }

    /** This method will add all the elements provided by an iterator
     *  of the iterable object <code>that</code> to the set.
     */
    def incl(that: Iterable[A]): Unit =
        that.elements.foreach(elem => +=(elem));

    /** <code>-=</code> can be used to remove a single element from
     *  a set.
     */
    def -=(elem: A): Unit;

    /** <code>excl</code> removes many elements from the set.
     */
    def excl(elems: A*): Unit = excl(elems);

    /** This method removes all the elements provided by an iterator
     *  of the iterable object <code>that</code> from the set.
     */
    def excl(that: Iterable[A]): Unit =
        that.elements.foreach(elem => -=(elem));

    /** This method computes an intersection with set <code>that</code>.
     *  It removes all the elements that are not present in <code>that</code>.
     */
    def intersect(that: Set[A]): Unit = filter(that.contains);

    /** Method <code>filter</code> removes all elements from the set for
     *  which the predicate <code>p</code> yields the value <code>false</code>.
     */
    def filter(p: A => Boolean): Unit = toList foreach {
        elem => if (p(elem)) -=(elem);
    }

    /** Removes all elements from the set. After this operation is completed,
     *  the set will be empty.
     */
    def clear: Unit;

    /** The hashCode method always yields an error, since it is not
     *  safe to use mutable stacks as keys in hash tables.
     *
     *  @return never.
     */
    override def hashCode(): Int = error("unsuitable as hash key");
}
