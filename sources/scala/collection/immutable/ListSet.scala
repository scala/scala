/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.collection.immutable;


object ListSet {
    /** constructs an empty ListSet
     */
    def Empty[A] = new ListSet[A];

}

/** This class implements immutable sets using a list-based data
 *  structure. Instances of <code>ListSet</code> represent
 *  empty sets; they can be either created by calling the constructor
 *  directly, or by applying the function <code>ListSet.Empty</code>.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 09/07/2003
 */
class ListSet[A] with Set[A] {

    /** Returns the number of elements in this set.
    *
    *  @return number of set elements.
    */
    def size: Int = 0;

    /** Checks if this set contains element <code>elem</code>.
    *
    *  @param  elem    the element to check for membership.
    *  @return true, iff <code>elem</code> is contained in this set.
    */
    def contains(elem: A): Boolean = false;

    /** This method creates a new set with an additional element.
     */
    def +(elem: A): ListSet[A] = new Node(elem);

    /** <code>-</code> can be used to remove a single element from
     *  a set.
     */
    def -(elem: A): ListSet[A] = this;

    /** Creates a new iterator over all elements contained in this
     *  object.
     *
     *  @return the new iterator
     */
    def elements: Iterator[A] = toList.elements;

    /** Transform this set into a list of all elements.
     *
     *  @return  a list which enumerates all elements of this set.
     */
    override def toList: List[A] = Nil;

    /** Compares two sets for equality.
    *   Two set are equal iff they contain the same elements.
    */
    override def equals(obj: Any): Boolean =
        if (obj.isInstanceOf[scala.collection.Set[A]]) {
            val that = obj.asInstanceOf[scala.collection.Set[A]];
            if (size != that.size) false else toList.forall(that.contains);
        } else
            false;

    override def hashCode(): Int = 0;

    protected class Node(elem: A) extends ListSet[A] {
      /** Returns the number of elements in this set.
      *
      *  @return number of set elements.
      */
      override def size = ListSet.this.size + 1;
      /** Checks if this set is empty.
      *
      *  @return true, iff there is no element in the set.
      */
      override def isEmpty: Boolean = false;

      /** Checks if this set contains element <code>elem</code>.
      *
      *  @param  elem    the element to check for membership.
      *  @return true, iff <code>elem</code> is contained in this set.
      */
      override def contains(e: A) = (e == elem) || ListSet.this.contains(e);

      /** This method creates a new set with an additional element.
      */
      override def +(e: A): ListSet[A] = if (contains(e)) this else new Node(e);
      /** <code>-</code> can be used to remove a single element from
      *  a set.
      */
      override def -(e: A): ListSet[A] = if (e == elem) ListSet.this else {
        val tail = ListSet.this - e; new tail.Node(elem)
      }

      /** Transform this set into a list of all elements.
      *
      *  @return  a list which enumerates all elements of this set.
      */
      override def toList: List[A] = elem :: ListSet.this.toList;
      override def hashCode(): Int = elem.hashCode() + ListSet.this.hashCode();
    }
}
