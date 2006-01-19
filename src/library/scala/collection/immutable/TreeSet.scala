/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.immutable;


/** This class implements immutable sets using a tree.
 *
 *  @author  Matthias Zenger
 *  @author  Burak Emir
 *  @version 1.1, 03/05/2004
 */

[serializable]
class TreeSet[A <% Ordered[A]]() extends Tree[A, A] with Set[A] {

    override protected type This = TreeSet[A];
    override protected def getThis: This = this;

    protected def New(sz: Int, t: aNode): This = new TreeSet[A] {
        override def size = sz;
        override protected def tree: aNode = t;
    }

    /** Checks if this set contains element <code>elem</code>.
     *
     *  @param  elem    the element to check for membership.
     *  @return true, iff <code>elem</code> is contained in this set.
     */
    def contains(elem: A): Boolean = !findValue(elem).isEmpty;

    /** This method creates a new set with an additional element.
     */
    def +(elem: A): TreeSet[A] = updateOrAdd(elem, elem);

    /** <code>-</code> can be used to remove a single element from
     *  a set.
     */
    def -(elem: A): TreeSet[A] = deleteAny(elem);

    /** Creates a new iterator over all elements contained in this
     *  object.
     *
     *  @return the new iterator
     */
    def elements: Iterator[A] = entries;

    /** Transform this set into a list of all elements.
     *
     *  @return  a list which enumerates all elements of this set.
     */
    override def toList: List[A] =
      tree.toList(scala.Nil) map (._2);

    /** Compares two sets for equality.
     *  Two set are equal iff they contain the same elements.
     */
    override def equals(obj: Any): Boolean =
        obj.isInstanceOf[scala.collection.Set[A]] && {
          val that = obj.asInstanceOf[scala.collection.Set[A]];
          (size == that.size) && toList.forall(that.contains);
        }
}
