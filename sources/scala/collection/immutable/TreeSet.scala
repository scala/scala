/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.collection.immutable;


/** This class implements immutable sets using a tree.
 *
 *  @author  Matthias Zenger
 *  @author  Burak Emir
 *  @version 1.1, 03/05/2004
 */
class TreeSet[A <% Ordered[A]] extends Tree[A, A] with Set[A] {

    type This = TreeSet[A];

    def entryKey(entry: A) = entry;

    protected def New(sz: Int, t: aNode): This = new TreeSet[A] {
        override def size = sz;
        override protected val tree: aNode = t;
    }

    /** Checks if this set contains element <code>elem</code>.
     *
     *  @param  elem    the element to check for membership.
     *  @return true, iff <code>elem</code> is contained in this set.
     */
    def contains(elem: A): Boolean = !findValue(elem).isEmpty;

    /** This method creates a new set with an additional element.
     */
    def +(elem: A): TreeSet[A] = update_or_add(elem, elem);

    /** <code>-</code> can be used to remove a single element from
     *  a set.
     */
    def -(elem: A): TreeSet[A] = delete_any(elem);

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
    override def toList: List[A] = tree.toList(scala.Nil);

    /** Compares two sets for equality.
     *  Two set are equal iff they contain the same elements.
     */
    override def equals(obj: Any): Boolean =
		if (obj.isInstanceOf[scala.collection.Set[A]]) {
		  val that = obj.asInstanceOf[scala.collection.Set[A]];
		  if (size != that.size) false else toList.forall(that.contains);
		} else
		  false;
}
