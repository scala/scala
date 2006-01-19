/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.collection.immutable;

object TreeMap {
    def Empty[A <% Ordered[A], B] = new TreeMap[A, B];
}

/** This class implements immutable maps using a tree.
 *
 *  @author  Erik Stenman
 *  @author  Matthias Zenger
 *  @version 1.1, 03/05/2004
 */

[serializable]
class TreeMap[A <% Ordered[A], B] extends Tree[A, Pair[A, B]] with Map[A, B] {

    override protected type This = TreeMap[A, B];
    override protected def getThis: This = this;

    /** A factory to create empty maps of the same type of keys.
     */
    def empty[C] = new TreeMap[A, C];

    /** Creates a new TreeMap from a GBTree and its size.
     */
    protected def New(sz:Int,t:aNode):This = new TreeMap[A,B] {
        override def size=sz;
        override protected def tree:aNode=t;
    }

    /** A new TreeMap with the entry added is returned,
     *  if key is <em>not</em> in the TreeMap, otherwise
     *  the key is updated with the new entry.
     */
    def update(key:A, value:B) = updateOrAdd(key, Pair(key, value));

    /** A new TreeMap with the entry added is returned,
     *  assuming that key is <em>not</em> in the TreeMap.
     */
    def insert(key:A,value:B) = add(key, Pair(key, value));

    /** Removes the key from the TreeMap.
     */
    def -(key:A) = deleteAny(key);

    /** Check if this map maps <code>key</code> to a value and return the
     *  value if it exists.
     *
     *  @param  key     the key of the mapping of interest
     *  @return the value of the mapping, if it exists
     */
    override def get(key: A): Option[B] =
      findValue(key) match {
        case Some(Pair(_, value: B)) => Some(value)
        case _ => None
      }

    /** Retrieve the value which is associated with the given key. This
     *  method throws an exception if there is no mapping from the given
     *  key to a value.
     *
     *  @param  key     the key
     *  @return the value associated with the given key.
     *  @throws Error("key not found").
     */
    override def apply(key:A):B = tree.apply(key)._2;

    /** Creates a list of all (key, value) mappings.
     *
     *  @return    the list of all mappings
     */
    override def toList: List[Pair[A,B]] =
      tree.toList(scala.Nil) map (._2);

    /** Creates a new iterator over all elements contained in this
     *  object.
     *
     *  @return the new iterator
     */
    def elements: Iterator[Pair[A,B]] = entries;

    /** Compares two maps structurally; i.e. checks if all mappings
     *  contained in this map are also contained in the other map,
     *  and vice versa.
     *
     *  @return    true, iff both maps contain exactly the same mappings.
     */
    override def equals(obj: Any): Boolean =
      obj.isInstanceOf[scala.collection.Map[A, B]] && {
        val that = obj.asInstanceOf[scala.collection.Map[A, B]];
        size == that.size && elements.forall {
          case Pair(key, value) => that.get(key) match {
            case None => false;
            case Some(v) => v == value;
          }
        }
      };
}

