/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.collection;

/** This trait defines the interface of collections that unambiguously map
 *  keys to values (i.e. a key is mapped to at least one value).
 *  Trait <code>Map</code> may only be used for
 *  accessing elements from map implementations. Two different extensions
 *  of trait <code>Map</code> in the package <code>scala.collections.mutable</code>
 *  and  <code>scala.collections.immutable</code> provide functionality for
 *  adding new key/value mappings to a map. The trait in the first package is
 *  implemented by maps that are modified destructively, whereas the trait in
 *  the second package is used by functional map implementations that rely on
 *  immutable data structures.
 *
 *  @author  Matthias Zenger
 *  @version 1.1, 02/05/2004
 */
trait Map[A, +B] extends AnyRef with PartialFunction[A, B] with Iterable[Pair[A, B]] {

    /** Compute the number of key-to-value mappings.
     *
     *  @return the number of mappings
     */
    def size: Int;

    /** Check if this map maps <code>key</code> to a value and return the
     *  value if it exists.
     *
     *  @param  key     the key of the mapping of interest
     *  @return the value of the mapping, if it exists
     */
    def get(key: A): Option[B];

    /** Is this an empty map?
     *
     *  @return true, iff the map is empty.
     */
    def isEmpty: Boolean = (size == 0);

    /** Retrieve the value which is associated with the given key. This
     *  method throws an exception if there is no mapping from the given
     *  key to a value.
     *
     *  @param  key     the key
     *  @return the value associated with the given key.
     */
    def apply(key: A): B = get(key) match {
        case None => default(key)
        case Some(value) => value
    }

    /** Is the given key mapped to a value by this map?
     *
     *  @param   key        the key
     *  @return true, iff there is a mapping for key in this map
     */
    def contains(key: A): Boolean = get(key) match {
        case None => false
        case Some(_) => true
    }

    /** Does this map contain a mapping from the given key to a value?
     *
     *  @param   key        the key
     *  @return true, iff there is a mapping for key in this map
     */
    def isDefinedAt(key: A) = contains(key);

    /** Creates an iterator for all keys.
     *
     *  @return an iterator over all keys.
     */
    def keys: Iterator[A] = new Iterator[A] {
        val iter = Map.this.elements;
        def hasNext = iter.hasNext;
        def next = iter.next._1;
    }

    /** Creates an iterator for a contained values.
     *
     *  @return an iterator over all values.
     */
    def values: Iterator[B] = new Iterator[B] {
        val iter = Map.this.elements;
        def hasNext = iter.hasNext;
        def next = iter.next._2;
    }

    /** Executes the given function for all (key, value) pairs
     *  contained in this map.
     *
     *  @param      f   the function to execute.
     */
    def foreach(f: (A, B) => Unit) = {
        val iter = elements;
        while (iter.hasNext) {
            val Pair(key, value) = iter.next;
            f(key, value);
        }
    }

    /** Applies the given predicate to all (key, value) mappings
     *  contained in this map and returns true if this predicate
     *  yields true for all mappings.
     *
     *  @param      p   the predicate
     *  @return    true, iff p yields true for all mappings.
     */
    def forall(p: (A, B) => Boolean): Boolean = elements.forall {
        case Pair(key, value) => p(key, value)
    }

    /** Applies the given predicate to all (key, value) mappings
     *  contained in this map and returns true if there is at least
     *  one mapping for which this predicate yields true.
     *
     *  @param     p   the predicate
     *  @return    true, iff there is at least one mapping for which
     *             p yields true.
     */
    def exists(p: (A, B) => Boolean): Boolean = elements.exists {
        case Pair(key, value) => p(key, value)
    }

    /** Compares two maps structurally; i.e. checks if all mappings
     *  contained in this map are also contained in the other map,
     *  and vice versa.
     *
     *  @return    true, iff both maps contain exactly the same mappings.
     */
    override def equals(that: Any): Boolean = that match {
      case other: Map[A, B] =>
        this.size == other.size && this.elements.forall {
          case Pair(key, value) => other.get(key) match {
            case None => false;
            case Some(otherval) => value == otherval;
          }
        }
      case _ => false
    }

    /** Returns the mappings of this map as a list.
     *
     *  @return    a list containing all mappings
     */
    def toList: List[Pair[A, B]] = elements.toList;

    /** Creates a string representation for this map.
     *
     *  @return    a string showing all mappings
     */
    override def toString() =
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

    /** The default value for the map, returned when a key is not found
     *  The method implemented here yields an error,
     *  but it might be overridden in subclasses.
     */
    def default(key: A): B =
      error("key not found: " + key)
}

