/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$



package scala.collection.immutable

object ListMap {

  /** The empty map of this type
   *  @deprecated   use <code>empty</code> instead
   */
  @deprecated
  def Empty[A, B] = new ListMap[A, B]

  /** The empty map of this type */
  def empty[A, B] = new ListMap[A, B]

  /** The canonical factory for this type
   */
  def apply[A, B](elems: (A, B)*) = empty[A, B] ++ elems
}

/** This class implements immutable maps using a list-based data
 *  structure. Instances of <code>ListMap</code> represent
 *  empty maps; they can be either created by calling the constructor
 *  directly, or by applying the function <code>ListMap.empty</code>.
 *
 *  @author  Matthias Zenger
 *  @author  Martin Oderskty
 *  @version 2.0, 01/01/2007
 */
@serializable
class ListMap[A, +B] extends Map[A, B] {

  /** Returns a <code>new ListMap</code> instance mapping keys of the
   *  same type to values of type <code>C</code>.
   */
  def empty[C] = ListMap.empty[A, C]

  /** Returns the number of mappings in this map.
   *
   *  @return number of mappings in this map.
   */
  def size: Int = 0

  /** Checks if this map maps <code>key</code> to a value and return the
   *  value if it exists.
   *
   *  @param  key the key of the mapping of interest
   *  @return     the value of the mapping, if it exists
   */
  def get(key: A): Option[B] = None

  /** This method allows one to create a new map with an
   *  additional mapping from <code>key</code>
   *  to <code>value</code>. If the map contains already a
   *  mapping for <code>key</code>, it will be overridden by this
   *  function.
   *
   *  @param key  the key element of the updated entry.
   *  @param value the value element of the updated entry.
   */
  def update [B1 >: B](key: A, value: B1): ListMap[A, B1] = new Node(key, value)

  /** This creates a new mapping without the given <code>key</code>.
   *  If the map does not contain a mapping for the given key, the
   *  method returns the same map.
   *
   *  @param key a map without a mapping for the given key.
   */
  def - (key: A): ListMap[A, B] = this

  /** Returns an iterator over key-value pairs.
   */
  def elements: Iterator[(A,B)] =
    new Iterator[(A,B)] {
      var self: ListMap[A,B] = ListMap.this
      def hasNext = !self.isEmpty
      def next: (A,B) =
        if (!hasNext) throw new NoSuchElementException("next on empty iterator")
        else { val res = (self.key, self.value); self = self.next; res }
    }.toList.reverse.elements

  protected def key: A = throw new NoSuchElementException("empty map")
  protected def value: B = throw new NoSuchElementException("empty map")
  protected def next: ListMap[A, B] = throw new NoSuchElementException("empty map")

  @serializable
  protected class Node[B1 >: B](override protected val key: A,
                                override protected val value: B1) extends ListMap[A, B1] {
    /** Returns the number of mappings in this map.
     *
     *  @return number of mappings.
     */
    override def size: Int = next.size + 1

    /** Is this an empty map?
     *
     *  @return true, iff the map is empty.
     */
    override def isEmpty: Boolean = false

    /** Retrieves the value which is associated with the given key. This
     *  method throws an exception if there is no mapping from the given
     *  key to a value.
     *
     *  @param  key the key
     *  @return     the value associated with the given key.
     */
    override def apply(k: A): B1 = if (k == key) value else next(k)

    /** Checks if this map maps <code>key</code> to a value and return the
     *  value if it exists.
     *
     *  @param  key the key of the mapping of interest
     *  @return     the value of the mapping, if it exists
     */
    override def get(k: A): Option[B1] =
      if (k == key) Some(value) else next.get(k)

    /** This method allows one to create a new map with an
     *  additional mapping from <code>key</code>
     *  to <code>value</code>. If the map contains already a
     *  mapping for <code>key</code>, it will be overridden by this
     *  function.
     *
     *  @param k ...
     *  @param v ...
     */
    override def update [B2 >: B1](k: A, v: B2): ListMap[A, B2] = {
      val m = if (contains(k)) this - k else this
      new m.Node(k, v)
    }

    /** Creates a new mapping without the given <code>key</code>.
     *  If the map does not contain a mapping for the given key, the
     *  method returns the same map.
     *
     *  @param k ...
     *  @return  ...
     */
    override def - (k: A): ListMap[A, B1] =
      if (k == key)
        next
      else {
        val tail = next - k
        if (tail eq next) this
        else new tail.Node(key, value)
      }

    override protected def next: ListMap[A,B1] = ListMap.this
  }
}
