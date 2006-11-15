/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.immutable


object ListMap {
  def Empty[A, B] = new ListMap[A, B]
}

/** This class implements immutable maps using a list-based data
 *  structure. Instances of <code>ListMap</code> represent
 *  empty maps; they can be either created by calling the constructor
 *  directly, or by applying the function <code>ListMap.Empty</code>.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 09/07/2003
 */
[serializable]
class ListMap[A, B] extends AnyRef with Map[A, B] {

  /** Returns a <code>new ListMap</code> instance mapping keys of the
   *  same type to values of type <code>C</code>.
   */
  def empty[C] = ListMap.Empty[A, C]

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
  def update(key: A, value: B): ListMap[A, B] = new Node(key, value)

  /** This creates a new mapping without the given <code>key</code>.
   *  If the map does not contain a mapping for the given key, the
   *  method returns the same map.
   *
   *  @param key a map without a mapping for the given key.
   */
  def -(key: A): ListMap[A, B] = this

  /** Returns an iterator over key-value pairs.
   */
  def elements: Iterator[Pair[A,B]] = new Iterator[Pair[A,B]] {
    var that: ListMap[A,B] = ListMap.this;
    def hasNext = !that.isEmpty;
    def next: Pair[A,B] =
      if (!hasNext) throw new NoSuchElementException("next on empty iterator")
      else { val res = Pair(that.key, that.value); that = that.next; res }
  }

  /** Compares two maps for equality.
   *   Two maps are equal iff they contain exactly the
   *   same key-value pairs.
   */
  override def equals(obj: Any): Boolean =
    if (obj.isInstanceOf[scala.collection.Map[A, B]]) {
      val that = obj.asInstanceOf[scala.collection.Map[A, B]]
      if (size != that.size) false else toList.forall {
        case Pair(key, value) => that.get(key) match {
          case None => false
          case Some(v) => v == value
        }
      }
    } else
      false

  override def hashCode(): Int = 0

  protected def key: A = throw new NoSuchElementException("empty map");
  protected def value: B = throw new NoSuchElementException("empty map");
  protected def next: ListMap[A, B] = throw new NoSuchElementException("empty map");

  [serializable]
  protected class Node(override protected val key: A, override protected val value: B)
    extends ListMap[A, B]
  {
    /** Returns the number of mappings in this map.
     *
     *  @return number of mappings.
     */
    override def size: Int = ListMap.this.size + 1

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
    override def apply(k: A): B = if (k == key) value else ListMap.this(k)

    /** Checks if this map maps <code>key</code> to a value and return the
     *  value if it exists.
     *
     *  @param  key the key of the mapping of interest
     *  @return     the value of the mapping, if it exists
     */
    override def get(k: A): Option[B] =
      if (k == key) Some(value) else ListMap.this.get(k)

    /** This method allows one to create a new map with an
     *  additional mapping from <code>key</code>
     *  to <code>value</code>. If the map contains already a
     *  mapping for <code>key</code>, it will be overridden by this
     *  function.
     *
     *  @param k ...
     *  @param v ...
     */
    override def update(k: A, v: B): ListMap[A, B] =
      if (k == key) {
        new ListMap.this.Node(k, v)
      } else {
        val tail = ListMap.this.update(k,v); new tail.Node(key, value)
      }

    /** Creates a new mapping without the given <code>key</code>.
     *  If the map does not contain a mapping for the given key, the
     *  method returns the same map.
     *
     *  @param k ...
     *  @return  ...
     */
    override def -(k: A): ListMap[A, B] =
      if (k == key)
        ListMap.this
      else {
        val tail = ListMap.this - k; new tail.Node(key, value)
      }

    override def hashCode(): Int =
      (key.hashCode() ^ value.hashCode()) + ListMap.this.hashCode()

    override protected def next: ListMap[A,B] = ListMap.this;
  }
}
