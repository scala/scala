/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable

import Predef._

//import Predef.UnsupportedOperationException

/** This class represents mutable maps. Concrete map implementations
 *  just have to provide functionality for the abstract methods in
 *  <code>scala.collection.Map</code> as well as for <code>update</code>,
 *  and <code>-=</code>.
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.0, 31/12/2006
 */

object Map {

  /** The empty map of this type; this is implemented as a hashtable */
  def empty[A, B]: Map[A, B] = new HashMap[A, B]

  /** The canonical factory for this type
   */
  def apply[A, B](elems: (A, B)*) = empty[A, B] ++ elems
}

@cloneable
trait Map[A, B] extends AnyRef
      with collection.Map[A, B]
      with Scriptable[Message[(A, B)]]
{
  /** This method allows one to add a new mapping from <code>key</code>
   *  to <code>value</code> to the map. If the map already contains a
   *  mapping for <code>key</code>, it will be overridden by this
   *  function.
   *
   * @param key    The key to update
   * @param value  The new value
   */
  def update(key: A, value: B)

  /** Add a key/value pair to this map.
   *  @param    kv the key/value pair.
   */
  def += (kv: (A, B)) { update(kv._1, kv._2) }

  /** Add two or more key/value pairs to this map.
   *  @param    kv1 the first key/value pair.
   *  @param    kv2 the second key/value pair.
   *  @param    kvs the remaining key/value pairs.
   */
  def += (kv1: (A, B), kv2: (A, B), kvs: (A, B)*) {
    this += kv1; this += kv2; this ++= kvs
  }

  /** Add a sequence of key/value pairs to this map.
   *  @param    kvs the iterable object containing all key/value pairs.
   */
  def ++= (kvs: Iterable[(A, B)]) { this ++= kvs.elements }

  /** Add a sequence of key/value pairs to this map.
   *  @param    kvs the iterator containing all key/value pairs.
   */
  def ++= (kvs: Iterator[(A, B)]) { kvs foreach += }

  /** Add a key/value pair to this map.
   *  @param    kv the key/value pair.
   *  @return   The map itself with the new binding added in place.
   */
  def + (kv: (A, B)): Map[A, B] = { this += kv; this }

  /** Add two or more key/value pairs to this map.
   *  @param    kv1 the first key/value pair.
   *  @param    kv2 the second key/value pair.
   *  @param    kvs the remaining key/value pairs.
   *  @return   The map itself with the new bindings added in place.
   */
  def + (kv1: (A, B), kv2: (A, B), kvs: (A, B)*): Map[A, B] = {
    this.+=(kv1, kv2, kvs: _*); this
  }

  /** Add a sequence of key/value pairs to this map.
   *  @param    kvs the iterable object containing all key/value pairs.
   *  @return   The itself map with the new bindings added in place.
   */
  def ++ (kvs: Iterable[(A, B)]): Map[A, B] = { this ++= kvs; this }

  /** Add a sequence of key/value pairs to this map.
   *  @param    kvs the iterator containing all key/value pairs.
   *  @return   The itself map with the new bindings added in place.
   */
  def ++ (kvs: Iterator[(A, B)]): Map[A, B] = { this ++= kvs; this }

  /** Remove a key from this map, noop if key is not present.
   *  @param    key the key to be removed
   */
  def -= (key: A)

  /** Remove two or more keys from this map
   *  @param    key1 the first key to be removed
   *  @param    key2 the second key to be removed
   *  @param    keys the remaining keys to be removed
   */
  def -= (key1: A, key2: A, keys: A*) { this -= key1; this -= key2; this --= keys }

  /** Remove a sequence of keys from this map
   *  @param    keys the keys to be removed
   */
  def --= (keys: Iterable[A]) { this --= keys.elements }

  /** Remove a sequence of keys from this map
   *  @param    keys the keys to be removed
   */
  def --= (keys: Iterator[A]) { keys foreach -= }

  /** Remove a key from this map
   *  @param    key the key to be removed
   *  @return   The map itself with the binding for <code>key</code> removed if
   *            it existed.
   */
  def - (key: A): Map[A, B] = { this -= key; this }

  /** Remove two or more keys from this map
   *  @param    key1 the first key to be removed
   *  @param    key2 the second key to be removed
   *  @param    keys the remaining keys to be removed
   *  @return   The map itself with all bindings for the given keys removed.
   */
  def - (key1: A, key2: A, keys: A*): Map[A, B] = { this.-=(key1, key2, keys: _*); this }

  /** Remove a sequence of keys from this map
   *  @param    keys the keys to be removed
   *  @return   The map itself with all bindings for <code>keys</code> removed.
   */
  def -- (keys: Iterable[A]): Map[A, B] = { this --= keys; this }

  /** Remove a sequence of keys from this map
   *  @param    keys the keys to be removed
   *  @return   The map itself with all bindings for <code>keys</code> removed.
   */
  def -- (keys: Iterator[A]): Map[A, B] = { this --= keys; this }

  /** Removes all mappings from the map. After this operation is
   *  completed, the map is empty.
   */
  def clear(): Unit = keys foreach -=

  /** This function transforms all the values of mappings contained
   *  in this map with function <code>f</code>.
   *
   * @param f  The transformation to apply
   */
  def transform(f: (A, B) => B) {
    elements foreach {
      case (key, value) => update(key, f(key, value))
    }
  }

  /** This method retains only those mappings for which the predicate
   *  <code>p</code> returns <code>true</code>.
   *
   * @param p  The test predicate
   */
  def retain(p: (A, B) => Boolean): Unit = toList foreach {
    case (key, value) => if (!p(key, value)) -=(key)
  }

  /** Send a message to this scriptable object.
   *
   *  @param cmd  the message to send.
   */
  def <<(cmd: Message[(A, B)]): Unit = cmd match {
    case Include((k, v)) => update(k, v)
    case Update((k, v)) => update(k, v)
    case Remove((k, _)) => this -= k
    case Reset() => clear
    case s: Script[_] => s.elements foreach <<
    case _ => throw new UnsupportedOperationException("message " + cmd + " not understood")
  }

  /** Return a clone of this map.
   *
   *  @return a map with the same elements.
   */
  override def clone(): Map[A, B] = super.clone().asInstanceOf[Map[A, B]]

  /** This method defines syntactic sugar for adding or modifying
   *  mappings. It is typically used in the following way:
   *  <pre>
   *  map += key -> value;
   *  </pre>
   *  @deprecated   use <code>+={key, value}</code>
   */
  @deprecated
  def +=(key: A): MapTo = new MapTo(key)

  /** <code>incl</code> can be used to add many mappings at the same time
   *  to the map. The method assumes that a mapping is represented
   *  by a <code>Pair</code> object who's first component denotes the
   *  key, and who's second component refers to the value.
   *
   * @param mappings
   * @deprecated   use <code>+=</code>
   */
  @deprecated
  def incl(mappings: (A, B)*): Unit = this ++= mappings.elements

  /** This method will remove all the mappings for the given sequence
   *  of keys from the map.
   *
   * @param keys
   * @deprecated    use <code>-=</code>
   */
  @deprecated
  def excl(keys: A*): Unit = this --= keys.elements

  @deprecated
  class MapTo(key: A) {
    def ->(value: B): Unit = update(key, value)
  }

}
