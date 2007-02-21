/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.immutable

import Predef._

/** <p>
 *    This class extends the <code>Map</code> interface of collections
 *    that unambiguously map keys to values (i.e. a key is mapped to at
 *    least one value).
 *  </p>
 *  <p>
 *    This class defines the interface for functional map implementations
 *    relying on immutable data structures.
 *  </p>
 *  <p>
 *    Concrete map implementations have to provide functionality for
 *    the abstract methods in <a href="../Map.html" target="contentFrame">
 *    <code>scala.collection.Map</code></a> as well as for
 *    <code>factory</code>, <code>update</code>, and <code>-</code>.
 *  </p>
 *
 *  @author  Matthias Zenger
 *  @author  Erik Stenman
 *  @author  Martin Odersky
 *  @version 1.2, 31/06/2006
 */
object Map {

  /** The empty map of this type; this is implemented as a treemap */
  def empty[A, B]: Map[A, B] = new EmptyMap[A, B]

  /** The canonical factory for this type
   */
  def apply[A, B](elems: (A, B)*) = empty[A, B] ++ elems
}

trait Map[A, +B] extends collection.Map[A, B] {

  /** This method returns a new map instance of the same class
   *  mapping keys of the same type to values of type <code>C</code>.
   */
  def empty[C]: Map[A, C]

  /** This method allows one to create a new map with an
   *  additional mapping from <code>key</code>
   *  to <code>value</code>. If the map contains already a
   *  mapping for <code>key</code>, it will be overridden by this
   *  function.
   *
   *  @param key   ...
   *  @param value ...
   *  @return      the created map
   *  @deprecated    use <code>+({A, B})</code> instead
   */
  def update [B1 >: B] (key: A, value: B1): Map[A, B1]

  /** Add a key/value pair to this map.
   *  @param    kv the key/value pair.
   *  @return   A new map with the new binding added to this map
   */
  def + [B1 >: B] (kv: (A, B1)): Map[A, B1] = update(kv._1, kv._2)

  /** Add two or more key/value pairs to this map.
   *  @param    kv1 the first key/value pair.
   *  @param    kv2 the second key/value pair.
   *  @param    kvs the remaining key/value pairs.
   *  @return   A new map with the new bindings added
   */
  def + [B1 >: B] (kv1: (A, B1), kv2: (A, B1), kvs: (A, B1)*): Map[A, B1] =
    this + kv1 + kv2 ++ kvs

  /** Add a sequence of key/value pairs to this map.
   *  @param    kvs the iterable object containing all key/value pairs.
   *  @return   A new map with the new bindings added
   */
  def ++ [B1 >: B] (kvs: Iterable[(A, B1)]): Map[A, B1] =
    ((this: Map[A, B1]) /: kvs) ((m, kv) => m + kv)

  /** Add a sequence of key/value pairs to this map.
   *  @param    kvs the iterator containing all key/value pairs.
   *  @return   A new map with the new bindings added
   */
  def ++ [B1 >: B] (kvs: Iterator[(A, B1)]): Map[A, B1] =
    ((this: Map[A, B1]) /: kvs) ((m, kv) => m + kv)

  /** Remove a key from this map
   *  @param    key the key to be removed
   *  @return   If the map does not contain a binding for <code>key</code>
   *            it is returned unchanged. Otherwise, return a new map
   *            without a binding for <code>key</code>
   */
  def - (key: A): Map[A, B]

  /** Remove two or more keys from this map
   *  @param    key1 the first key to be removed
   *  @param    key2 the second key to be removed
   *  @param    keys the remaining keys to be removed
   *  @return   A map without bindings for <code>keys</code>
   *            If the map is mutable, the bindings are removed in place
   *            and the map itself is returned.
   *            If the map is immutable, a new map with the bindings removed is returned.
   */
  def - (key1: A, key2: A, keys: A*): Map[A, B] =
    this - key1 - key2 -- keys

  /** Remove a sequence of keys from this map
   *  @param    keys the keys to be removed
   *  @return   A map without bindings for the given keys.
   *            If the map is mutable, the bindings are removed in place
   *            and the map itself is returned.
   *            If the map is immutable, a new map with the bindings removed is returned.
   */
  def -- (keys: Iterable[A]): Map[A, B] = this -- keys.elements

  /** Remove a sequence of keys from this map
   *  @param    keys the keys to be removed
   *  @return   A map without bindings for the given keys.
   *            If the map is mutable, the bindings are removed in place
   *            and the map itself is returned.
   *            If the map is immutable, a new map with the bindings removed is returned.
   */
  def -- (keys: Iterator[A]): Map[A, B] =
    (this /: keys) ((m, key) => m - key)


  /** The same map with a given default function */
  def withDefault[B1 >: B](d: A => B1): Map[A, B1] = new Map[A, B1] {
    def size = Map.this.size
    def get(key: A) = Map.this.get(key)
    def elements = Map.this.elements
    def empty[C] = Map.this.empty
    def update [B2 >: B1](key: A, value: B2): Map[A, B2] =
      Map.this.update(key, value) withDefault d
    def -(key: A): Map[A, B1] = Map.this - key withDefault d
    override def default(key: A): B1 = d(key)
  }

  /** The same map with a given default value */
  def withDefaultValue[B1 >: B](d: B1): Map[A, B1] = withDefault(x => d)

  /** This function transforms all the values of mappings contained
   *  in this map with function <code>f</code>.
   *
   *  @param f A function over keys and values
   *  @return  the updated map
   */
  def transform[C](f: (A, B) => C): Map[A, C] = {
    var res = empty[C]
    foreach { case (key, value) => res = res.update(key, f(key, value)) }
    res
  }

  /** This method removes all the mappings for which the predicate
   *  <code>p</code> returns <code>false</code>.
   *
   *  @param p A prediacte over key-value pairs
   *  @return  the updated map
   */
  override def filter(p: ((A, B)) => Boolean): Map[A, B] = {
    var res = this
    foreach {
      case kv @ (key, _) => if (!p(kv)) { res = res - key }
    }
    res
  }

  /** <p>
   *    This method defines syntactic sugar for adding a
   *    mapping. It is typically used in the following way:
   *  </p>
   *  <pre>
   *    map + key -> value
   *  </pre>
   *  @deprecated  use <code>+({A, B})</code> instead
   */
  @deprecated def +(key: A): MapTo = new MapTo(key)

  /** <code>incl</code> can be used to add many mappings at the same time
   *  to the map. The method assumes that a mapping is represented
   *  by a <code>Pair</code> object who's first component denotes the
   *  key, and who's second component refers to the value.
   *
   *  @param mappings ...
   *  @return         ...
   *  @deprecated   use <code>+</code> instead
   */
  @deprecated def incl[B1 >: B](mappings: (A, B1)*): Map[A, B1] = incl(mappings)

  /** <code>incl</code> can be used to add many mappings at the same time
   *  to the map. The method assumes that each mapping is represented
   *  by an Iterator over <code>Pair</code> objects who's first component
   *  denotes the key, and who's second component refers to the value.
   *
   *  @deprecated    use <code>++</code> instead
   */
  @deprecated def incl[B1 >: B](map: Iterable[(A, B1)]): Map[A, B1] = {
    val iter = map.elements
    var res: Map[A, B1] = this
    while (iter.hasNext) {
      val (key, value) = iter.next
      res = res.update(key, value)
    }
    res
  }

  /** This method will return a map where all the mappings
   *  for the given sequence of keys are removed from the map.
   *
   *  @param keys ...
   *  @return     the updated map
   *  @deprecated    use <code>-</code> instead
   */
  @deprecated def excl(keys: A*): Map[A, B] = excl(keys)

  /** This method removes all the mappings for keys provided by an
   *  iterator over the elements of the <code>keys</code> object.
   *
   *  @param keys ...
   *  @return     the updated map
   *  @deprecated    use <code>--</code> instead
   */
  @deprecated def excl(keys: Iterable[A]): Map[A, B] = {
    val iter = keys.elements
    var res = this
    while (iter.hasNext) {
      res = res - iter.next
    }
    res
  }

  /** This method controls how a mapping is represented in the string
   *  representation provided by method <code>toString</code>.
   *
   *  @param p ...
   *  @return  the string representation of a map entry
   */
  @deprecated def mappingToString[B1 >: B](p: (A, B1)) =
    p._1.toString() + " -> " + p._2

  /** @deprecated    use <code>+({A, B})</code> instead
   */
  @deprecated class MapTo(key: A) {
    def -> [B1 >: B](value: B1) = update(key, value)
  }
}

