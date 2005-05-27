/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2005, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.collection.mutable;

/** This trait represents mutable maps. Concrete map implementations
 *  just have to provide functionality for the abstract methods in
 *  <code>scala.collection.Map</code> as well as for <code>update</code>,
 *  and <code>remove</code>.
 *
 *  @author  Matthias Zenger
 *  @version 1.1, 09/05/2004
 */
trait Map[A, B] extends AnyRef with scala.collection.Map[A, B] with Scriptable[Message[Pair[A, B]]] with Cloneable {

  /** This method allows one to add a new mapping from <code>key</code>
   *  to <code>value</code> to the map. If the map already contains a
   *  mapping for <code>key</code>, it will be overridden by this
   *  function.
   *
   * @param key
   * @param value
   */
  def update(key: A, value: B): Unit;

  /** This method defines syntactic sugar for adding or modifying
   *  mappings. It is typically used in the following way:
   *  <pre>
   *  map += key -> value;
   *  </pre>
   */
  def +=(key: A): MapTo = new MapTo(key);

  /** This method adds all the mappings provided by an iterator of
   *  parameter <code>map</code> to the map.
   *
   * @param map
   */
  def ++=(map: Iterable[Pair[A, B]]): Unit = ++=(map.elements);

  /** This method adds all the mappings provided by an iterator of
   *  parameter <code>map</code> to the map.
   *
   * @param it
   */
  def ++=(it: Iterator[Pair[A, B]]): Unit = it foreach {
    case Pair(key, value) => update(key, value);
  }

  /** <code>incl</code> can be used to add many mappings at the same time
   *  to the map. The method assumes that a mapping is represented
   *  by a <code>Pair</code> object who's first component denotes the
   *  key, and who's second component refers to the value.
   *
   * @param mappings
   */
  def incl(mappings: Pair[A, B]*): Unit = ++=(mappings.elements);

  /** This method removes a mapping from the given <code>key</code>.
   *  If the map does not contain a mapping for the given key, the
   *  method does nothing.
   */
  def -=(key: A): Unit;

  /** This method removes all the mappings for keys provided by an
   *  iterator over the elements of the <code>keys</code> object.
   *
   * @param keys
   */
  def --=(keys: Iterable[A]): Unit = --=(keys.elements);

  /** This method removes all the mappings for keys provided by an
   *  iterator over the elements of the <code>keys</code> object.
   *
   * @param it
   */
  def --=(it: Iterator[A]): Unit = it foreach -=;

  /** This method will remove all the mappings for the given sequence
   *  of keys from the map.
   *
   * @param keys
   */
  def excl(keys: A*): Unit = --=(keys.elements);

  /** Removes all mappings from the map. After this operation is
   *  completed, the map is empty.
   */
  def clear: Unit = keys foreach -=;

  /** This function transforms all the values of mappings contained
   *  in this map with function <code>f</code>.
   *
   * @param f
   */
  def map(f: (A, B) => B): Unit = elements foreach {
    case Pair(key, value) => update(key, f(key, value));
  }

  /** This method removes all the mappings for which the predicate
   *  <code>p</code> returns <code>false</code>.
   *
   * @param p
   */
  def filter(p: (A, B) => Boolean): Unit = toList foreach {
    case Pair(key, value) => if (!p(key, value)) -=(key);
  }

  /** Send a message to this scriptable object.
   *
   *  @param cmd  the message to send.
   */
  def <<(cmd: Message[Pair[A, B]]): Unit = cmd match {
    case Include(Pair(k, v)) => update(k, v);
    case Update(Pair(k, v)) => update(k, v);
    case Remove(Pair(k, _)) => this -= k;
    case Reset() => clear;
    case s: Script[Pair[A, B]] => s.elements foreach <<;
    case _ => error("message " + cmd + " not understood");
  }

  /** Return a clone of this map.
   *
   *  @return an map with the same elements.
   */
  override def clone(): Map[A, B] = super.clone().asInstanceOf[Map[A, B]];

  /** The hashCode method always yields an error, since it is not
   *  safe to use mutable maps as keys in hash tables.
   *
   *  @return never.
   */
  override def hashCode(): Int = error("unsuitable as hash key");

  /** Returns a string representation of this map which shows
   *  all the mappings.
   */
  override def toString() =
    if (size == 0)
      "{}"
    else
      "{" + {
        val iter = elements;
        var res = mappingToString(iter.next);
        while (iter.hasNext) {
          res = res + ", " + mappingToString(iter.next);
        }
        res;
      } + "}";

  /** This method controls how a mapping is represented in the string
   *  representation provided by method <code>toString</code>.
   *
   * @param p
   */
  def mappingToString(p: Pair[A, B]) = p._1.toString() + " -> " + p._2;

  class MapTo(key: A) {
    def ->(value: B): Unit = update(key, value);
  }

}
