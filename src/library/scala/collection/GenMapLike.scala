/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection

/** A trait for all maps upon which operations may be
 *  implemented in parallel.
 *
 *  @define Coll `GenMap`
 *  @define coll general map
 *  @author Martin Odersky
 *  @author Aleksandar Prokopec
 *  @since 2.9
 *  @define mapNote
 *
 *  A map is a collection of bindings from keys to values, where there are
 *  no duplicate keys.
 */
trait GenMapLike[A, +B, +Repr] extends GenIterableLike[(A, B), Repr] with Equals with Parallelizable[(A, B), parallel.ParMap[A, B]] {
  def default(key: A): B
  def get(key: A): Option[B]
  def apply(key: A): B
  def seq: Map[A, B]
  def +[B1 >: B](kv: (A, B1)): GenMap[A, B1]
  def - (key: A): Repr

  // This hash code must be symmetric in the contents but ought not
  // collide trivially.
  override def hashCode()= scala.util.hashing.MurmurHash3.mapHash(seq)

  /**  Returns the value associated with a key, or a default value if the key is not contained in the map.
   *   @param   key      the key.
   *   @param   default  a computation that yields a default value in case no binding for `key` is
   *                     found in the map.
   *   @tparam  B1       the result type of the default computation.
   *   @return  the value associated with `key` if it exists,
   *            otherwise the result of the `default` computation.
   *   @usecase def getOrElse(key: A, default: => B): B
   *     @inheritdoc
   */
  def getOrElse[B1 >: B](key: A, default: => B1): B1

  /** Tests whether this map contains a binding for a key.
   *
   *  @param key the key
   *  @return    `true` if there is a binding for `key` in this map, `false` otherwise.
   */
  def contains(key: A): Boolean

  /** Tests whether this map contains a binding for a key. This method,
   *  which implements an abstract method of trait `PartialFunction`,
   *  is equivalent to `contains`.
   *
   *  @param key the key
   *  @return    `true` if there is a binding for `key` in this map, `false` otherwise.
   */
  def isDefinedAt(key: A): Boolean

  def keySet: GenSet[A]

  /** Collects all keys of this map in an iterable collection.
   *
   *  @return the keys of this map as an iterable.
   */
  def keys: GenIterable[A]

  /** Collects all values of this map in an iterable collection.
   *
   *  @return the values of this map as an iterable.
   */
  def values: GenIterable[B]

  /** Creates an iterator for all keys.
   *
   *  @return an iterator over all keys.
   */
  def keysIterator: Iterator[A]

  /** Creates an iterator for all values in this map.
   *
   *  @return an iterator over all values that are associated with some key in this map.
   */
  def valuesIterator: Iterator[B]

  /** Filters this map by retaining only keys satisfying a predicate.
   *  @param  p   the predicate used to test keys
   *  @return an immutable map consisting only of those key value pairs of this map where the key satisfies
   *          the predicate `p`. The resulting map wraps the original map without copying any elements.
   */
  def filterKeys(p: A => Boolean): GenMap[A, B]

  /** Transforms this map by applying a function to every retrieved value.
   *  @param  f   the function used to transform values of this map.
   *  @return a map view which maps every key of this map
   *          to `f(this(key))`. The resulting map wraps the original map without copying any elements.
   */
  def mapValues[C](f: B => C): GenMap[A, C]

  /** Compares two maps structurally; i.e., checks if all mappings
   *  contained in this map are also contained in the other map,
   *  and vice versa.
   *
   *  @param that the other map
   *  @return     `true` if both maps contain exactly the
   *              same mappings, `false` otherwise.
   */
  override def equals(that: Any): Boolean = that match {
    case that: GenMap[b, _] =>
      (this eq that) ||
      (that canEqual this) &&
      (this.size == that.size) && {
      try {
        this forall {
          case (k, v) => that.get(k.asInstanceOf[b]) match {
            case Some(`v`) =>
              true
            case _ => false
          }
        }
      } catch {
        case ex: ClassCastException => false
      }}
    case _ =>
      false
  }
}
