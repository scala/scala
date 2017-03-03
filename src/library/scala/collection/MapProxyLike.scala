/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection

// Methods could be printed by  cat MapLike.scala | egrep '^  (override )?def'

/** This trait implements a proxy for Map objects. It forwards
 *  all calls to a different Map object.
 *
 *  @author  Martin Odersky
 *  @version 2.8
 *  @since   2.8
 */
@deprecated("proxying is deprecated due to lack of use and compiler-level support", "2.11.0")
trait MapProxyLike[A, +B, +This <: MapLike[A, B, This] with Map[A, B]]
      extends MapLike[A, B, This]
      with IterableProxyLike[(A, B), This]
{
  override def get(key: A): Option[B] = self.get(key)
  override def iterator: Iterator[(A, B)] = self.iterator
  override def + [B1 >: B] (kv: (A, B1)): Map[A, B1] = self.+(kv)
  override def - (key: A): This = self.-(key)
  override def isEmpty: Boolean = self.isEmpty
  override def getOrElse[B1 >: B](key: A, default: => B1): B1 = self.getOrElse(key, default)
  override def apply(key: A): B = self.apply(key)
  override def contains(key: A): Boolean = self.contains(key)
  override def isDefinedAt(key: A) = self.isDefinedAt(key)
  override def keySet: Set[A] = self.keySet
  override def keysIterator: Iterator[A] = self.keysIterator
  override def keys: Iterable[A] = self.keys
  override def values: Iterable[B] = self.values
  override def valuesIterator: Iterator[B] = self.valuesIterator
  override def default(key: A): B = self.default(key)
  override def filterKeys(p: A => Boolean) = self.filterKeys(p)
  override def mapValues[C](f: B => C) = self.mapValues(f)
  override def updated [B1 >: B](key: A, value: B1): Map[A, B1] = self.updated(key, value)
  override def + [B1 >: B] (kv1: (A, B1), kv2: (A, B1), kvs: (A, B1) *): Map[A, B1] = self.+(kv1, kv2, kvs: _*)
  override def ++[B1 >: B](xs: GenTraversableOnce[(A, B1)]): Map[A, B1] =  self.++(xs)
  override def filterNot(p: ((A, B)) => Boolean) = self filterNot p

  override def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder =
    self.addString(b, start, sep, end)
}
