/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection.generic

// Methods could be printed by  cat MapTemplate.scala | egrep '^  (override )?def'

/** This trait implements a proxy for iterable objects. It forwards
 *  all calls to a different iterable object
 *
 *  @author  Martin Odersky
 *  @version 2.8
 */
trait MapProxyTemplate[A, +B, +This <: MapTemplate[A, B, This] with Map[A, B]]
      extends MapTemplate[A, B, This]
      with IterableProxyTemplate[(A, B), This]
{
  override def self: This

  override def get(key: A): Option[B] = self.get(key)
  override def iterator: Iterator[(A, B)] = self.iterator
  override def isEmpty: Boolean = self.isEmpty
  override def getOrElse[B1 >: B](key: A, default: => B1): B1 = self.getOrElse(key, default)
  override def apply(key: A): B = self.apply(key)
  override def contains(key: A): Boolean = self.contains(key)
  override def isDefinedAt(key: A) = self.isDefinedAt(key)
  override def keySet: Set[A] = self.keySet
  override def keysIterator: Iterator[A] = self.keysIterator
  override def keys: Iterator[A] = self.keys
  override def valueIterable: Iterable[B] = self.valueIterable
  override def valuesIterator: Iterator[B] = self.valuesIterator
  override def values: Iterator[B] = self.values
  override def default(key: A): B = self.default(key)
  override def filterKeys(p: A => Boolean) = self.filterKeys(p)
  override def mapValues[C](f: B => C) = self.mapValues(f)

  override def hashCode = self.hashCode
  override def equals(other: Any) = self equals other
}
