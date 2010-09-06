/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection
package interfaces

import generic._

/**
 * @since 2.8
 */
trait MapMethods[A, +B, +This <: MapLike[A, B, This] with Map[A, B]]
          extends IterableMethods[(A, B), This]
             with SubtractableMethods[A, This] {
  self: Map[A, B] =>

  // abstract
  def empty: This
  def get(key: A): Option[B]
  def iterator: Iterator[(A, B)]
  def + [B1 >: B] (kv: (A, B1)): Map[A, B1]
  def - (key: A): This

  // concrete
  def getOrElse[B1 >: B](key: A, default: => B1): B1
  def apply(key: A): B
  def contains(key: A): Boolean
  def isDefinedAt(key: A): Boolean
  def keys: Iterable[A]
  def keysIterator: Iterator[A]
  def keySet: Set[A]
  def values: Iterable[B]
  def valuesIterator: Iterator[B]
  def default(key: A): B
  def filterKeys(p: A => Boolean): DefaultMap[A, B]
  def mapValues[C](f: B => C): DefaultMap[A, C]
  def updated [B1 >: B](key: A, value: B1): Map[A, B1]
  def + [B1 >: B] (elem1: (A, B1), elem2: (A, B1), elems: (A, B1) *): Map[A, B1]
  def ++[B1 >: B](xs: TraversableOnce[(A, B1)]): Map[A, B1]
}
