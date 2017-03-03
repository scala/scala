/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala
package collection
package mutable

import scala.annotation.migration

/** This class can be used as an adaptor to create mutable maps from
 *  immutable map implementations. Only method `empty` has
 *  to be redefined if the immutable map on which this mutable map is
 *  originally based is not empty. `empty` is supposed to
 *  return the representation of an empty map.
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.0, 01/01/2007
 *  @since   1
 */
@deprecated("adaptors are inherently unreliable and prone to performance problems", "2.11.0")
class ImmutableMapAdaptor[A, B](protected var imap: immutable.Map[A, B])
extends AbstractMap[A, B]
   with Map[A, B]
   with Serializable
{

  override def size: Int = imap.size

  def get(key: A): Option[B] = imap.get(key)

  override def isEmpty: Boolean = imap.isEmpty

  override def apply(key: A): B = imap.apply(key)

  override def contains(key: A): Boolean = imap.contains(key)

  override def isDefinedAt(key: A) = imap.isDefinedAt(key)

  override def keySet: scala.collection.Set[A] = imap.keySet

  override def keysIterator: Iterator[A] = imap.keysIterator

  @migration("`keys` returns Iterable[A] rather than Iterator[A].", "2.8.0")
  override def keys: scala.collection.Iterable[A] = imap.keys

  override def valuesIterator: Iterator[B] = imap.valuesIterator

  @migration("`values` returns Iterable[B] rather than Iterator[B].", "2.8.0")
  override def values: scala.collection.Iterable[B] = imap.values

  def iterator: Iterator[(A, B)] = imap.iterator

  override def toList: List[(A, B)] = imap.toList

  override def update(key: A, value: B): Unit = { imap = imap.updated(key, value) }

  def -= (key: A): this.type = { imap = imap - key; this }

  def += (kv: (A, B)): this.type = { imap = imap + kv; this }

  override def clear(): Unit = { imap = imap.empty }

  override def transform(f: (A, B) => B): this.type = { imap = imap.transform(f); this }

  override def retain(p: (A, B) => Boolean): this.type = {
    imap = imap.filter(xy => p(xy._1, xy._2))
    this
  }

  override def toString() = imap.toString()
}

