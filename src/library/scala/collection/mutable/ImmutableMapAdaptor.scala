/* TODO: Reintegrate
* /*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable


/** This class can be used as an adaptor to create mutable maps from
 *  immutable map implementations. Only method <code>empty</code> has
 *  to be redefined if the immutable map on which this mutable map is
 *  originally based is not empty. <code>empty</code> is supposed to
 *  return the representation of an empty map.
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.0, 01/01/2007
 */
@serializable
class ImmutableMapAdaptor[A, B](protected var imap: immutable.Map[A, B])
extends Map[A, B]
{

  def size: Int = imap.size

  def get(key: A): Option[B] = imap.get(key)

  override def isEmpty: Boolean = imap.isEmpty

  override def apply(key: A): B = imap.apply(key)

  override def contains(key: A): Boolean = imap.contains(key)

  override def isDefinedAt(key: A) = imap.isDefinedAt(key)

  override def keys: collection.Set[A] = imap.keys

  override def keysIterator: Iterator[A] = imap.keysIterator

  override def values: collection.Set[A] = imap.values

  override def valuesIterator: Iterator[B] = imap.valuesIterator

  def iterator: Iterator[(A, B)] = imap.iterator

  @deprecated("use `iterator' instead") def elements = iterator

  override def toList: List[(A, B)] = imap.toList

  def update(key: A, value: B): Unit = { imap = imap.update(key, value) }

  def -= (key: A): this.type = { imap = imap - key; this }

  override def clear(): Unit = { imap = imap.empty }

  override def transform(f: (A, B) => B): Unit = { imap = imap.transform(f) }

  override def retain(p: (A, B) => Boolean): Unit = {
    imap = imap.filter(xy => p(xy._1, xy._2))
  }

  override def toString() = imap.toString()
}
*/
