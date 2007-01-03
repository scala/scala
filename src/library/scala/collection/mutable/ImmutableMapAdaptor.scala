/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
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
[serializable]
class ImmutableMapAdaptor[A, B](protected var imap: immutable.Map[A, B])
extends Map[A, B]
{

  def size: Int = imap.size

  def get(key: A): Option[B] = imap.get(key)

  override def isEmpty: Boolean = imap.isEmpty

  override def apply(key: A): B = imap.apply(key)

  override def contains(key: A): Boolean = imap.contains(key)

  override def isDefinedAt(key: A) = imap.isDefinedAt(key)

  override def keys: Iterator[A] = imap.keys

  override def keySet: collection.Set[A] = imap.keySet

  override def values: Iterator[B] = imap.values

  def elements: Iterator[Pair[A, B]] = imap.elements

  override def toList: List[Pair[A, B]] = imap.toList

  def update(key: A, value: B): Unit = { imap = imap.update(key, value) }

  def -= (key: A): Unit = { imap = imap - key }

  override def clear: Unit = { imap = imap.empty }

  override def transform(f: (A, B) => B): Unit = { imap = imap.transform(f) }

  [deprecated] override def map[C](f: Pair[A, B] => C): Iterable[C] = {
    val f1 = f.asInstanceOf[Pair[A, B] => B]
    imap = imap transform { (x, y) => f1(Pair(x, y)) }
    null
  }

  /** @deprecated   use retain instead */
  [deprecated] override def filter(p: Pair[A, B] => Boolean): Iterable[Pair[A, B]] = {
    imap = imap.filter(p)
    this
  }

  override def retain(p: (A, B) => Boolean): Unit = {
    imap = imap.filter(xy => p(xy._1, xy._2))
  }

  override def toString() = imap.toString()
}
