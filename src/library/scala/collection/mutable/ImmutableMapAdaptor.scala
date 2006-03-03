/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable;


/** This class can be used as an adaptor to create mutable maps from
 *  immutable map implementations. Only method <code>empty</code> has
 *  to be redefined if the immutable map on which this mutable map is
 *  originally based is not empty. <code>empty</code> is supposed to
 *  return the representation of an empty map.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 21/07/2003
 */
[serializable]
class ImmutableMapAdaptor[A, B](m: immutable.Map[A, B]) extends Map[A, B] {

    protected var imap = m;

    def size: Int = imap.size;

    def get(key: A): Option[B] = imap.get(key);

    override def isEmpty: Boolean = imap.isEmpty;

    override def apply(key: A): B = imap.apply(key);

    override def contains(key: A): Boolean = imap.contains(key);

    override def isDefinedAt(key: A) = imap.isDefinedAt(key);

    override def keys: Iterator[A] = imap.keys;

    override def values: Iterator[B] = imap.values;

    def elements: Iterator[Pair[A, B]] = imap.elements;

    override def foreach(f: (A, B) => Unit) = imap.foreach(f);

    override def toList: List[Pair[A, B]] = imap.toList;

    override def toString() = imap.toString();

    def update(key: A, value: B): Unit = { imap = imap.update(key, value); }

    def -=(key: A): Unit = { imap = imap - key; }

    override def clear: Unit = { imap = empty; }

    override def map(f: (A, B) => B): Unit = { imap = imap.map(f); }

    override def filter(p: (A, B) => Boolean): Unit = { imap = imap.filter(p); }

    override def mappingToString(p: Pair[A, B]) = imap.mappingToString(p);

    protected def empty: scala.collection.immutable.Map[A, B] = m;
}
