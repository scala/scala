/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.collection
package mutable


/** This class can be used as an adaptor to create mutable sets from
 *  immutable set implementations. Only method `empty` has
 *  to be redefined if the immutable set on which this mutable set is
 *  originally based is not empty. `empty` is supposed to
 *  return the representation of an empty set.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 21/07/2003
 *  @since   1
 */
class ImmutableSetAdaptor[A](protected var set: immutable.Set[A]) extends Set[A] with Serializable {

  override def size: Int = set.size

  override def isEmpty: Boolean = set.isEmpty

  def contains(elem: A): Boolean = set.contains(elem)

  override def foreach[U](f: A =>  U): Unit = set.foreach(f)

  override def exists(p: A => Boolean): Boolean = set.exists(p)

  override def toList: List[A] = set.toList

  override def toString = set.toString

  def iterator: Iterator[A] = set.iterator

  @deprecated("use `iterator` instead", "2.8.0")
  override def elements: Iterator[A] = iterator

  def +=(elem: A): this.type = { set = set + elem; this }

  def -=(elem: A): this.type = { set = set - elem; this }

  override def clear(): Unit = { set = set.empty }

}

