/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection
package mutable

/** This class can be used as an adaptor to create mutable sets from
 *  immutable set implementations. Only method `empty` has
 *  to be redefined if the immutable set on which this mutable set is
 *  originally based is not empty. `empty` is supposed to
 *  return the representation of an empty set.
 *
 *  @author  Matthias Zenger
 *  @since   1
 */
@deprecated("adaptors are inherently unreliable and prone to performance problems", "2.11.0")
class ImmutableSetAdaptor[A](protected var set: immutable.Set[A])
extends AbstractSet[A]
   with Set[A]
   with Serializable {

  override def size: Int = set.size

  override def isEmpty: Boolean = set.isEmpty

  def contains(elem: A): Boolean = set.contains(elem)

  override def foreach[U](f: A => U): Unit = set.foreach(f)

  override def exists(p: A => Boolean): Boolean = set.exists(p)

  override def toList: List[A] = set.toList

  override def toString = set.toString

  def iterator: Iterator[A] = set.iterator

  def +=(elem: A): this.type = { set = set + elem; this }

  def -=(elem: A): this.type = { set = set - elem; this }

  override def clear(): Unit = { set = set.empty }
}
