/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable

import generic._

/** Buffers are used to create sequences of elements incrementally by
 *  appending, prepending, or inserting new elements. It is also
 *  possible to access and modify elements in a random access fashion
 *  via the index of the element in the current sequence.
  *
 *  @author Matthias Zenger
 *  @author Martin Odersky
 *  @version 2.8
  */
@cloneable
trait Buffer[A] extends Sequence[A] with BufferTemplate[A, Buffer[A]] {
  override protected[this] def newBuilder = Buffer.newBuilder
  override def traversableBuilder[B]: Builder[B, Buffer[B], Any] = Buffer.newBuilder[B]
}

/* Factory object for `Buffer` trait */
object Buffer extends SequenceFactory[Buffer] {
  type Coll = Buffer[_]
  implicit def builderFactory[A]: BuilderFactory[A, Buffer[A], Coll] = new BuilderFactory[A, Buffer[A], Coll] { def apply(from: Coll) = from.traversableBuilder[A] }
  def newBuilder[A]: Builder[A, Buffer[A], Any] = new ArrayBuffer
}

