/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable

import scala.collection.generic._

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
trait Buffer[A] extends Sequence[A]
                   with TraversableClass[A, Buffer]
                   with BufferTemplate[A, Buffer[A]] {
  override def companion: Companion[Buffer] = Buffer
}

/** Factory object for <code>Buffer</code> trait.
 */
object Buffer extends SequenceFactory[Buffer] {
  implicit def builderFactory[A]: BuilderFactory[A, Buffer[A], Coll] = new VirtualBuilderFactory[A]
  def newBuilder[A]: Builder[A, Buffer[A]] = new ArrayBuffer
}

