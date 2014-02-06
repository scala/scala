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

import generic._

/** Buffers are used to create sequences of elements incrementally by
 *  appending, prepending, or inserting new elements. It is also
 *  possible to access and modify elements in a random access fashion
 *  via the index of the element in the current sequence.
 *
 *  @author Matthias Zenger
 *  @author Martin Odersky
 *  @version 2.8
 *  @since   1
 *
 *  @tparam A    type of the elements contained in this buffer.
 *
 *  @define Coll `Buffer`
 *  @define coll buffer
 */
trait Buffer[A] extends Seq[A]
                   with GenericTraversableTemplate[A, Buffer]
                   with BufferLike[A, Buffer[A]]
                   with scala.Cloneable {
  override def companion: GenericCompanion[Buffer] = Buffer
}

/** $factoryInfo
 *  @define coll buffer
 *  @define Coll `Buffer`
 */
object Buffer extends SeqFactory[Buffer] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, Buffer[A]] = ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]
  def newBuilder[A]: Builder[A, Buffer[A]] = new ArrayBuffer
}

/** Explicit instantiation of the `Buffer` trait to reduce class file size in subclasses. */
abstract class AbstractBuffer[A] extends AbstractSeq[A] with Buffer[A]
