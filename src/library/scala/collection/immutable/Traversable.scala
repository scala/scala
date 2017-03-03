/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala
package collection
package immutable

import generic._
import mutable.Builder

/** A trait for traversable collections that are guaranteed immutable.
 *  $traversableInfo
 *  @define mutability immutable
 *
 *  @define usesMutableState
 *
 *    Note: Despite being an immutable collection, the implementation uses mutable state internally during
 *    construction. These state changes are invisible in single-threaded code but can lead to race conditions
 *    in some multi-threaded scenarios. The state of a new collection instance may not have been "published"
 *    (in the sense of the Java Memory Model specification), so that an unsynchronized non-volatile read from
 *    another thread may observe the object in an invalid state (see
 *    [[https://issues.scala-lang.org/browse/SI-7838 SI-7838]] for details). Note that such a read is not
 *    guaranteed to ''ever'' see the written object at all, and should therefore not be used, regardless
 *    of this issue. The easiest workaround is to exchange values between threads through a volatile var.
 */
trait Traversable[+A] extends scala.collection.Traversable[A]
//                         with GenTraversable[A]
                         with GenericTraversableTemplate[A, Traversable]
                         with TraversableLike[A, Traversable[A]]
                         with Immutable {
  override def companion: GenericCompanion[Traversable] = Traversable
  override def seq: Traversable[A] = this
}

/** $factoryInfo
 *  The current default implementation of a $Coll is a `List`.
 *  @define coll immutable traversable collection
 *  @define Coll `immutable.Traversable`
 */
object Traversable extends TraversableFactory[Traversable] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, Traversable[A]] = ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]
  def newBuilder[A]: Builder[A, Traversable[A]] = new mutable.ListBuffer
}
