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
package immutable

import generic._
import mutable.Builder
import scala.collection.immutable.Iterable

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
 *    [[https://github.com/scala/bug/issues/7838 scala/bug#7838]] for details). Note that such a read is not
 *    guaranteed to ''ever'' see the written object at all, and should therefore not be used, regardless
 *    of this issue. The easiest workaround is to exchange values between threads through a volatile var.
 */
trait Iterable[+A] extends scala.collection.Iterable[A]
//                         with GenTraversable[A]
                         with GenericTraversableTemplate[A, Iterable]
                         with TraversableLike[A, Iterable[A]]
                         with Immutable {
  override def companion: GenericCompanion[Iterable] = Iterable
  override def seq: Iterable[A] = this
}

/** $factoryInfo
 *  The current default implementation of a $Coll is a `List`.
 *  @define coll immutable traversable collection
 *  @define Coll `immutable.Traversable`
 */
object Iterable extends TraversableFactory[Iterable] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, Iterable[A]] = ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]
  def newBuilder[A]: Builder[A, Iterable[A]] = new mutable.ListBuffer
}
