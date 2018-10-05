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

/** This is a simple wrapper class for [[scala.collection.immutable.Set]].
 *
 *  It is most useful for assembling customized set abstractions
 *  dynamically using object composition and forwarding.
 *
 *  @tparam A    type of the elements contained in this set proxy.
 *
 *  @since 2.8
 */
@deprecated("proxying is deprecated due to lack of use and compiler-level support.", "2.11.0")
trait SetProxy[A] extends Set[A] with SetProxyLike[A, Set[A]] {
  override def repr = this
  private def newProxy[B >: A](newSelf: Set[B]): SetProxy[B] =
    new AbstractSet[B] with SetProxy[B] { val self = newSelf }

  override def empty = newProxy(self.empty)
  override def + (elem: A) = newProxy(self + elem)
  override def - (elem: A) = newProxy(self - elem)
}
