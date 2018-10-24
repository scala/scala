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

/** This is a simple wrapper class for [[scala.collection.mutable.Set]].
 *  It is most useful for assembling customized set abstractions
 *  dynamically using object composition and forwarding.
 *
 *  @author  Matthias Zenger
 *  @since   1
 */
@deprecated("proxying is deprecated due to lack of use and compiler-level support", "2.11.0")
trait SetProxy[A] extends Set[A] with SetProxyLike[A, Set[A]] {
  override def repr = this
  override def empty = new SetProxy[A] { val self = SetProxy.this.self.empty }
  override def + (elem: A) = { self += elem ; this }
  override def - (elem: A) = { self -= elem ; this }

  def +=(elem: A) = { self += elem; this }
  def -=(elem: A) = { self -= elem; this }
}
