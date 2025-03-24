/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.ref

class ReferenceQueue[+T <: AnyRef] {

  private[ref] val underlying: java.lang.ref.ReferenceQueue[_ <: T] = new java.lang.ref.ReferenceQueue[T]
  override def toString: String = underlying.toString

  protected def Wrapper(jref: java.lang.ref.Reference[_]): Option[Reference[T]] =
    jref match {
      case null => None
      case ref => Some(ref.asInstanceOf[ReferenceWithWrapper[T]].wrapper)
    }

  def poll: Option[Reference[T]] = Wrapper(underlying.poll)
  def remove: Option[Reference[T]] = Wrapper(underlying.remove)
  def remove(timeout: Long): Option[Reference[T]] = Wrapper(underlying.remove(timeout))

}
