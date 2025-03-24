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

package scala

/** This class implements a simple proxy that forwards all calls to
 *  the public, non-final methods defined in class `Any` to another
 *  object self.  Those methods are:
 *  {{{
 *    def hashCode(): Int
 *    def equals(other: Any): Boolean
 *    def toString(): String
 *  }}}
 *  '''Note:''' forwarding methods in this way will most likely create
 *  an asymmetric equals method, which is not generally recommended.
 */
@deprecated("Explicitly override hashCode, equals and toString instead.", "2.13.0")
trait Proxy extends Any {
  def self: Any

  override def hashCode: Int = self.hashCode
  override def equals(that: Any): Boolean = that match {
    case null  => false
    case _     =>
      val x = that.asInstanceOf[AnyRef]
      (x eq this.asInstanceOf[AnyRef]) || (x eq self.asInstanceOf[AnyRef]) || (x equals self)
  }
  override def toString = "" + self
}

@deprecated("All members of this object are deprecated.", "2.13.0")
object Proxy {
  /** A proxy which exposes the type it is proxying for via a type parameter.
   */
  @deprecated("Explicitly override hashCode, equals and toString instead.", "2.13.0")
  trait Typed[T] extends Any with Proxy {
    def self: T
  }
}
