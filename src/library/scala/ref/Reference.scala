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

/**
 * @see `java.lang.ref.Reference`
 */
trait Reference[+T <: AnyRef] extends Function0[T] {
  /** return the underlying value */
  def apply(): T
  /** return `Some` underlying if it hasn't been collected, otherwise `None` */
  def get: Option[T]
  override def toString: String = get.map(_.toString).getOrElse("<deleted>")
  def clear(): Unit
  def enqueue(): Boolean
  def isEnqueued: Boolean
}
