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

package scala.ref

/**
 *  @author Sean McDirmid
 */
trait ReferenceWrapper[+T <: AnyRef] extends Reference[T] with Proxy {
  val underlying: java.lang.ref.Reference[_ <: T]
  override def get = Option(underlying.get)
  def apply() = {
    val ret = underlying.get
    if (ret eq null) throw new NoSuchElementException
    ret
  }
  def clear() = underlying.clear()
  def enqueue = underlying.enqueue
  def isEnqueued = underlying.isEnqueued
  def self = underlying
}

/**
 *  @author Philipp Haller
 */
private trait ReferenceWithWrapper[T <: AnyRef] {
  val wrapper: ReferenceWrapper[T]
}
