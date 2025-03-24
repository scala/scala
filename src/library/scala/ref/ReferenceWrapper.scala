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

import scala.annotation.nowarn

@nowarn("cat=deprecation")
trait ReferenceWrapper[+T <: AnyRef] extends Reference[T] with Proxy {
  val underlying: java.lang.ref.Reference[_ <: T]
  override def get = Option(underlying.get)
  def apply() = {
    val ret = underlying.get
    if (ret eq null) throw new NoSuchElementException
    ret
  }
  def clear(): Unit = underlying.clear()
  def enqueue(): Boolean = underlying.enqueue()
  def isEnqueued: Boolean = underlying.isEnqueued
  def self: java.lang.ref.Reference[_ <: T] = underlying
}

private trait ReferenceWithWrapper[T <: AnyRef] {
  val wrapper: ReferenceWrapper[T]
}
