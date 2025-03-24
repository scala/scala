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
package util.hashing






/** A fast multiplicative hash by Phil Bagwell.
 */
final class ByteswapHashing[T] extends Hashing[T] {

  def hash(v: T) = byteswap32(v.##)

}


object ByteswapHashing {

  private class Chained[T](h: Hashing[T]) extends Hashing[T] {
    def hash(v: T) = byteswap32(h.hash(v))
  }

  /** Composes another `Hashing` with the Byteswap hash.
   */
  def chain[T](h: Hashing[T]): Hashing[T] = new Chained(h)

}
