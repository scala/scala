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
package util






package object hashing {

  /** Fast multiplicative hash with a nice distribution.
   */
  def byteswap32(v: Int): Int = {
    var hc = v * 0x9e3775cd
    hc = java.lang.Integer.reverseBytes(hc)
    hc * 0x9e3775cd
  }

  /** Fast multiplicative hash with a nice distribution
   *  for 64-bit values.
   */
  def byteswap64(v: Long): Long = {
    var hc = v * 0x9e3775cd9e3775cdL
    hc = java.lang.Long.reverseBytes(hc)
    hc * 0x9e3775cd9e3775cdL
  }

}
