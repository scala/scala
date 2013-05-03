/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://www.scala-lang.org/           **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

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
