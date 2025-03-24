/*
 * Scala classfile decoder (https://www.scala-lang.org)
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
package tools.scalap


class ByteArrayReader(content: Array[Byte]) {

  /** the buffer containing the file
   */
  val buf: Array[Byte] = content

  /** the current input pointer
   */
  var bp: Int = 0

  /** return byte at offset 'pos'
   */
  def byteAt(pos: Int): Byte = buf(pos)

  /** read a byte
   */
  def nextByte: Byte = {
    bp += 1
    buf(bp - 1)
  }

  /** read some bytes
   */
  def nextBytes(len: Int): Array[Byte] = {
    val res = new Array[Byte](len)
    System.arraycopy(buf, bp, res, 0, len)
    bp += len
    res
  }

  /** read a character
   */
  def nextChar: Char = {
    bp += 2
    getChar(bp - 2)
  }

  /** read an integer
   */
  def nextInt: Int = {
    bp += 4
    getInt(bp - 4)
  }

  /** read a long
   */
  def nextLong: Long = {
    bp += 8
    getLong(bp - 8)
  }

  /** read a float
   */
  def nextFloat: Float = java.lang.Float.intBitsToFloat(nextInt)

  /** read a double
   */
  def nextDouble: Double = java.lang.Double.longBitsToDouble(nextLong)

  /** read an UTF8 encoded string
   */
  def nextUTF8(len: Int): String = {
    val cs = scala.io.Codec.fromUTF8(buf, bp, len)
    bp += len
    new String(cs)
  }

  /** extract a character at position bp from buf
   */
  def getChar(bp: Int): Char =
    (((buf(bp) & 0xff) << 8) + (buf(bp + 1) & 0xff)).asInstanceOf[Char]

  /** extract an integer at position bp from buf
   */
  def getInt(bp: Int): Int =
    ((buf(bp  ) & 0xff) << 24) +
    ((buf(bp + 1) & 0xff) << 16) +
    ((buf(bp + 2) & 0xff) << 8) +
     (buf(bp + 3) & 0xff)

  /** extract a long integer at position bp from buf
   */
  def getLong(bp: Int): Long =
    (getInt(bp).toLong << 32) + (getInt(bp + 4).toLong & 0xffffffffL)

  /** extract a float at position bp from buf
   */
  def getFloat(bp: Int): Float = java.lang.Float.intBitsToFloat(getInt(bp))

  /** extract a double at position bp from buf
   */
  def getDouble(bp: Int): Double = java.lang.Double.longBitsToDouble(getLong(bp))

   /** skip next 'n' bytes
  */
  def skip(n: Int): Unit = {
    bp += n
  }

}
