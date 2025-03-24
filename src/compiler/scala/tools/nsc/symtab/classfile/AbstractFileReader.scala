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

package scala.tools.nsc
package symtab
package classfile

import java.io.{ByteArrayInputStream, DataInputStream}
import java.lang.Double.longBitsToDouble
import java.lang.Float.intBitsToFloat

import scala.tools.nsc.io.AbstractFile

/**
 * This class reads files byte per byte. Only used by ClassFileParser
 *
 * @author Philippe Altherr
 */
final class AbstractFileReader(val buf: Array[Byte]) extends DataReader {
  @deprecated("Use other constructor", "2.13.0")
  def this(file: AbstractFile) = this(file.toByteArray)

  /** the current input pointer
   */
  var bp: Int = 0

  /** read a byte
   */
  @throws(classOf[IndexOutOfBoundsException])
  def nextByte: Byte = {
    val b = buf(bp)
    bp += 1
    b
  }

  /** read some bytes
   */
  def nextBytes(len: Int): Array[Byte] = { // used in ide
    bp += len
    buf.slice(bp - len, bp)
  }

  /** read a character
   */
  def nextChar: Char =
    (((nextByte & 0xff) << 8) + (nextByte & 0xff)).toChar

  /** read an integer
   */
  def nextInt: Int =
    ((nextByte & 0xff) << 24) + ((nextByte & 0xff) << 16) +
    ((nextByte & 0xff) <<  8) +  (nextByte & 0xff)

  /** extract a byte at position bp from buf
   */
  def getByte(mybp: Int): Byte =
    buf(mybp)

  def getBytes(mybp: Int, bytes: Array[Byte]): Unit =
    System.arraycopy(buf, mybp, bytes, 0, bytes.length)

  /** extract a character at position bp from buf
   */
  def getChar(mybp: Int): Char =
    (((getByte(mybp) & 0xff) << 8) + (getByte(mybp+1) & 0xff)).toChar

  /** extract an integer at position bp from buf
   */
  def getInt(mybp: Int): Int =
    ((getByte(mybp) & 0xff) << 24) + ((getByte(mybp + 1) & 0xff) << 16) +
    ((getByte(mybp + 2) & 0xff) << 8) + (getByte(mybp + 3) & 0xff)

  /** extract a long integer at position bp from buf
   */
  def getLong(mybp: Int): Long =
    (getInt(mybp).toLong << 32) + (getInt(mybp + 4) & 0xffffffffL)

  /** extract a float at position bp from buf
   */
  def getFloat(mybp: Int): Float = intBitsToFloat(getInt(mybp))

  /** extract a double at position bp from buf
   */
  def getDouble(mybp: Int): Double = longBitsToDouble(getLong(mybp))

  def getUTF(mybp: Int, len: Int): String =
    new DataInputStream(new ByteArrayInputStream(buf, mybp, len)).readUTF

  /** skip next 'n' bytes
   */
  def skip(n: Int): Unit = { bp += n }
}
