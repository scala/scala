/*     ___ ____ ___   __   ___   ___
**    / _// __// _ | / /  / _ | / _ \    Scala classfile decoder
**  __\ \/ /__/ __ |/ /__/ __ |/ ___/    (c) 2003-2006, LAMP/EPFL
** /____/\___/_/ |_/____/_/ |_/_/
**
*/

// $Id$

package scala.tools.scalap


class ByteArrayReader(content: Array[Byte]) {
  import java.io._

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
    bp = bp + 1
    buf(bp - 1)
  }

  /** read some bytes
   */
  def nextBytes(len: Int): Array[Byte] = {
    val res = new Array[Byte](len)
    System.arraycopy(buf, bp, res, 0, len)
    bp = bp + len
    res
  }

  /** read a character
   */
  def nextChar: Char = {
    bp = bp + 2
    (((buf(bp - 2) & 0xff) << 8) + (buf(bp - 1) & 0xff)).asInstanceOf[Char]
  }

  /** read an integer
   */
  def nextInt: Int = {
    bp = bp + 4
    ((buf(bp - 4) & 0xff) << 24) +
    ((buf(bp - 3) & 0xff) << 16) +
    ((buf(bp - 2) & 0xff) <<  8) +
     (buf(bp - 1) & 0xff)
  }

  /** read a long
   */
  def nextLong: Long =
    (nextInt.asInstanceOf[Long] << 32) + (nextInt.asInstanceOf[Long] & 0xffffffffL)

  /** read a float
   */
  def nextFloat: Float = java.lang.Float.intBitsToFloat(nextInt)

  /** read a double
   */
  def nextDouble: Double = java.lang.Double.longBitsToDouble(nextLong)

  /** read the next integer number
   */
  def nextNat: Int = {
    var x = 0
    var b: Byte = 0
    do {
      b = buf(bp)
      bp = bp + 1
      x = (x << 7) + (b & 0x7f)
    } while ((b & 0x80) != 0)
    x
  }

  /** read the next signed number in big endian format
   */
  def nextNum(n: Int): Long = {
    var x: Long = 0
    var i: Int = 0
    while (i < n) {
      x = (x << 8) + (nextByte & 0xff)
      i = i + 1
    }
    val leading: Int = 64 - (n * 8)
    x << leading >> leading
  }

  /** read an UTF8 encoded string
   */
  def nextUTF8(len: Int): String = {
    val cs: Array[Char] = new Array(len)
    var i = bp
    var j = 0
    bp = bp + len
    while (i < bp) {
      var b: Int = buf(i) & 0xFF
      i = i + 1
      if (b >= 0xE0) {
        b = ((b & 0x0F) << 12) | (buf(i) & 0x3F) << 6
        i = i + 1
        b = b | (buf(i) & 0x3F)
        i = i + 1
      } else if (b >= 0xC0) {
        b = ((b & 0x1F) << 6) | (buf(i) & 0x3F)
        i = i + 1
      }
      cs(j) = b.asInstanceOf[Char]
      j = j + 1
    }
    new String(cs, 0, j)
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
    (getInt(bp).asInstanceOf[Long] << 32) + (getInt(bp + 4).asInstanceOf[Long] & 0xffffffffL)

  /** extract a float at position bp from buf
   */
  def getFloat(bp: Int): Float = java.lang.Float.intBitsToFloat(getInt(bp))

  /** extract a double at position bp from buf
   */
  def getDouble(bp: Int): Double = java.lang.Double.longBitsToDouble(getLong(bp))

   /** skip next 'n' bytes
  */
  def skip(n: Int): Unit = {
    bp = bp + n
  }

}
