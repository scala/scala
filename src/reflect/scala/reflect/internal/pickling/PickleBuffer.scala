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
package reflect
package internal
package pickling

import scala.annotation.tailrec

/** Variable length byte arrays, with methods for basic pickling and unpickling.
 *
 *  @param data The initial buffer
 *  @param from The first index where defined data are found
 *  @param to   The first index where new data can be written
 */
class PickleBuffer(data: Array[Byte], from: Int, to: Int) {

  var bytes = data
  var readIndex = from
  var writeIndex = to

  @inline
  private def growTo(targetCapacity: Int): Unit = {
    val bytes1 = new Array[Byte](targetCapacity)
    Array.copy(bytes, 0, bytes1, 0, writeIndex)
    bytes = bytes1
  }

  def ensureCapacity(capacity: Int) =
    if (bytes.length < writeIndex + capacity) {
      var newCapacity = bytes.length
      while (newCapacity < writeIndex + capacity) newCapacity <<= 1
      growTo(newCapacity)
    }

  // -- Basic output routines --------------------------------------------

  /** Write a byte of data */
  def writeByte(b: Int): Unit = {
    if (writeIndex == bytes.length)
      growTo(bytes.length << 1)
    bytes(writeIndex) = b.toByte
    writeIndex += 1
  }

  /** Write a natural number in big endian format, base 128.
   *  All but the last digits have bit 0x80 set.
   */
  def writeNat(x: Int) =
    writeLongNat(x.toLong & 0x00000000FFFFFFFFL)

  /**
   * Like writeNat, but for longs. This is not the same as
   * writeLong, which writes in base 256. Note that the
   * binary representation of LongNat is identical to Nat
   * if the long value is in the range Int.MIN_VALUE to
   * Int.MAX_VALUE.
   */
  def writeLongNat(x: Long): Unit = {
    def writeNatPrefix(x: Long): Unit = {
      val y = x >>> 7
      if (y != 0L) writeNatPrefix(y)
      writeByte(((x & 0x7f) | 0x80).toInt)
    }
    val y = x >>> 7
    if (y != 0L) writeNatPrefix(y)
    writeByte((x & 0x7f).toInt)
  }

  /** Write a natural number `x` at position `pos`.
   *  If number is more than one byte, shift rest of array to make space.
   */
  def patchNat(pos: Int, x: Int): Unit = {
    @tailrec
    def patchNatPrefix(x: Int): Unit = {
      writeByte(0)
      Array.copy(bytes, pos, bytes, pos+1, writeIndex - (pos+1))
      bytes(pos) = ((x & 0x7f) | 0x80).toByte
      val y = x >>> 7
      if (y != 0) patchNatPrefix(y)
    }
    bytes(pos) = (x & 0x7f).toByte
    val y = x >>> 7
    if (y != 0) patchNatPrefix(y)
  }

  /** Write a long number `x` in signed big endian format, base 256.
   *
   *  @param x The long number to be written.
   */
  def writeLong(x: Long): Unit = {
    val y = x >> 8
    val z = x & 0xff
    if (-y != (z >> 7)) writeLong(y)
    writeByte(z.toInt)
  }

  // -- Basic input routines --------------------------------------------

  /** Read a byte */
  def readByte(): Int = {
    val x = bytes(readIndex).toInt; readIndex += 1; x
  }

  /** Read a natural number in big endian format, base 128.
   *  All but the last digits have bit 0x80 set.*/
  def readNat(): Int = readLongNat().toInt

  def readLongNat(): Long = {
    var b = 0L
    var x = 0L
    do {
      b = readByte().toLong
      x = (x << 7) + (b & 0x7f)
    } while ((b & 0x80) != 0L)
    x
  }

  /** Read a long number in signed big endian format, base 256. */
  def readLong(len: Int): Long = {
    var x = 0L
    var i = 0
    while (i < len) {
      x = (x << 8) + (readByte() & 0xff)
      i += 1
    }
    val leading = 64 - (len << 3)
    x << leading >> leading
  }

  /** Returns the buffer as a sequence of (Int, Array[Byte]) representing
   *  (tag, data) of the individual entries.  Saves and restores buffer state.
   */

  def toIndexedSeq: IndexedSeq[(Int, Array[Byte])] = {
    val saved = readIndex
    readIndex = 0
    readNat() ; readNat()     // discarding version
    val result = new Array[(Int, Array[Byte])](readNat())

    result.indices foreach { index =>
      val tag = readNat()
      val len = readNat()
      val bytes = data.slice(readIndex, len + readIndex)
      readIndex += len

      result(index) = tag -> bytes
    }

    readIndex = saved
    result.toIndexedSeq
  }

  /** Perform operation `op` until the condition
   *  `readIndex == end` is satisfied.
   *  Concatenate results into a list.
   */
  def until[T](end: Int, op: () => T): List[T] =
    if (readIndex == end) List() else op() :: until(end, op)

  /** Perform operation `op` the number of
   *  times specified.  Concatenate the results into a list.
   */
  def times[T](n: Int, op: () => T): List[T] =
    if (n == 0) List() else op() :: times(n-1, op)

  /** Pickle = majorVersion_Nat minorVersion_Nat nbEntries_Nat {Entry}
   *  Entry  = type_Nat length_Nat [actual entries]
   *
   *  Assumes that the ..Version_Nat are already consumed.
   *
   *  @return an array mapping entry numbers to locations in
   *  the byte array where the entries start.
   */
  def createIndex: Array[Int] = {
    val index = new Array[Int](readNat()) // nbEntries_Nat
    for (i <- 0 until index.length) {
      index(i) = readIndex
      readByte() // skip type_Nat
      readIndex = readNat() + readIndex // read length_Nat, jump to next entry
    }
    index
  }
}
