/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.symtab.classfile

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

  /** Double bytes array */
  private def dble() {
    val bytes1 = new Array[Byte](bytes.length * 2)
    Array.copy(bytes, 0, bytes1, 0, writeIndex)
    bytes = bytes1
  }

  def ensureCapacity(capacity: Int) =
    while (bytes.length < writeIndex + capacity) dble()

  // -- Basic output routines --------------------------------------------

  /** Write a byte of data */
  def writeByte(b: Int) {
    if (writeIndex == bytes.length) dble()
    bytes(writeIndex) = b.asInstanceOf[Byte]
    writeIndex += 1
  }

  /** Write a natural number in big endian format, base 128.
   *  All but the last digits have bit 0x80 set.
   */
  def writeNat(x: Int) =
    writeLongNat(x.asInstanceOf[Long] & 0x00000000FFFFFFFFL)

  /**
   * Like writeNat, but for longs. This is not the same as
   * writeLong, which writes in base 256. Note that the
   * binary representation of LongNat is identical to Nat
   * if the long value is in the range Int.MIN_VALUE to
   * Int.MAX_VALUE.
   */
  def writeLongNat(x: Long) {
    def writeNatPrefix(x: Long) {
      val y = x >>> 7
      if (y != 0) writeNatPrefix(y)
      writeByte(((x & 0x7f) | 0x80).asInstanceOf[Int])
    }
    val y = x >>> 7
    if (y != 0) writeNatPrefix(y)
    writeByte((x & 0x7f).asInstanceOf[Int])
  }

  /** Write a natural number <code>x</code> at position <code>pos</code>.
   *  If number is more than one byte, shift rest of array to make space.
   *
   *  @param pos ...
   *  @param x   ...
   */
  def patchNat(pos: Int, x: Int) {
    def patchNatPrefix(x: Int) {
      writeByte(0)
      Array.copy(bytes, pos, bytes, pos+1, writeIndex - (pos+1))
      bytes(pos) = ((x & 0x7f) | 0x80).asInstanceOf[Byte]
      val y = x >>> 7
      if (y != 0) patchNatPrefix(y)
    }
    bytes(pos) = (x & 0x7f).asInstanceOf[Byte]
    val y = x >>> 7
    if (y != 0) patchNatPrefix(y)
  }

  /** Write a long number <code>x</code> in signed big endian format, base 256.
   *
   *  @param x The long number to be written.
   */
  def writeLong(x: Long) {
    val y = x >> 8
    val z = x & 0xff
    if (-y != (z >> 7)) writeLong(y)
    writeByte(z.asInstanceOf[Int])
  }

  // -- Basic input routines --------------------------------------------

  /** Peek at the current byte without moving the read index */
  def peekByte(): Int = bytes(readIndex)

  /** Read a byte */
  def readByte(): Int = {
    val x = bytes(readIndex); readIndex += 1; x
  }

  /** Read a natural number in big endian format, base 128.
   *  All but the last digits have bit 0x80 set.*/
  def readNat(): Int = readLongNat().asInstanceOf[Int]

  def readLongNat(): Long = {
    var b = 0L
    var x = 0L
    do {
      b = readByte()
      x = (x << 7) + (b & 0x7f)
    } while ((b & 0x80) != 0);
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

  /** Perform operation <code>op</code> until the condition
   *  <code>readIndex == end</code> is satisfied.
   *  Concatenate results into a list.
   *
   *  @param end ...
   *  @param op  ...
   *  @return    ...
   */
  def until[T](end: Int, op: () => T): List[T] =
    if (readIndex == end) List() else op() :: until(end, op);

  /** Perform operation <code>op</code> the number of
   *  times specified.  Concatenate the results into a list.
   */
  def times[T](n: Int, op: ()=>T): List[T] =
    if (n == 0) List() else op() :: times(n-1, op)

  /** Create an index.
   *
   *  @return ...
   */
  def createIndex: Array[Int] = {
    val index = new Array[Int](readNat())
    for (i <- 0 until index.length) {
      index(i) = readIndex
      readByte()
      readIndex = readNat() + readIndex
    }
    index
  }
}
