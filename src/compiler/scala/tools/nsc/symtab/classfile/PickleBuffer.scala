/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
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
class PickleBuffer(data: Array[byte], from: int, to: int) {

  var bytes = data
  var readIndex = from
  var writeIndex = to

  /** Double bytes array */
  private def dble: unit = {
    val bytes1 = new Array[byte](bytes.length * 2)
    Array.copy(bytes, 0, bytes1, 0, writeIndex)
    bytes = bytes1
  }

  def ensureCapacity(capacity: int) =
    while (bytes.length < writeIndex + capacity) dble

  // -- Basic output routines --------------------------------------------

  /** Write a byte of data */
  def writeByte(b: int): unit = {
    if (writeIndex == bytes.length) dble
    bytes(writeIndex) = b.asInstanceOf[byte]
    writeIndex = writeIndex + 1
  }

  /** Write a natural number in big endian format, base 128.
   *  All but the last digits have bit 0x80 set.
   */
  def writeNat(x: int): unit = {
    def writeNatPrefix(x: int): unit = {
      val y = x >>> 7
      if (y != 0) writeNatPrefix(y)
      writeByte((x & 0x7f) | 0x80)
    }
    val y = x >>> 7
    if (y != 0) writeNatPrefix(y)
    writeByte(x & 0x7f)
  }

  /** Write a natural number <code>x</code> at position <code>pos</code>.
   *  If number is more than one byte, shift rest of array to make space.
   *
   *  @param pos ...
   *  @param x   ...
   */
  def patchNat(pos: int, x: int): unit = {
    def patchNatPrefix(x: int): unit = {
      writeByte(0)
      Array.copy(bytes, pos, bytes, pos+1, writeIndex - (pos+1))
      bytes(pos) = ((x & 0x7f) | 0x80).asInstanceOf[byte]
      val y = x >>> 7
      if (y != 0) patchNatPrefix(y)
    }
    bytes(pos) = (x & 0x7f).asInstanceOf[byte]
    val y = x >>> 7
    if (y != 0) patchNatPrefix(y)
  }

  /** Write a long number <code>x</code> in signed big endian format, base 256.
   *
   *  @param x The long number to be written.
   */
  def writeLong(x: long): unit = {
    val y = x >> 8
    val z = x & 0xff
    if (-y != (z >> 7)) writeLong(y)
    writeByte(z.asInstanceOf[int])
  }

  // -- Basic input routines --------------------------------------------

  /** Read a byte */
  def readByte(): int = {
    val x = bytes(readIndex); readIndex = readIndex + 1; x
  }

  /** Read a natural number in big endian format, base 128.
   *  All but the last digits have bit 0x80 set.*/
  def readNat(): int = {
    var b = 0
    var x = 0
    do {
      b = readByte()
      x = (x << 7) + (b & 0x7f)
    } while ((b & 0x80) != 0);
    x
  }

  /** Read a long number in signed big endian format, base 256. */
  def readLong(len: int): long = {
    var x = 0L
    var i = 0
    while (i < len) {
      x = (x << 8) + (readByte() & 0xff)
      i = i + 1
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
  def until[T](end: int, op: () => T): List[T] =
    if (readIndex == end) List() else op() :: until(end, op);

  /** Create an index.
   *
   *  @return ...
   */
  def createIndex: Array[int] = {
    val index = new Array[int](readNat())
    for (val i <- Iterator.range(0, index.length)) {
      index(i) = readIndex
      readByte()
      readIndex = readNat() + readIndex
    }
    index
  }
}
