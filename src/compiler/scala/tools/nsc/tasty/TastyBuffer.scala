//package scala.tools.nsc
//package tasty
//
//import util.Util.dble
//
//object TastyBuffer {
//
//  /** The number of digits of the natural number `nat`, written in base 128 format. */
//  def natSize(nat: Int): Int = {
//    def loop(n: Int, acc: Int): Int =
//      if (n < 128) acc else loop(n >>> 7, acc + 1)
//    loop(nat, 1)
//  }
//
//  /** An address pointing to an index in a Tasty buffer's byte array */
//  case class Addr(index: Int) extends AnyVal {
//    def - (delta: Int): Addr = Addr(this.index - delta)
//    def + (delta: Int): Addr = Addr(this.index + delta)
//
//    def relativeTo(base: Addr): Addr = this - base.index - AddrWidth
//
//    def ==(that: Addr): Boolean = this.index == that.index
//    def !=(that: Addr): Boolean = this.index != that.index
//  }
//
//  val NoAddr: Addr = Addr(-1)
//
//  /** The maximal number of address bytes.
//   *  Since addresses are written as base-128 natural numbers,
//   *  the value of 4 gives a maximal array size of 256M.
//   */
//  final val AddrWidth = 4
//
//  /** An address referring to a serialized name */
//  case class NameRef(index: Int) extends AnyVal
//}
//import TastyBuffer._
//
///** A byte array buffer that can be filled with bytes or natural numbers in TASTY format,
// *  and that supports reading and patching addresses represented as natural numbers.
// */
//class TastyBuffer(initialSize: Int) {
//
//  /** The current byte array, will be expanded as needed */
//  var bytes: Array[Byte] = new Array(initialSize)
//
//  /** The number of bytes written */
//  var length: Int = 0
//
//  // -- Output routines --------------------------------------------
//
//  /** Write a byte of data. */
//  def writeByte(b: Int): Unit = {
//    if (length >= bytes.length)
//      bytes = dble(bytes)
//    bytes(length) = b.toByte
//    length += 1
//  }
//
//  /** Write the first `n` bytes of `data`. */
//  def writeBytes(data: Array[Byte], n: Int): Unit = {
//    while (bytes.length < length + n) bytes = dble(bytes)
//    System.arraycopy(data, 0, bytes, length, n)
//    length += n
//  }
//
//  /** Write a natural number in big endian format, base 128.
//   *  All but the last digits have bit 0x80 set.
//   */
//  def writeNat(x: Int): Unit =
//    writeLongNat(x.toLong & 0x00000000FFFFFFFFL)
//
//  /** Write a natural number in 2's complement big endian format, base 128.
//   *  All but the last digits have bit 0x80 set.
//   */
//  def writeInt(x: Int): Unit =
//    writeLongInt(x)
//
//  /**
//   * Like writeNat, but for longs. Note that the
//   * binary representation of LongNat is identical to Nat
//   * if the long value is in the range Int.MIN_VALUE to
//   * Int.MAX_VALUE.
//   */
//  def writeLongNat(x: Long): Unit = {
//    def writePrefix(x: Long): Unit = {
//      val y = x >>> 7
//      if (y != 0L) writePrefix(y)
//      writeByte((x & 0x7f).toInt)
//    }
//    val y = x >>> 7
//    if (y != 0L) writePrefix(y)
//    writeByte(((x & 0x7f) | 0x80).toInt)
//  }
//
//  /** Like writeInt, but for longs */
//  def writeLongInt(x: Long): Unit = {
//    def writePrefix(x: Long): Unit = {
//      val y = x >> 7
//      if (y != 0L - ((x >> 6) & 1)) writePrefix(y)
//      writeByte((x & 0x7f).toInt)
//    }
//    val y = x >> 7
//    if (y != 0L - ((x >> 6) & 1)) writePrefix(y)
//    writeByte(((x & 0x7f) | 0x80).toInt)
//  }
//
//  /** Write an uncompressed Long stored in 8 bytes in big endian format */
//  def writeUncompressedLong(x: Long): Unit = {
//    var y = x
//    val bytes = new Array[Byte](8)
//    for (i <- 7 to 0 by -1) {
//      bytes(i) = (y & 0xff).toByte
//      y = y >>> 8
//    }
//    writeBytes(bytes, 8)
//  }
//
//  // -- Address handling --------------------------------------------
//
//  /** Write natural number `x` right-adjusted in a field of `width` bytes
//   *  starting with address `at`.
//   */
//  def putNat(at: Addr, x: Int, width: Int): Unit = {
//    var y = x
//    var w = width
//    if(at.index + w >= bytes.length)
//      bytes = dble(bytes)
//    var digit = y & 0x7f | 0x80
//    while (w > 0) {
//      w -= 1
//      bytes(at.index + w) = digit.toByte
//      y >>>= 7
//      digit = y & 0x7f
//    }
//    assert(y == 0, s"number $x too large to fit in $width bytes")
//  }
//
//  /** The byte at given address */
//  def getByte(at: Addr): Int = bytes(at.index)
//
//  /** The natural number at address `at` */
//  def getNat(at: Addr): Int = getLongNat(at).toInt
//
//  /** The long natural number at address `at` */
//  def getLongNat(at: Addr): Long = {
//    var b = 0L
//    var x = 0L
//    var idx = at.index
//    while ({
//      b = bytes(idx)
//      x = (x << 7) | (b & 0x7f)
//      idx += 1
//      (b & 0x80) == 0
//    }) ()
//    x
//  }
//
//  /** The address (represented as a natural number) at address `at` */
//  def getAddr(at: Addr): Addr = Addr(getNat(at))
//
//  /** The smallest address equal to or following `at` which points to a non-zero byte */
//  final def skipZeroes(at: Addr): Addr =
//    if (getByte(at) != 0) at else skipZeroes(at + 1)
//
//  /** The address after the natural number found at address `at`. */
//  final def skipNat(at: Addr): Addr = {
//    val next = at + 1
//    if ((getByte(at) & 0x80) != 0) next else skipNat(next)
//  }
//
//  /** The address referring to the end of data written so far */
//  def currentAddr: Addr = Addr(length)
//
//  /** Reserve `AddrWidth` bytes to write an address into */
//  def reserveAddr(): Addr = {
//    val result = currentAddr
//    length += AddrWidth
//    result
//  }
//
//  /** Fill reserved space at address `at` with address `target` */
//  def fillAddr(at: Addr, target: Addr): Unit =
//    putNat(at, target.index, AddrWidth)
//
//  /** Write address without leading zeroes */
//  def writeAddr(addr: Addr): Unit = writeNat(addr.index)
//
//  // -- Finalization --------------------------------------------
//
//  /** Hook to be overridden in subclasses.
//   *  Perform all actions necessary to assemble the final byte array.
//   *  After `assemble` no more output actions to this buffer are permitted.
//   */
//  def assemble(): Unit = ()
//}
