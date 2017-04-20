package strawman
package collection

import scala.Int

trait Hashing[A] {

  protected final def elemHashCode(key: A): Int = key.##

  protected final def improve(hcode: Int): Int = {
    var h: Int = hcode + ~(hcode << 9)
    h = h ^ (h >>> 14)
    h = h + (h << 4)
    h ^ (h >>> 10)
  }

  protected final def computeHash(key: A): Int =
    improve(elemHashCode(key))

  /**
    * Utility method to keep a subset of all bits in a given bitmap
    *
    * Example
    *    bitmap (binary): 00000001000000010000000100000001
    *    keep (binary):                               1010
    *    result (binary): 00000001000000000000000100000000
    *
    * @param bitmap the bitmap
    * @param keep a bitmask containing which bits to keep
    * @return the original bitmap with all bits where keep is not 1 set to 0
    */
  protected def keepBits(bitmap: Int, keep: Int): Int = {
    var result = 0
    var current = bitmap
    var kept = keep
    while (kept != 0) {
      // lowest remaining bit in current
      val lsb = current ^ (current & (current - 1))
      if ((kept & 1) != 0) {
        // mark bit in result bitmap
        result |= lsb
      }
      // clear lowest remaining one bit in abm
      current &= ~lsb
      // look at the next kept bit
      kept >>>= 1
    }
    result
  }

}
