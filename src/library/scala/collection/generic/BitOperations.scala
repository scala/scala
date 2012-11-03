/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection
package generic

/** Some bit operations.
 *
 *  See http://www.drmaciver.com/2008/08/unsigned-comparison-in-javascala/ for
 *  an explanation of unsignedCompare.
 */
private[collection] object BitOperations {
  trait Int {
    type Int = scala.Int
    def zero(i: Int, mask: Int)                 = (i & mask) == 0
    def mask(i: Int, mask: Int)                 = i & (complement(mask - 1) ^ mask)
    def hasMatch(key: Int, prefix: Int, m: Int) = mask(key, m) == prefix
    def unsignedCompare(i: Int, j: Int)         = (i < j) ^ (i < 0) ^ (j < 0)
    def shorter(m1: Int, m2: Int)               = unsignedCompare(m2, m1)
    def complement(i: Int)                      = (-1) ^ i
    def bits(num: Int)                          = 31 to 0 by -1 map (i => (num >>> i & 1) != 0)
    def bitString(num: Int, sep: String = "")   = bits(num) map (b => if (b) "1" else "0") mkString sep

    def highestOneBit(j: Int) = {
      var i = j
      i |= (i >>  1)
      i |= (i >>  2)
      i |= (i >>  4)
      i |= (i >>  8)
      i |= (i >> 16)
      i - (i >>> 1)
    }
  }
  object Int extends Int

  trait Long {
    type Long = scala.Long
    def zero(i: Long, mask: Long)                  = (i & mask) == 0L
    def mask(i: Long, mask: Long)                  = i & (complement(mask - 1) ^ mask)
    def hasMatch(key: Long, prefix: Long, m: Long) = mask(key, m) == prefix
    def unsignedCompare(i: Long, j: Long)          = (i < j) ^ (i < 0L) ^ (j < 0L)
    def shorter(m1: Long, m2: Long)                = unsignedCompare(m2, m1)
    def complement(i: Long)                        = (-1L) ^ i
    def bits(num: Long)                            = 63L to 0L by -1L map (i => (num >>> i & 1L) != 0L)
    def bitString(num: Long, sep: String = "")     = bits(num) map (b => if (b) "1" else "0") mkString sep

    def highestOneBit(j: Long) = {
      var i = j
      i |= (i >>  1)
      i |= (i >>  2)
      i |= (i >>  4)
      i |= (i >>  8)
      i |= (i >> 16)
      i |= (i >> 32)
      i - (i >>> 1)
    }
  }
  object Long extends Long
}
