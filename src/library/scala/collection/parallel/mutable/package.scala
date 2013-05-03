/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection.parallel

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ArraySeq
import scala.collection.generic.Sizing

package object mutable {
  /* aliases */
  type ParArrayCombiner[T] = ResizableParArrayCombiner[T]
  val ParArrayCombiner = ResizableParArrayCombiner
}

package mutable {
  /* classes and traits */
  private[mutable] trait SizeMapUtils {

    protected def calcNumElems(from: Int, until: Int, tableLength: Int, sizeMapBucketSize: Int) = {
      // find the first bucket
      val fbindex = from / sizeMapBucketSize

      // find the last bucket
      val lbindex = until / sizeMapBucketSize
      // note to self: FYI if you define lbindex as from / sizeMapBucketSize, the first branch
      // below always triggers and tests pass, so you spend a great day benchmarking and profiling

      if (fbindex == lbindex) {
        // if first and last are the same, just count between `from` and `until`
        // return this count
        countElems(from, until)
      } else {
        // otherwise count in first, then count in last
        val fbuntil = ((fbindex + 1) * sizeMapBucketSize) min tableLength
        val fbcount = countElems(from, fbuntil)
        val lbstart = lbindex * sizeMapBucketSize
        val lbcount = countElems(lbstart, until)

        // and finally count the elements in all the buckets between first and last using a sizemap
        val inbetween = countBucketSizes(fbindex + 1, lbindex)

        // return the sum
        fbcount + inbetween + lbcount
      }
    }

    protected def countElems(from: Int, until: Int): Int

    protected def countBucketSizes(fromBucket: Int, untilBucket: Int): Int
  }

  /* hack-arounds */
  private[mutable] class ExposedArrayBuffer[T] extends ArrayBuffer[T] with Sizing {
    def internalArray = array
    def setInternalSize(s: Int) = size0 = s
    override def sizeHint(len: Int) = {
      if (len > size && len >= 1) {
        val newarray = new Array[AnyRef](len)
        Array.copy(array, 0, newarray, 0, size0)
        array = newarray
      }
    }
  }

  private[mutable] class ExposedArraySeq[T](arr: Array[AnyRef], sz: Int) extends ArraySeq[T](sz) {
    override val array = arr
    override val length = sz
    override def stringPrefix = "ArraySeq"
  }
}
