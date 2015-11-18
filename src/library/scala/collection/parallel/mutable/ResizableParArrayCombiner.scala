/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection.parallel.mutable

import scala.collection.mutable.ArraySeq
import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.Task

/** An array combiner that uses a chain of arraybuffers to store elements. */
trait ResizableParArrayCombiner[T] extends LazyCombiner[T, ParArray[T], ExposedArrayBuffer[T]] {

  override def sizeHint(sz: Int) = if (chain.length == 1) chain(0).sizeHint(sz)

  // public method with private[mutable] type ExposedArrayBuffer in parameter type; cannot be overridden.
  final def newLazyCombiner(c: ArrayBuffer[ExposedArrayBuffer[T]]) = ResizableParArrayCombiner(c)

  def allocateAndCopy = if (chain.size > 1) {
    val arrayseq = new ArraySeq[T](size)
    val array = arrayseq.array.asInstanceOf[Array[Any]]

    combinerTaskSupport.executeAndWaitResult(new CopyChainToArray(array, 0, size))

    new ParArray(arrayseq)
  } else { // optimisation if there is only 1 array
    new ParArray(new ExposedArraySeq[T](chain(0).internalArray, size))
  }

  override def toString = "ResizableParArrayCombiner(" + size + "): " //+ chain

  /* tasks */

  class CopyChainToArray(array: Array[Any], offset: Int, howmany: Int) extends Task[Unit, CopyChainToArray] {
    var result = ()
    def leaf(prev: Option[Unit]) = if (howmany > 0) {
      var totalleft = howmany
      val (stbuff, stind) = findStart(offset)
      var buffind = stbuff
      var ind = stind
      var arrayIndex = offset
      while (totalleft > 0) {
        val currbuff = chain(buffind)
        val chunksize = if (totalleft < (currbuff.size - ind)) totalleft else currbuff.size - ind
        val until = ind + chunksize

        copyChunk(currbuff.internalArray, ind, array, arrayIndex, until)
        arrayIndex += chunksize
        ind += chunksize

        totalleft -= chunksize
        buffind += 1
        ind = 0
      }
    }
    private def copyChunk(buffarr: Array[AnyRef], buffStart: Int, ra: Array[Any], arrayStart: Int, until: Int) {
      Array.copy(buffarr, buffStart, ra, arrayStart, until - buffStart)
    }
    private def findStart(pos: Int) = {
      var left = pos
      var buffind = 0
      while (left >= chain(buffind).size) {
        left -= chain(buffind).size
        buffind += 1
      }
      (buffind, left)
    }
    def split = {
      val fp = howmany / 2
      List(new CopyChainToArray(array, offset, fp), new CopyChainToArray(array, offset + fp, howmany - fp))
    }
    def shouldSplitFurther = howmany > scala.collection.parallel.thresholdFromSize(size, combinerTaskSupport.parallelismLevel)
  }
}

object ResizableParArrayCombiner {
  def apply[T](c: ArrayBuffer[ExposedArrayBuffer[T]]): ResizableParArrayCombiner[T] = {
    new { val chain = c } with ResizableParArrayCombiner[T] // was: with EnvironmentPassingCombiner[T, ParArray[T]]
  }
  def apply[T](): ResizableParArrayCombiner[T] = apply(new ArrayBuffer[ExposedArrayBuffer[T]] += new ExposedArrayBuffer[T])
}
