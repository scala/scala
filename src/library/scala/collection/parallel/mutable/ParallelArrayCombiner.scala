package scala.collection.parallel.mutable





import scala.collection.generic.Sizing
import scala.collection.mutable.ArraySeq
import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.TaskSupport
import scala.collection.parallel.EnvironmentPassingCombiner







trait ParallelArrayCombiner[T]
extends LazyCombiner[T, ParallelArray[T], ExposedArrayBuffer[T]]
   with TaskSupport {
  self: EnvironmentPassingCombiner[T, ParallelArray[T]] =>

  override def sizeHint(sz: Int) = if (chain.length == 1) chain(0).sizeHint(sz)

  def newLazyCombiner(c: ArrayBuffer[ExposedArrayBuffer[T]]) = ParallelArrayCombiner(c)

  def allocateAndCopy = if (chain.size > 1) {
    val arrayseq = new ArraySeq[T](size)
    val array = arrayseq.array.asInstanceOf[Array[Any]]

    executeAndWait(new CopyChainToArray(array, 0, size))

    new ParallelArray(arrayseq)
  } else { // optimisation if there is only 1 array
    val pa = new ParallelArray(new ExposedArraySeq[T](chain(0).internalArray, size))
    pa
  }

  override def toString = "ParallelArrayCombiner(" + size + "): " + chain

  /* tasks */

  class CopyChainToArray(array: Array[Any], offset: Int, howmany: Int) extends super.Task[Unit, CopyChainToArray] {
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
    def shouldSplitFurther = howmany > collection.parallel.thresholdFromSize(size, parallelismLevel)
  }

}


object ParallelArrayCombiner {
  def apply[T](c: ArrayBuffer[ExposedArrayBuffer[T]]): ParallelArrayCombiner[T] = {
    new { val chain = c } with ParallelArrayCombiner[T] with EnvironmentPassingCombiner[T, ParallelArray[T]]
  }
  def apply[T]: ParallelArrayCombiner[T] = apply(new ArrayBuffer[ExposedArrayBuffer[T]] += new ExposedArrayBuffer[T])
}












