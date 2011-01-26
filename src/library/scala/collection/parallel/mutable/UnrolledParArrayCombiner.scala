package scala.collection.parallel.mutable





import scala.collection.generic.Sizing
import scala.collection.mutable.ArraySeq
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.UnrolledBuffer
import scala.collection.mutable.UnrolledBuffer.Unrolled
import scala.collection.parallel.TaskSupport
import scala.collection.parallel.EnvironmentPassingCombiner
import scala.collection.parallel.unsupportedop
import scala.collection.parallel.Combiner





private[mutable] class DoublingUnrolledBuffer[T](implicit m: ClassManifest[T]) extends UnrolledBuffer[T]()(m) {
  override def calcNextLength(sz: Int) = if (sz < 10000) sz * 2 else sz
  protected override def newUnrolled = new Unrolled[T](0, new Array[T](4), null, this)
}



/** An array combiner that uses doubling unrolled buffers to store elements. */
trait UnrolledParArrayCombiner[T]
extends Combiner[T, ParArray[T]] {
self: EnvironmentPassingCombiner[T, ParArray[T]] =>
  // because size is doubling, random access is O(logn)!
  val buff = new DoublingUnrolledBuffer[Any]

  import tasksupport._

  def +=(elem: T) = {
    buff += elem
    this
  }

  def result = {
    val arrayseq = new ArraySeq[T](size)
    val array = arrayseq.array.asInstanceOf[Array[Any]]

    executeAndWaitResult(new CopyUnrolledToArray(array, 0, size))

    new ParArray(arrayseq)
  }

  def clear {
    buff.clear
  }

  override def sizeHint(sz: Int) = {
    buff.lastPtr.next = new Unrolled(0, new Array[Any](sz), null, buff)
    buff.lastPtr = buff.lastPtr.next
  }

  def combine[N <: T, NewTo >: ParArray[T]](other: Combiner[N, NewTo]): Combiner[N, NewTo] = other match {
    case that if that eq this => this // just return this
    case that: UnrolledParArrayCombiner[t] =>
      buff concat that.buff
      this
    case _ => unsupportedop("Cannot combine with combiner of different type.")
  }

  def size = buff.size

  /* tasks */

  class CopyUnrolledToArray(array: Array[Any], offset: Int, howmany: Int)
  extends Task[Unit, CopyUnrolledToArray] {
    var result = ();
    def leaf(prev: Option[Unit]) = if (howmany > 0) {
      var totalleft = howmany
      val (startnode, startpos) = findStart(offset)
      var curr = startnode
      var pos = startpos
      var arroffset = offset
      while (totalleft > 0) {
        val lefthere = math.min(totalleft, curr.size - pos)
        Array.copy(curr.array, pos, array, arroffset, lefthere)
        // println("from: " + arroffset + " elems " + lefthere + " - " + pos + ", " + curr + " -> " + array.toList + " by " + this + " !! " + buff.headPtr)
        totalleft -= lefthere
        arroffset += lefthere
        pos = 0
        curr = curr.next
      }
    }
    private def findStart(pos: Int) = {
      var left = pos
      var node = buff.headPtr
      while ((left - node.size) >= 0) {
        left -= node.size
        node = node.next
      }
      (node, left)
    }
    def split = {
      val fp = howmany / 2
      List(new CopyUnrolledToArray(array, offset, fp), new CopyUnrolledToArray(array, offset + fp, howmany - fp))
    }
    def shouldSplitFurther = howmany > collection.parallel.thresholdFromSize(size, parallelismLevel)
    override def toString = "CopyUnrolledToArray(" + offset + ", " + howmany + ")"
  }
}



object UnrolledParArrayCombiner {
  def apply[T](): UnrolledParArrayCombiner[T] = new UnrolledParArrayCombiner[T] with EnvironmentPassingCombiner[T, ParArray[T]]
}


/** An array combiner that uses a chain of arraybuffers to store elements. */
trait ResizableParArrayCombiner[T]
extends LazyCombiner[T, ParArray[T], ExposedArrayBuffer[T]]
{
self: EnvironmentPassingCombiner[T, ParArray[T]] =>
  import tasksupport._

  override def sizeHint(sz: Int) = if (chain.length == 1) chain(0).sizeHint(sz)

  def newLazyCombiner(c: ArrayBuffer[ExposedArrayBuffer[T]]) = ResizableParArrayCombiner(c)

  def allocateAndCopy = if (chain.size > 1) {
    val arrayseq = new ArraySeq[T](size)
    val array = arrayseq.array.asInstanceOf[Array[Any]]

    executeAndWaitResult(new CopyChainToArray(array, 0, size))

    new ParArray(arrayseq)
  } else { // optimisation if there is only 1 array
    val pa = new ParArray(new ExposedArraySeq[T](chain(0).internalArray, size))
    pa
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
    def shouldSplitFurther = howmany > collection.parallel.thresholdFromSize(size, parallelismLevel)
  }

}


object ResizableParArrayCombiner {
  def apply[T](c: ArrayBuffer[ExposedArrayBuffer[T]]): ResizableParArrayCombiner[T] = {
    new { val chain = c } with ResizableParArrayCombiner[T] with EnvironmentPassingCombiner[T, ParArray[T]]
  }
  def apply[T](): ResizableParArrayCombiner[T] = apply(new ArrayBuffer[ExposedArrayBuffer[T]] += new ExposedArrayBuffer[T])
}












