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
import scala.collection.mutable.DoublingUnrolledBuffer
import scala.collection.mutable.UnrolledBuffer.Unrolled
import scala.collection.parallel.Combiner
import scala.collection.parallel.Task

/** An array combiner that uses doubling unrolled buffers to store elements. */
trait UnrolledParArrayCombiner[T]
extends Combiner[T, ParArray[T]] {
//self: EnvironmentPassingCombiner[T, ParArray[T]] =>
  // because size is doubling, random access is O(logn)!
  val buff = new DoublingUnrolledBuffer[Any]

  def +=(elem: T) = {
    buff += elem
    this
  }

  def result = {
    val arrayseq = new ArraySeq[T](size)
    val array = arrayseq.array.asInstanceOf[Array[Any]]

    combinerTaskSupport.executeAndWaitResult(new CopyUnrolledToArray(array, 0, size))

    new ParArray(arrayseq)
  }

  def clear() {
    buff.clear()
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
    case _ => throw new UnsupportedOperationException("Cannot combine with combiner of different type.")
  }

  def size = buff.size

  /* tasks */

  class CopyUnrolledToArray(array: Array[Any], offset: Int, howmany: Int)
  extends Task[Unit, CopyUnrolledToArray] {
    var result = ()

    def leaf(prev: Option[Unit]) = if (howmany > 0) {
      var totalleft = howmany
      val (startnode, startpos) = findStart(offset)
      var curr = startnode
      var pos = startpos
      var arroffset = offset
      while (totalleft > 0) {
        val lefthere = scala.math.min(totalleft, curr.size - pos)
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
    def shouldSplitFurther = howmany > scala.collection.parallel.thresholdFromSize(size, combinerTaskSupport.parallelismLevel)
    override def toString = "CopyUnrolledToArray(" + offset + ", " + howmany + ")"
  }
}

object UnrolledParArrayCombiner {
  def apply[T](): UnrolledParArrayCombiner[T] = new UnrolledParArrayCombiner[T] {} // was: with EnvironmentPassingCombiner[T, ParArray[T]]
}

