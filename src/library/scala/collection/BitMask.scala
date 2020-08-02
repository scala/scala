package scala.collection

import scala.annotation.tailrec

import scala.runtime.AbstractFunction1

/** helper methods for BitSets  */
// its private[collection] for bincompat
private[collection] object BitMask {
  import BitSetLike._

  private val emptyArray = new Array[Long](0)
  def wordCapacity(t: collection.Traversable[Int]): Int = t match {
    case ls: LinearSeq[Int] =>
      (highestElementValue(-1, ls) >> LogWL) + 1
    case _                  =>
      object getMax extends AbstractFunction1[Int, Unit] {
        var max = -1
        override def apply(value: Int): Unit = max = checkRange(max, value)
      }
      t foreach getMax
      (getMax.max >> LogWL) + 1
  }

  @tailrec private[collection]
  def highestElementValue(maxSoFar: Int, rest: LinearSeq[Int]): Int = {
    if (rest.isEmpty) maxSoFar
    else highestElementValue(checkRange(maxSoFar, rest.head), rest.tail)
  }

  def arrayOfWords(wordCount: Int): Array[Long] =
    if (wordCount == 0) emptyArray
    else new Array[Long](wordCount)

  def arrayforElement(highestElementValue: Int): Array[Long] =
    arrayOfWords((highestElementValue >> LogWL) + 1)

  @inline def addToBitMask(array: Array[Long], valueToAdd: Int) =
    array(valueToAdd >> LogWL) |= 1L << valueToAdd

  @inline def checkRange(maxSoFar: Int, value: Int): Int = {
    require(value >= 0, "")
    Math.max(maxSoFar, value)
  }

  def setBits(bitMask: Array[Long], xs: Traversable[Int]): Unit = {
    if (bitMask.length > 0)
      xs match {
        case ls: LinearSeq[Int] =>
          setBits(bitMask, ls)
        case _                  =>
          xs foreach (e => addToBitMask(bitMask, e))
      }
  }
  def setBits(array: Array[Long], rest: LinearSeq[Int]): Unit = {
    if (!rest.isEmpty) {
      addToBitMask(array, rest.head)
      setBits(array, rest.tail)
    }
  }
}
