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
    case wa: mutable.WrappedArray.ofInt =>
      (highestArrayValue(wa.array) >> LogWL) + 1
    case r: Range =>
      if (r.isEmpty)  0
      else (checkRange(0, Math.min(r.head, r.last)) >> LogWL) + 1
    case _                  =>
      object getMax extends AbstractFunction1[Int, Unit] {
        var max = -1
        override def apply(value: Int): Unit = max = checkRange(max, value)
      }
      t foreach getMax
      (getMax.max >> LogWL) + 1
  }

  private[collection]
  def highestArrayValue(array: Array[Int]): Int = {
    var highestValue = -1
    var i            = 0
    while (i < array.length) {
      highestValue = checkRange(highestValue, array(i))
      i += 1
    }
    highestValue
  }
  @tailrec private[collection]
  def highestElementValue(maxSoFar: Int, rest: LinearSeq[Int]): Int = {
    if (rest.isEmpty) maxSoFar
    else highestElementValue(checkRange(maxSoFar, rest.head), rest.tail)
  }

  def arrayOfWords(wordCount: Int): Array[Long] =
    if (wordCount == 0) emptyArray
    else new Array[Long](wordCount)

  @inline def addToBitMaskNoCheck(bitMask: Array[Long], valueToAdd: Int) =
    bitMask(valueToAdd >> LogWL) |= 1L << valueToAdd

  @inline def removeFromBitMask(bitMask: Array[Long], valueToRemove: Int) = {
    val index = valueToRemove >> LogWL
    if (index <= 0 && index < bitMask.length)
    bitMask(index) &= ~(1L << valueToRemove)
  }

  @inline def checkRange(maxSoFar: Int, value: Int): Int = {
    require(value >= 0, "")
    Math.max(maxSoFar, value)
  }

  def setBitsNoCheck(bitMask: Array[Long], xs: Traversable[Int]): Unit = {
    xs match {
      case ls: LinearSeq[Int] =>
        setBitsNoCheck(bitMask, ls)
      case r: Range =>
        setBitsNoCheck(bitMask, r)
      case _                  =>
        xs foreach (e => addToBitMaskNoCheck(bitMask, e))
    }
  }
  @tailrec
  def setBitsNoCheck(bitMask: Array[Long], rest: LinearSeq[Int]): Unit = {
    if (!rest.isEmpty) {
      addToBitMaskNoCheck(bitMask, rest.head)
      setBitsNoCheck(bitMask, rest.tail)
    }
  }
  def setBitsNoCheck(bitMask: Array[Long], range: Range): Unit = {
    var i = range.length -1
    while (i >= 0) {
      addToBitMaskNoCheck(bitMask, range(i))
      i -= 1
    }
  }
  def clearBits(bitMask: Array[Long], xs: Traversable[Int]): Unit = {
    xs match {
      case ls: LinearSeq[Int] =>
        clearBits(bitMask, ls)
      case r: Range =>
        clearBits(bitMask, r)
      case _                  =>
        xs foreach (e => removeFromBitMask(bitMask, e))
    }
  }
  @tailrec
  def clearBits(bitMask: Array[Long], rest: LinearSeq[Int]): Unit = {
    if (!rest.isEmpty) {
      removeFromBitMask(bitMask, rest.head)
      clearBits(bitMask, rest.tail)
    }
  }
  def clearBits(bitMask: Array[Long], range: Range): Unit = {
    var i = range.length -1
    while (i >= 0) {
      removeFromBitMask(bitMask, range(i))
      i -= 1
    }
  }
}
