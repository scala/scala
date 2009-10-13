/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


// Questions: how to name update, appendFront, appendBack?
//            how to make them available? trait Vector? VectorLike? have another companion object VectorImpl?
//            mix in LinearSeq as well? <-- not yet

package scala.collection
package immutable

import scala.annotation.unchecked.uncheckedVariance

import scala.collection.generic._
import scala.collection.mutable.Builder

/** A subtrait of <code>collection.Vector</code> which represents sequences
 *  that cannot be mutated.
 *
 *  @since 2.8
 */
trait Vector[+A] extends Seq[A]
                    with scala.collection.Vector[A]
                    with GenericTraversableTemplate[A, Vector]
                    with VectorLike[A, Vector[A]] {
  override def companion: GenericCompanion[Vector] = Vector
}

/**
 * @since 2.8
 */
object Vector extends SeqFactory[Vector] {
  implicit def builderFactory[A]: BuilderFactory[A, Vector[A], Coll] =
    new VirtualBuilderFactory[A]
  def newBuilder[A]: Builder[A, Vector[A]] =
    NewVector.newBuilder[A]
}



// implementation classes below


trait NewVector[+A]  extends Vector[A]
                   with GenericTraversableTemplate[A, NewVector]
                   with VectorLike[A, NewVector[A]] {
  override def companion: GenericCompanion[NewVector] = NewVector
}


object NewVector extends SeqFactory[NewVector] {
  implicit def builderFactory[A]: BuilderFactory[A, NewVector[A], Coll] =
    new VirtualBuilderFactory[A]
  def newBuilder[A]: Builder[A, NewVector[A]] =
    new NewVectorBuilder[A]

  // TODO: override object empty { }
}





@serializable @SerialVersionUID(7129304555082767876L)
class NewVectorImpl[+A](startIndex: Int, endIndex: Int, focus: Int) extends NewVector[A]
                              with NewVectorPointer[A @uncheckedVariance] {

  //assert(startIndex >= 0, startIndex+"<0")
  //assert(startIndex <= endIndex, startIndex+">"+endIndex)
  //assert(focus >= 0, focus+"<0")
  //assert(focus <= endIndex, focus+">"+endIndex)

  def length = endIndex - startIndex

  override def iterator: NewVectorIterator[A] = {
    val s = new NewVectorIterator[A](startIndex, endIndex)
    s.initFrom(this)
    s.stabilize(focus)
    if (s.depth > 1) s.gotoPos(startIndex, startIndex ^ focus)
    s
  }

  // TODO: reverse
  // TODO: reverseIterator

  private def checkRangeConvert(index: Int) = {
    val idx = index + startIndex
    if (0 <= index && idx < endIndex)
      idx
    else
      throw new IndexOutOfBoundsException(index.toString)
  }

  def apply(index: Int): A = {
    val idx = checkRangeConvert(index)
//    //println("get elem: "+index + "/"+idx + "(focus:" +focus+" xor:"+(idx^focus)+" depth:"+depth+")")
    getElem(idx, idx ^ focus)
  }

  def update[B>:A](index: Int, value: B): NewVector[B] = {
    val idx = checkRangeConvert(index)
    val s = new NewVectorImpl[B](startIndex, endIndex, idx)
    s.initFrom(this)
    s.gotoPosClean(focus, idx, focus ^ idx)
    s.display0(idx & 0x1f) = value.asInstanceOf[AnyRef]
    s
  }

  override def take(n: Int): NewVector[A] = {
    if (n < 0) throw new IllegalArgumentException(n.toString)
    if (startIndex + n < endIndex) {
      dropBack0(startIndex + n)
    } else
      this
  }

  override def drop(n: Int): NewVector[A] = {
    if (n < 0) throw new IllegalArgumentException(n.toString)
    if (startIndex + n < endIndex) {
      dropFront0(startIndex + n)
    } else
      NewVector.empty
  }

  override def takeRight(n: Int): NewVector[A] = {
    if (n < 0) throw new IllegalArgumentException(n.toString)
    if (endIndex - n > startIndex) {
      dropFront0(endIndex + n)
    } else
      this
  }

  override def dropRight(n: Int): NewVector[A] = {
    if (n < 0) throw new IllegalArgumentException(n.toString)
    if (endIndex - n > startIndex) {
      dropBack0(endIndex - n)
    } else
      NewVector.empty
  }



  private def copyRange(array: Array[AnyRef], oldLeft: Int, newLeft: Int) = {
    val elems = new Array[AnyRef](32)
    System.arraycopy(array, oldLeft, elems, newLeft, 32 - Math.max(newLeft,oldLeft))
    elems
  }

  private def shiftTopLevel(oldLeft: Int, newLeft: Int) = (depth - 1) match {
    case 0 =>
      display0 = copyRange(display0, oldLeft, newLeft)
    case 1 =>
      display1 = copyRange(display1, oldLeft, newLeft)
    case 2 =>
      display2 = copyRange(display2, oldLeft, newLeft)
    case 3 =>
      display3 = copyRange(display3, oldLeft, newLeft)
    case 4 =>
      display4 = copyRange(display4, oldLeft, newLeft)
    case 5 =>
      display5 = copyRange(display5, oldLeft, newLeft)
  }

  private def cleanLeftEdge(cutIndex: Int) = (depth - 1) match {
    case 5 =>
      for (i <- 0 until ((cutIndex >>> 25) & 0x1f)) display5(i) = null
      for (i <- 0 until ((cutIndex >>> 20) & 0x1f)) display4(i) = null
      for (i <- 0 until ((cutIndex >>> 15) & 0x1f)) display3(i) = null
      for (i <- 0 until ((cutIndex >>> 10) & 0x1f)) display2(i) = null
      for (i <- 0 until ((cutIndex >>>  5) & 0x1f)) display1(i) = null
      for (i <- 0 until ((cutIndex >>>  0) & 0x1f)) display0(i) = null
    case 4 =>
      display5 = null
      for (i <- 0 until ((cutIndex >>> 20) & 0x1f)) display4(i) = null
      for (i <- 0 until ((cutIndex >>> 15) & 0x1f)) display3(i) = null
      for (i <- 0 until ((cutIndex >>> 10) & 0x1f)) display2(i) = null
      for (i <- 0 until ((cutIndex >>>  5) & 0x1f)) display1(i) = null
      for (i <- 0 until ((cutIndex >>>  0) & 0x1f)) display0(i) = null
    case 3 =>
      display5 = null
      display4 = null
      for (i <- 0 until ((cutIndex >>> 15) & 0x1f)) display3(i) = null
      for (i <- 0 until ((cutIndex >>> 10) & 0x1f)) display2(i) = null
      for (i <- 0 until ((cutIndex >>>  5) & 0x1f)) display1(i) = null
      for (i <- 0 until ((cutIndex >>>  0) & 0x1f)) display0(i) = null
    case 2 =>
      display5 = null
      display4 = null
      display3 = null
      for (i <- 0 until ((cutIndex >>> 10) & 0x1f)) display2(i) = null
      for (i <- 0 until ((cutIndex >>>  5) & 0x1f)) display1(i) = null
      for (i <- 0 until ((cutIndex >>>  0) & 0x1f)) display0(i) = null
    case 1 =>
      display5 = null
      display4 = null
      display3 = null
      display2 = null
      for (i <- 0 until ((cutIndex >>>  5) & 0x1f)) display1(i) = null
      for (i <- 0 until ((cutIndex >>>  0) & 0x1f)) display0(i) = null
    case 0 =>
      display5 = null
      display4 = null
      display3 = null
      display2 = null
      display1 = null
      for (i <- 0 until ((cutIndex >>>  0) & 0x1f)) display0(i) = null
  }

  private def cleanRightEdge(cutIndex: Int) = (depth - 1) match {
    case 5 =>
      for (i <- ((cutIndex >>> 25) & 0x1f) until 32) display5(i) = null
      for (i <- ((cutIndex >>> 20) & 0x1f) until 32) display4(i) = null
      for (i <- ((cutIndex >>> 15) & 0x1f) until 32) display3(i) = null
      for (i <- ((cutIndex >>> 10) & 0x1f) until 32) display2(i) = null
      for (i <- ((cutIndex >>>  5) & 0x1f) until 32) display1(i) = null
      for (i <- ((cutIndex >>>  0) & 0x1f) until 32) display0(i) = null
    case 4 =>
      display5 = null
      for (i <- ((cutIndex >>> 20) & 0x1f) until 32) display4(i) = null
      for (i <- ((cutIndex >>> 15) & 0x1f) until 32) display3(i) = null
      for (i <- ((cutIndex >>> 10) & 0x1f) until 32) display2(i) = null
      for (i <- ((cutIndex >>>  5) & 0x1f) until 32) display1(i) = null
      for (i <- ((cutIndex >>>  0) & 0x1f) until 32) display0(i) = null
    case 3 =>
      display5 = null
      display4 = null
      for (i <- ((cutIndex >>> 15) & 0x1f) until 32) display3(i) = null
      for (i <- ((cutIndex >>> 10) & 0x1f) until 32) display2(i) = null
      for (i <- ((cutIndex >>>  5) & 0x1f) until 32) display1(i) = null
      for (i <- ((cutIndex >>>  0) & 0x1f) until 32) display0(i) = null
    case 2 =>
      display5 = null
      display4 = null
      display3 = null
      for (i <- ((cutIndex >>> 10) & 0x1f) until 32) display2(i) = null
      for (i <- ((cutIndex >>>  5) & 0x1f) until 32) display1(i) = null
      for (i <- ((cutIndex >>>  0) & 0x1f) until 32) display0(i) = null
    case 1 =>
      display5 = null
      display4 = null
      display3 = null
      display2 = null
      for (i <- ((cutIndex >>>  5) & 0x1f) until 32) display1(i) = null
      for (i <- ((cutIndex >>>  0) & 0x1f) until 32) display0(i) = null
    case 0 =>
      display5 = null
      display4 = null
      display3 = null
      display2 = null
      display1 = null
      for (i <- ((cutIndex >>>  0) & 0x1f) until 32) display0(i) = null
  }

  private def requiredDepth(xor: Int) = {
    if (xor < (1 <<  5)) 1
    else if (xor < (1 << 10)) 2
    else if (xor < (1 << 15)) 3
    else if (xor < (1 << 20)) 4
    else if (xor < (1 << 25)) 5
    else if (xor < (1 << 30)) 6
    else throw new IllegalArgumentException()
  }

  private def dropFront0(cutIndex: Int): NewVector[A] = {
    var blockIndex = cutIndex & ~31
    var lo = cutIndex & 31

    val xor = cutIndex ^ (endIndex - 1)
    val d = requiredDepth(xor)
    val shift = (cutIndex & ~((1 << (5*d))-1))

    //println("cut front at " + cutIndex + ".." + endIndex + " (xor: "+xor+" shift: " + shift + " d: " + d +")")

    val s = new NewVectorImpl(cutIndex-shift, endIndex-shift, blockIndex-shift)
    s.initFrom(this)
    if (s.depth > 1)
      s.gotoPos(blockIndex, focus ^ blockIndex)
    s.depth = d
    s.stabilize(blockIndex-shift)
    s.cleanLeftEdge(cutIndex-shift)
    s
  }

  private def dropBack0(cutIndex: Int): NewVector[A] = {
    var blockIndex = (cutIndex - 1) & ~31
    var lo = ((cutIndex - 1) & 31) + 1

    val xor = startIndex ^ (cutIndex - 1)
    val d = requiredDepth(xor)

    //println("cut back at " + startIndex + ".." + cutIndex + " (xor: "+xor+" d: " + d +")")

    val s = new NewVectorImpl(startIndex, cutIndex, blockIndex)
    s.initFrom(this)
    if (s.depth > 1)
      s.gotoPos(blockIndex, focus ^ blockIndex)
    s.depth = d
    s.stabilize(blockIndex)
    s.cleanRightEdge(cutIndex)
    s
  }


  def appendFront[B>:A](value: B): NewVector[B] = {
    if (endIndex != startIndex) {
      var blockIndex = (startIndex - 1) & ~31
      var lo = (startIndex - 1) & 31

      if (blockIndex + 32 == startIndex) {

        val freeSpace = ((1<<5*(depth)) - endIndex)
        val shift = freeSpace & ~((1<<5*(depth-1))-1)
        val shiftBlocks = freeSpace >>> 5*(depth-1)

        //println("----- appendFront " + value + " at " + (startIndex - 1) + " reached block start")
        if (shift != 0) {
          // case A: we can shift right on the top level
          debug
          //println("shifting right by " + shiftBlocks + " at level " + (depth-1) + " (had "+freeSpace+" free space)")

          if (depth > 1) {
            val newBlockIndex = blockIndex + shift
            val newFocus = focus + shift
            val s = new NewVectorImpl(startIndex - 1 + shift, endIndex + shift, newBlockIndex)
            s.initFrom(this)
            s.shiftTopLevel(0, shiftBlocks) // shift right by n blocks
            s.debug
            s.gotoFreshPosClean(newFocus, newBlockIndex, newFocus ^ newBlockIndex)
            s.display0(lo) = value.asInstanceOf[AnyRef]
            //assert(depth == s.depth)
            s
          } else {
            val newBlockIndex = blockIndex + 32
            val newFocus = focus

            //assert(newBlockIndex == 0)
            //assert(newFocus == 0)

            val s = new NewVectorImpl(startIndex - 1 + shift, endIndex + shift, newBlockIndex)
            s.initFrom(this)
            s.shiftTopLevel(0, shiftBlocks) // shift right by n elements
            s.gotoPosClean(newFocus, newBlockIndex, newFocus ^ newBlockIndex)
            s.display0(shift-1) = value.asInstanceOf[AnyRef]
            s.debug
            s
          }
        } else if (blockIndex < 0) {
          // case B: we need to move the whole structure
          val move = (1 << 5*(depth+1)) - (1 << 5*(depth))
          //println("moving right by " + move + " at level " + (depth-1) + " (had "+freeSpace+" free space)")

          val newBlockIndex = blockIndex + move
          val newFocus = focus + move


          val s = new NewVectorImpl(startIndex - 1 + move, endIndex + move, newBlockIndex)
          s.initFrom(this)
          s.debug
          s.gotoFreshPosClean(newFocus, newBlockIndex, newFocus ^ newBlockIndex) // could optimize: we know it will create a whole branch
          s.display0(lo) = value.asInstanceOf[AnyRef]
          s.debug
          //assert(s.depth == depth+1)
          s
        } else {
          val newBlockIndex = blockIndex
          val newFocus = focus

          val s = new NewVectorImpl(startIndex - 1, endIndex, newBlockIndex)
          s.initFrom(this)
          s.gotoFreshPosClean(newFocus, newBlockIndex, newFocus ^ newBlockIndex)
          s.display0(lo) = value.asInstanceOf[AnyRef]
          //assert(s.depth == depth)
          s
        }

      } else {
//        //println("will make writable block (from "+focus+") at: " + blockIndex)
        val s = new NewVectorImpl(startIndex - 1, endIndex, blockIndex)
        s.initFrom(this)
        s.gotoPosClean(focus, blockIndex, focus ^ blockIndex)
        s.display0(lo) = value.asInstanceOf[AnyRef]
        s
      }
    } else {
      // empty vector, just insert single element at the back
      val elems = new Array[AnyRef](32)
      elems(31) = value.asInstanceOf[AnyRef]
      val s = new NewVectorImpl(31,32,0)
      s.depth = 1
      s.display0 = elems
      s
    }
  }

  def appendBack[B>:A](value: B): NewVector[B] = {
//    //println("------- append " + value)
//    debug()
    if (endIndex != startIndex) {
      var blockIndex = endIndex & ~31
      var lo = endIndex & 31

      if (endIndex == blockIndex) {
        val shift = startIndex & ~((1<<5*(depth-1))-1)
        val shiftBlocks = startIndex >>> 5*(depth-1)

        //println("----- appendBack " + value + " at " + endIndex + " reached block end")

        if (shift != 0) {
          debug
          //println("shifting left by " + shiftBlocks + " at level " + (depth-1) + " (had "+startIndex+" free space)")
          if (depth > 1) {
            val newBlockIndex = blockIndex - shift
            val newFocus = focus - shift
            val s = new NewVectorImpl(startIndex - shift, endIndex + 1 - shift, newBlockIndex)
            s.initFrom(this)
            s.shiftTopLevel(shiftBlocks, 0) // shift left by n blocks
            s.debug
            s.gotoFreshPosClean(newFocus, newBlockIndex, newFocus ^ newBlockIndex)
            s.display0(lo) = value.asInstanceOf[AnyRef]
            s.debug
            //assert(depth == s.depth)
            s
          } else {
            val newBlockIndex = blockIndex - 32
            val newFocus = focus

            //assert(newBlockIndex == 0)
            //assert(newFocus == 0)

            val s = new NewVectorImpl(startIndex - shift, endIndex + 1 - shift, newBlockIndex)
            s.initFrom(this)
            s.shiftTopLevel(shiftBlocks, 0) // shift right by n elements
            s.gotoPosClean(newFocus, newBlockIndex, newFocus ^ newBlockIndex)
            s.display0(32 - shift) = value.asInstanceOf[AnyRef]
            s.debug
            s
          }
        } else {
          val newBlockIndex = blockIndex
          val newFocus = focus

          val s = new NewVectorImpl(startIndex, endIndex + 1, newBlockIndex)
          s.initFrom(this)
          s.gotoFreshPosClean(newFocus, newBlockIndex, newFocus ^ newBlockIndex)
          s.display0(lo) = value.asInstanceOf[AnyRef]
          //assert(s.depth == depth+1) might or might not create new level!
          if (s.depth == depth+1) {
            //println("creating new level " + s.depth + " (had "+0+" free space)")
            s.debug
          }
          s
        }
      } else {
//        //println("will make writable block (from "+focus+") at: " + blockIndex)
        val s = new NewVectorImpl(startIndex, endIndex + 1, blockIndex)
        s.initFrom(this)
        s.gotoPosClean(focus, blockIndex, focus ^ blockIndex)
        s.display0(lo) = value.asInstanceOf[AnyRef]
        s
      }
    } else {
      val elems = new Array[AnyRef](32)
      elems(0) = value.asInstanceOf[AnyRef]
      val s = new NewVectorImpl(0,1,0)
      s.depth = 1
      s.display0 = elems
      s
    }
  }

}


final class NewVectorIterator[+A](_startIndex: Int, _endIndex: Int) extends Iterator[A] with NewVectorPointer[A @uncheckedVariance] {

  private var blockIndex: Int = _startIndex & ~31
  private var lo: Int = _startIndex & 31
  private var endIndex: Int = _endIndex

  private var endLo = Math.min(endIndex - blockIndex, 32)

  def hasNext = _hasNext

  private var _hasNext = blockIndex + lo < endIndex

  def next(): A = {
    if (!_hasNext) throw new NoSuchElementException("reached iterator end")

    val res = display0(lo).asInstanceOf[A]
    lo += 1

    if (lo == endLo) {
      if (blockIndex + lo < endIndex) {
        val newBlockIndex = blockIndex+32
        gotoNextBlockStart(newBlockIndex, blockIndex ^ newBlockIndex)

        blockIndex = newBlockIndex
        endLo = Math.min(endIndex - blockIndex, 32)
        lo = 0
      } else {
        _hasNext = false
      }
    }

    res
  }

  // TODO: take
  // TODO: drop
}


final class NewVectorBuilder[A]() extends Builder[A,NewVector[A]] with NewVectorPointer[A @uncheckedVariance] {

  display0 = new Array[AnyRef](32)
  depth = 1

  var blockIndex = 0
  var lo = 0

  def += (elem: A): this.type = {
    if (lo == 32) {
      val newBlockIndex = blockIndex+32
      gotoNextBlockStartClean(newBlockIndex, blockIndex ^ newBlockIndex)
      blockIndex = newBlockIndex
      lo = 0
    }
    display0(lo) = elem.asInstanceOf[AnyRef]
    lo += 1
    this
  }

  def result: NewVector[A] = {
    val s = new NewVectorImpl[A](0, blockIndex + lo, 0) // TODO: should focus front or back?
    s.initFrom(this)
    if (depth > 1) s.gotoPos(0, blockIndex + lo)
    s
  }

  def clear: Unit = {
    display0 = new Array[AnyRef](32)
    depth = 1
    blockIndex = 0
    lo = 0
  }
}



trait NewVectorPointer[T] {
    var depth: Int = _
    var display0: Array[AnyRef] = _
    var display1: Array[AnyRef] = _
    var display2: Array[AnyRef] = _
    var display3: Array[AnyRef] = _
    var display4: Array[AnyRef] = _
    var display5: Array[AnyRef] = _

    // used
    final def initFrom[U](that: NewVectorPointer[U]) = {
      depth = that.depth
      (depth - 1) match {
        case 0 =>
          display0 = that.display0
        case 1 =>
          display1 = that.display1
          display0 = that.display0
        case 2 =>
          display2 = that.display2
          display1 = that.display1
          display0 = that.display0
        case 3 =>
          display3 = that.display3
          display2 = that.display2
          display1 = that.display1
          display0 = that.display0
        case 4 =>
          display4 = that.display4
          display3 = that.display3
          display2 = that.display2
          display1 = that.display1
          display0 = that.display0
        case 5 =>
          display5 = that.display5
          display4 = that.display4
          display3 = that.display3
          display2 = that.display2
          display1 = that.display1
          display0 = that.display0
      }
    }

    // used
    final def getElem(index: Int, xor: Int): T = {
      if (xor < (1 << 5)) { // level = 0
        display0(index & 31).asInstanceOf[T]
      } else
      if (xor < (1 << 10)) { // level = 1
        display1((index >> 5) & 31).asInstanceOf[Array[AnyRef]](index & 31).asInstanceOf[T]
      } else
      if (xor < (1 << 15)) { // level = 2
        display2((index >> 10) & 31).asInstanceOf[Array[AnyRef]]((index >> 5) & 31).asInstanceOf[Array[AnyRef]](index & 31).asInstanceOf[T]
      } else
      if (xor < (1 << 20)) { // level = 3
        display3((index >> 15) & 31).asInstanceOf[Array[AnyRef]]((index >> 10) & 31).asInstanceOf[Array[AnyRef]]((index >> 5) & 31).asInstanceOf[Array[AnyRef]](index & 31).asInstanceOf[T]
      } else
      if (xor < (1 << 25)) { // level = 4
        display4((index >> 20) & 31).asInstanceOf[Array[AnyRef]]((index >> 15) & 31).asInstanceOf[Array[AnyRef]]((index >> 10) & 31).asInstanceOf[Array[AnyRef]]((index >> 5) & 31).asInstanceOf[Array[AnyRef]](index & 31).asInstanceOf[T]
      } else
      if (xor < (1 << 30)) { // level = 5
        display5((index >> 25) & 31).asInstanceOf[Array[AnyRef]]((index >> 20) & 31).asInstanceOf[Array[AnyRef]]((index >> 15) & 31).asInstanceOf[Array[AnyRef]]((index >> 10) & 31).asInstanceOf[Array[AnyRef]]((index >> 5) & 31).asInstanceOf[Array[AnyRef]](index & 31).asInstanceOf[T]
      } else { // level = 6
        throw new IllegalArgumentException()
      }
    }

    final def gotoZeroInit(elems: Array[AnyRef]) = (depth - 1) match { // goto pos zero
      case 5 =>
        display5 = elems.asInstanceOf[Array[AnyRef]]
        display4 = display5(0).asInstanceOf[Array[AnyRef]]
        display3 = display4(0).asInstanceOf[Array[AnyRef]]
        display2 = display3(0).asInstanceOf[Array[AnyRef]]
        display1 = display2(0).asInstanceOf[Array[AnyRef]]
        display0 = display1(0).asInstanceOf[Array[AnyRef]]
      case 4 =>
        display4 = elems.asInstanceOf[Array[AnyRef]]
        display3 = display4(0).asInstanceOf[Array[AnyRef]]
        display2 = display3(0).asInstanceOf[Array[AnyRef]]
        display1 = display2(0).asInstanceOf[Array[AnyRef]]
        display0 = display1(0).asInstanceOf[Array[AnyRef]]
      case 3 =>
        display3 = elems.asInstanceOf[Array[AnyRef]]
        display2 = display3(0).asInstanceOf[Array[AnyRef]]
        display1 = display2(0).asInstanceOf[Array[AnyRef]]
        display0 = display1(0).asInstanceOf[Array[AnyRef]]
      case 2 =>
        display2 = elems.asInstanceOf[Array[AnyRef]]
        display1 = display2(0).asInstanceOf[Array[AnyRef]]
        display0 = display1(0).asInstanceOf[Array[AnyRef]]
      case 1 =>
        display1 = elems.asInstanceOf[Array[AnyRef]]
        display0 = display1(0).asInstanceOf[Array[AnyRef]]
      case 0 =>
        display0 = elems.asInstanceOf[Array[AnyRef]]
    }

    // USED BY ITERATOR

    // xor: oldIndex ^ index
    final def gotoNextBlockStart(index: Int, xor: Int): Unit = { // goto block start pos
      if (xor < (1 << 10)) { // level = 1
        display0 = display1((index >> 5) & 31).asInstanceOf[Array[AnyRef]]
      } else
      if (xor < (1 << 15)) { // level = 2
        display1 = display2((index >> 10) & 31).asInstanceOf[Array[AnyRef]]
        display0 = display1(0).asInstanceOf[Array[AnyRef]]
      } else
      if (xor < (1 << 20)) { // level = 3
        display2 = display3((index >> 15) & 31).asInstanceOf[Array[AnyRef]]
        display1 = display2(0).asInstanceOf[Array[AnyRef]]
        display0 = display1(0).asInstanceOf[Array[AnyRef]]
      } else
      if (xor < (1 << 25)) { // level = 4
        display3 = display4((index >> 20) & 31).asInstanceOf[Array[AnyRef]]
        display2 = display3(0).asInstanceOf[Array[AnyRef]]
        display1 = display2(0).asInstanceOf[Array[AnyRef]]
        display0 = display1(0).asInstanceOf[Array[AnyRef]]
      } else
      if (xor < (1 << 30)) { // level = 5
        display4 = display5((index >> 25) & 31).asInstanceOf[Array[AnyRef]]
        display3 = display4(0).asInstanceOf[Array[AnyRef]]
        display2 = display3(0).asInstanceOf[Array[AnyRef]]
        display1 = display2(0).asInstanceOf[Array[AnyRef]]
        display0 = display1(0).asInstanceOf[Array[AnyRef]]
      } else { // level = 6
        throw new IllegalArgumentException()
      }
    }

    // USED BY BUILDER

    // xor: oldIndex ^ index
    final def gotoNextBlockStartClean(index: Int, xor: Int): Unit = { // goto block start pos
      if (xor < (1 << 10)) { // level = 1
        if (depth == 1) { display1 = new Array(32); display1(0) = display0; depth+=1}
        display0 = new Array(32)
        display1((index >>  5) & 31) = display0
      } else
      if (xor < (1 << 15)) { // level = 2
        if (depth == 2) { display2 = new Array(32); display2(0) = display1; depth+=1}
        display0 = new Array(32)
        display1 = new Array(32)
        display1((index >>  5) & 31) = display0
        display2((index >> 10) & 31) = display1
      } else
      if (xor < (1 << 20)) { // level = 3
        if (depth == 3) { display3 = new Array(32); display3(0) = display2; depth+=1}
        display0 = new Array(32)
        display1 = new Array(32)
        display2 = new Array(32)
        display1((index >>  5) & 31) = display0
        display2((index >> 10) & 31) = display1
        display3((index >> 15) & 31) = display2
      } else
      if (xor < (1 << 25)) { // level = 4
        if (depth == 4) { display4 = new Array(32); display4(0) = display3; depth+=1}
        display0 = new Array(32)
        display1 = new Array(32)
        display2 = new Array(32)
        display3 = new Array(32)
        display1((index >>  5) & 31) = display0
        display2((index >> 10) & 31) = display1
        display3((index >> 15) & 31) = display2
        display4((index >> 20) & 31) = display3
      } else
      if (xor < (1 << 30)) { // level = 5
        if (depth == 5) { display5 = new Array(32); display5(0) = display4; depth+=1}
        display0 = new Array(32)
        display1 = new Array(32)
        display2 = new Array(32)
        display3 = new Array(32)
        display4 = new Array(32)
        display1((index >>  5) & 31) = display0
        display2((index >> 10) & 31) = display1
        display3((index >> 15) & 31) = display2
        display4((index >> 20) & 31) = display3
        display5((index >> 25) & 31) = display4
      } else { // level = 6
        throw new IllegalArgumentException()
      }
    }



    // STUFF BELOW USED BY APPEND

    final def copyOf(a: Array[AnyRef]) = {
//      //println("copy")
      val b = new Array[AnyRef](a.length)
      System.arraycopy(a, 0, b, 0, a.length)
      b
    }

    final def nullSlotAndCopy(array: Array[AnyRef], index: Int) = {
//      //println("copy and null")
      val x = array(index)
//TODO
//      array(index) = null
      copyOf(x.asInstanceOf[Array[AnyRef]])
    }


    // USED IN APPEND
    // create a new block at the bottom level, and possible ex

    final def gotoFreshPosClean(oldIndex: Int, newIndex: Int, xor: Int): Unit = { // goto block start pos
      if (xor < (1 << 5)) { // level = 0
        //println("XXX clean with low xor")
      } else
      if (xor < (1 << 10)) { // level = 1
        if (depth == 1) {
          display1 = new Array(32)
          display1((oldIndex >>  5) & 31) = display0
          depth +=1
        }
        display0 = new Array(32)
      } else
      if (xor < (1 << 15)) { // level = 2
        if (depth == 2) {
          display2 = new Array(32)
          display2((oldIndex >> 10) & 31) = display1
          depth +=1
        }
        display1 = display2((newIndex >> 10) & 31).asInstanceOf[Array[AnyRef]]
        if (display1 == null) display1 = new Array(32)
        display0 = new Array(32)
      } else
      if (xor < (1 << 20)) { // level = 3
        if (depth == 3) {
          display3 = new Array(32)
          display3((oldIndex >> 15) & 31) = display2
          display2 = new Array(32)
          display1 = new Array(32)
          depth +=1
        }
        display2 = display3((newIndex >> 15) & 31).asInstanceOf[Array[AnyRef]]
        if (display2 == null) display2 = new Array(32)
        display1 = display2((newIndex >> 10) & 31).asInstanceOf[Array[AnyRef]]
        if (display1 == null) display1 = new Array(32)
        display0 = new Array(32)
      } else
      if (xor < (1 << 25)) { // level = 4
        if (depth == 4) {
          display4 = new Array(32)
          display4((oldIndex >> 20) & 31) = display3
          display3 = new Array(32)
          display2 = new Array(32)
          display1 = new Array(32)
          depth +=1
        }
        display3 = display4((newIndex >> 20) & 31).asInstanceOf[Array[AnyRef]]
        if (display3 == null) display3 = new Array(32)
        display2 = display3((newIndex >> 15) & 31).asInstanceOf[Array[AnyRef]]
        if (display2 == null) display2 = new Array(32)
        display1 = display2((newIndex >> 10) & 31).asInstanceOf[Array[AnyRef]]
        if (display1 == null) display1 = new Array(32)
        display0 = new Array(32)
      } else
      if (xor < (1 << 30)) { // level = 5
        if (depth == 5) {
          display5 = new Array(32)
          display5((oldIndex >>  25) & 31) = display4
          display4 = new Array(32)
          display3 = new Array(32)
          display2 = new Array(32)
          display1 = new Array(32)
          depth +=1
        }
        display4 = display5((newIndex >> 20) & 31).asInstanceOf[Array[AnyRef]]
        if (display4 == null) display4 = new Array(32)
        display3 = display4((newIndex >> 20) & 31).asInstanceOf[Array[AnyRef]]
        if (display3 == null) display3 = new Array(32)
        display2 = display3((newIndex >> 15) & 31).asInstanceOf[Array[AnyRef]]
        if (display2 == null) display2 = new Array(32)
        display1 = display2((newIndex >> 10) & 31).asInstanceOf[Array[AnyRef]]
        if (display1 == null) display1 = new Array(32)
        display0 = new Array(32)
      } else { // level = 6
        throw new IllegalArgumentException()
      }
      stabilize(newIndex)
    }


    // xor: oldIndex ^ index
    final def XgotoFreshPosClean(oldIndex: Int, newIndex: Int, xor: Int): Unit = { // goto block start pos
      if (xor < (1 << 10)) { // level = 1
        if (depth == 1) { display1 = new Array(32); depth +=1 }
        else { display1 = copyOf(display1) }
        display1((oldIndex >>  5) & 31) = display0
        display0 = new Array(32)
      } else
      if (xor < (1 << 15)) { // level = 2
        if (depth == 2) { display2 = new Array(32); depth+=1}
        else { display2 = copyOf(display2) }
        display1 = copyOf(display1)
        display1((oldIndex >>  5) & 31) = display0
        display2((oldIndex >> 10) & 31) = display1
        display0 = new Array(32)
        display1 = new Array(32)
      } else
      if (xor < (1 << 20)) { // level = 3
        if (depth == 3) { display3 = new Array(32); depth+=1}
        else { display3 = copyOf(display3) }
        display1 = copyOf(display1)
        display2 = copyOf(display2)
        display1((oldIndex >>  5) & 31) = display0
        display2((oldIndex >> 10) & 31) = display1
        display3((oldIndex >> 15) & 31) = display2
        display0 = new Array(32)
        display1 = new Array(32)
        display2 = new Array(32)
      } else
      if (xor < (1 << 25)) { // level = 4
        if (depth == 4) { display4 = new Array(32); depth+=1}
        else { display4 = copyOf(display4) }
        display1 = copyOf(display1)
        display2 = copyOf(display2)
        display3 = copyOf(display3)
        display1((oldIndex >>  5) & 31) = display0
        display2((oldIndex >> 10) & 31) = display1
        display3((oldIndex >> 15) & 31) = display2
        display4((oldIndex >> 20) & 31) = display3
        display0 = new Array(32)
        display1 = new Array(32)
        display2 = new Array(32)
        display3 = new Array(32)
      } else
      if (xor < (1 << 30)) { // level = 5
        if (depth == 5) { display5 = new Array(32); depth+=1 }
        else { display5 = copyOf(display5) }
        display1 = copyOf(display1)
        display2 = copyOf(display2)
        display3 = copyOf(display3)
        display4 = copyOf(display4)
        display1((oldIndex >>  5) & 31) = display0
        display2((oldIndex >> 10) & 31) = display1
        display3((oldIndex >> 15) & 31) = display2
        display4((oldIndex >> 20) & 31) = display3
        display5((oldIndex >> 25) & 31) = display4
        display0 = new Array(32)
        display1 = new Array(32)
        display2 = new Array(32)
        display3 = new Array(32)
        display4 = new Array(32)
      } else { // level = 6
        throw new IllegalArgumentException()
      }
    }

    /// USED IN UPDATE AND APPEND BACK

    // prepare for writing at an existing position

    // xor: oldIndex ^ index
    // precondition: the pointers (and only those) on path oldIndex to display0 are null
    // postconditions:
    // the pointers (and only those) on path newIndex to display0 are null
    // path oldIndex will be updated with previous displayX
    // displayX will be fresh, writable copies of path newIndex


    final def gotoPosClean(oldIndex: Int, newIndex: Int, xor: Int): Unit = {
      if (depth > 1) {
        gotoPos(newIndex, xor)
        stabilize(newIndex)
      }
    }

    final def XgotoPosClean(oldIndex: Int, newIndex: Int, xor: Int): Unit = { // goto pos index
      if (xor < (1 <<  5)) { // level = 0
//        //println("inside same chunk")
        display0 = copyOf(display0)
      } else
      if (xor < (1 << 10)) { // level = 1
//        //println("transfer to chunk at level 1 ("+oldIndex+"->"+newIndex+")")
        display1 = copyOf(display1)
        display1((oldIndex >> 5) & 31) = display0
        display0 = nullSlotAndCopy(display1, (newIndex >>  5) & 31)
      } else
      if (xor < (1 << 15)) { // level = 2
//        //println("transfer to chunk at level 1")
        display1 = copyOf(display1)
        display2 = copyOf(display2)
        display1((oldIndex >>  5) & 31) = display0
        display2((oldIndex >> 10) & 31) = display1
        display1 = nullSlotAndCopy(display2, (newIndex >> 10) & 31).asInstanceOf[Array[AnyRef]]
        display0 = nullSlotAndCopy(display1, (newIndex >>  5) & 31).asInstanceOf[Array[AnyRef]]
      } else
      if (xor < (1 << 20)) { // level = 3
        display1 = copyOf(display1)
        display2 = copyOf(display2)
        display3 = copyOf(display3)
        display1((oldIndex >>  5) & 31) = display0
        display2((oldIndex >> 10) & 31) = display1
        display3((oldIndex >> 15) & 31) = display2
        display2 = nullSlotAndCopy(display3, (newIndex >> 15) & 31).asInstanceOf[Array[AnyRef]]
        display1 = nullSlotAndCopy(display2, (newIndex >> 10) & 31).asInstanceOf[Array[AnyRef]]
        display0 = nullSlotAndCopy(display1, (newIndex >>  5) & 31).asInstanceOf[Array[AnyRef]]
      } else
      if (xor < (1 << 25)) { // level = 4
        display1 = copyOf(display1)
        display2 = copyOf(display2)
        display3 = copyOf(display3)
        display4 = copyOf(display4)
        display1((oldIndex >>  5) & 31) = display0
        display2((oldIndex >> 10) & 31) = display1
        display3((oldIndex >> 15) & 31) = display2
        display4((oldIndex >> 20) & 31) = display3
        display3 = nullSlotAndCopy(display4, (newIndex >> 20) & 31).asInstanceOf[Array[AnyRef]]
        display2 = nullSlotAndCopy(display3, (newIndex >> 15) & 31).asInstanceOf[Array[AnyRef]]
        display1 = nullSlotAndCopy(display2, (newIndex >> 10) & 31).asInstanceOf[Array[AnyRef]]
        display0 = nullSlotAndCopy(display1, (newIndex >>  5) & 31).asInstanceOf[Array[AnyRef]]
      } else
      if (xor < (1 << 30)) { // level = 5
        display1 = copyOf(display1)
        display2 = copyOf(display2)
        display3 = copyOf(display3)
        display4 = copyOf(display4)
        display5 = copyOf(display5)
        display1((oldIndex >>  5) & 31) = display0
        display2((oldIndex >> 10) & 31) = display1
        display3((oldIndex >> 15) & 31) = display2
        display4((oldIndex >> 20) & 31) = display3
        display5((oldIndex >> 25) & 31) = display4
        display4 = nullSlotAndCopy(display5, (newIndex >> 25) & 31).asInstanceOf[Array[AnyRef]]
        display3 = nullSlotAndCopy(display4, (newIndex >> 20) & 31).asInstanceOf[Array[AnyRef]]
        display2 = nullSlotAndCopy(display3, (newIndex >> 15) & 31).asInstanceOf[Array[AnyRef]]
        display1 = nullSlotAndCopy(display2, (newIndex >> 10) & 31).asInstanceOf[Array[AnyRef]]
        display0 = nullSlotAndCopy(display1, (newIndex >>  5) & 31).asInstanceOf[Array[AnyRef]]
      } else { // level = 6
        throw new IllegalArgumentException()
      }
    }


    def debug(): Unit = {
      return
/*
      //println("DISPLAY 5: " + display5 + " ---> " + (if (display5 ne null) display5.map(x=> if (x eq null) "." else x + "->" +x.asInstanceOf[Array[AnyRef]].mkString("")).mkString(" ") else "null"))
      //println("DISPLAY 4: " + display4 + " ---> " + (if (display4 ne null) display4.map(x=> if (x eq null) "." else x + "->" +x.asInstanceOf[Array[AnyRef]].mkString("")).mkString(" ") else "null"))
      //println("DISPLAY 3: " + display3 + " ---> " + (if (display3 ne null) display3.map(x=> if (x eq null) "." else x + "->" +x.asInstanceOf[Array[AnyRef]].mkString("")).mkString(" ") else "null"))
      //println("DISPLAY 2: " + display2 + " ---> " + (if (display2 ne null) display2.map(x=> if (x eq null) "." else x + "->" +x.asInstanceOf[Array[AnyRef]].mkString("")).mkString(" ") else "null"))
      //println("DISPLAY 1: " + display1 + " ---> " + (if (display1 ne null) display1.map(x=> if (x eq null) "." else x + "->" +x.asInstanceOf[Array[AnyRef]].mkString("")).mkString(" ") else "null"))
      //println("DISPLAY 0: " + display0 + " ---> " + (if (display0 ne null) display0.map(x=> if (x eq null) "." else x.toString).mkString(" ") else "null"))
*/
      //println("DISPLAY 5: " + (if (display5 ne null) display5.map(x=> if (x eq null) "." else x.asInstanceOf[Array[AnyRef]].deepMkString("[","","]")).mkString(" ") else "null"))
      //println("DISPLAY 4: " + (if (display4 ne null) display4.map(x=> if (x eq null) "." else x.asInstanceOf[Array[AnyRef]].deepMkString("[","","]")).mkString(" ") else "null"))
      //println("DISPLAY 3: " + (if (display3 ne null) display3.map(x=> if (x eq null) "." else x.asInstanceOf[Array[AnyRef]].deepMkString("[","","]")).mkString(" ") else "null"))
      //println("DISPLAY 2: " + (if (display2 ne null) display2.map(x=> if (x eq null) "." else x.asInstanceOf[Array[AnyRef]].deepMkString("[","","]")).mkString(" ") else "null"))
      //println("DISPLAY 1: " + (if (display1 ne null) display1.map(x=> if (x eq null) "." else x.asInstanceOf[Array[AnyRef]].deepMkString("[","","]")).mkString(" ") else "null"))
      //println("DISPLAY 0: " + (if (display0 ne null) display0.map(x=> if (x eq null) "." else x.toString).mkString(" ") else "null"))
    }


    // make sure there is no aliasing

    // TODO: optimize
    final def stabilize(index: Int) = {
//      debug()
      display0 = copyOf(display0)
      if (depth > 1) {
        display1 = copyOf(display1)
        display1((index >>  5) & 31) = display0
      }
      if (depth > 2) {
        display2 = copyOf(display2)
        display2((index >> 10) & 31) = display1
      }
      if (depth > 3) {
        display3 = copyOf(display3)
        display3((index >> 15) & 31) = display2
      }
      if (depth > 4) {
        display4 = copyOf(display4)
        display4((index >> 20) & 31) = display3
      }
      if (depth > 5) {
        display5 = copyOf(display5)
        display5((index >> 25) & 31) = display4
      }
    }




    // go to specified position

    // xor: oldIndex ^ index
    final def gotoPos(index: Int, xor: Int): Unit = { // goto pos index
      if (xor < (1 << 10)) { // level = 1
        display0 = display1((index >> 5) & 31).asInstanceOf[Array[AnyRef]]
      } else
      if (xor < (1 << 15)) { // level = 2
        display1 = display2((index >> 10) & 31).asInstanceOf[Array[AnyRef]]
        display0 = display1((index >>  5) & 31).asInstanceOf[Array[AnyRef]]
      } else
      if (xor < (1 << 20)) { // level = 3
        display2 = display3((index >> 15) & 31).asInstanceOf[Array[AnyRef]]
        display1 = display2((index >> 10) & 31).asInstanceOf[Array[AnyRef]]
        display0 = display1((index >>  5) & 31).asInstanceOf[Array[AnyRef]]
      } else
      if (xor < (1 << 25)) { // level = 4
        display3 = display4((index >> 20) & 31).asInstanceOf[Array[AnyRef]]
        display2 = display3((index >> 15) & 31).asInstanceOf[Array[AnyRef]]
        display1 = display2((index >> 10) & 31).asInstanceOf[Array[AnyRef]]
        display0 = display1((index >>  5) & 31).asInstanceOf[Array[AnyRef]]
      } else
      if (xor < (1 << 30)) { // level = 5
        display4 = display5((index >> 25) & 31).asInstanceOf[Array[AnyRef]]
        display3 = display4((index >> 20) & 31).asInstanceOf[Array[AnyRef]]
        display2 = display3((index >> 15) & 31).asInstanceOf[Array[AnyRef]]
        display1 = display2((index >> 10) & 31).asInstanceOf[Array[AnyRef]]
        display0 = display1((index >>  5) & 31).asInstanceOf[Array[AnyRef]]
      } else { // level = 6
        throw new IllegalArgumentException()
      }
    }

}

