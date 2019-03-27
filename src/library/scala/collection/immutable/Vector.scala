/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection
package immutable

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.Stepper.EfficientSplit
import scala.collection.generic.DefaultSerializable
import scala.collection.mutable.ReusableBuilder
import scala.runtime.Statics.releaseFence

/** $factoryInfo
  * @define Coll `Vector`
  * @define coll vector
  */
@SerialVersionUID(3L)
object Vector extends StrictOptimizedSeqFactory[Vector] {

  def empty[A]: Vector[A] = NIL

  def from[E](it: collection.IterableOnce[E]): Vector[E] =
    it match {
      case as: ArraySeq[E] if as.length <= 32 && as.unsafeArray.isInstanceOf[Array[AnyRef]] =>
        if (as.isEmpty) NIL
        else {
          val v = new Vector(0, as.length, 0)
          v.display0 = as.unsafeArray.asInstanceOf[Array[AnyRef]]
          v.depth = 1
          releaseFence()
          v
        }
      case v: Vector[E] => v
      case _ =>
        val knownSize = it.knownSize

        if (knownSize == 0) empty[E]
        else if (knownSize > 0 && knownSize <= 32) {
          val display0 = new Array[AnyRef](knownSize)

          var i = 0
          val iterator = it.iterator
          while (iterator.hasNext) {
            display0(i) = iterator.next().asInstanceOf[AnyRef]
            i += 1
          }
          val v = new Vector[E](0, knownSize, 0)
          v.depth = 1
          v.display0 = display0
          releaseFence()
          v
        } else {
          (newBuilder ++= it).result()
        }
    }

  def newBuilder[A]: ReusableBuilder[A, Vector[A]] = new VectorBuilder[A]

  /** Creates a Vector of one element. Not safe for publication, the caller is responsible for `releaseFence` */
  private def single[A](elem: A): Vector[A] = {
    val s = new Vector[A](0, 1, 0)
    s.depth = 1
    s.display0 = Array[AnyRef](elem.asInstanceOf[AnyRef])
    s
  }

  @transient
  private val NIL = new Vector[Nothing](0, 0, 0)

  private val defaultApplyPreferredMaxLength: Int =
    try System.getProperty("scala.collection.immutable.Vector.defaultApplyPreferredMaxLength",
      "1024").toInt
    catch {
      case _: SecurityException => 1024
    }

  // Constants governing concat strategy for performance
  private final val Log2ConcatFaster = 5
  private final val TinyAppendFaster = 2
}

// in principle, most members should be private. however, access privileges must
// be carefully chosen to not prevent method inlining

/** Vector is a general-purpose, immutable data structure.  It provides random access and updates
 *  in effectively constant time, as well as very fast append and prepend.  Because vectors strike
 *  a good balance between fast random selections and fast random functional updates, they are
 *  currently the default implementation of immutable indexed sequences.  It is backed by a little
 *  endian bit-mapped vector trie with a branching factor of 32.  Locality is very good, but not
 *  contiguous, which is good for very large sequences.
 *
 *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-immutable-collection-classes.html#vectors "Scala's Collection Library overview"]]
 *  section on `Vectors` for more information.
 *
 *  @tparam A the element type
 *
 *  @define Coll `Vector`
 *  @define coll vector
 *  @define orderDependent
 *  @define orderDependentFold
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
final class Vector[+A] private[immutable] (private[collection] val startIndex: Int, private[collection] val endIndex: Int, private[immutable] val focus: Int)
  extends AbstractSeq[A]
    with IndexedSeq[A]
    with IndexedSeqOps[A, Vector, Vector[A]]
    with StrictOptimizedSeqOps[A, Vector, Vector[A]]
    with IterableFactoryDefaults[A @uncheckedVariance, Vector]
    with VectorPointer[A @uncheckedVariance]
    with DefaultSerializable { self =>

  override def iterableFactory: SeqFactory[Vector] = Vector

  // Code paths that mutates `dirty` _must_ call `Statics.releaseFence()` before returning from
  // the public method.
  private[immutable] var dirty = false
  // While most JDKs would implicit add this fence because of >= 1 final field, the spec only mandates
  // it if all fields are final, so let's add this in explicitly.
  releaseFence()

  def length: Int = endIndex - startIndex

  private[collection] def initIterator[B >: A](s: VectorIterator[B]): Unit = {
    s.initFrom(this)
    if (dirty) s.stabilize(focus)
    if (s.depth > 1) s.gotoPos(startIndex, startIndex ^ focus)
  }

  override def iterator: Iterator[A] = {
    if(isEmpty)
      Iterator.empty
    else {
      val s = new VectorIterator[A](startIndex, endIndex)
      initIterator(s)
      s
    }
  }

  override def stepper[B >: A, S <: Stepper[_]](implicit shape: StepperShape[B, S]): S with EfficientSplit = {
    import convert.impl._
    var depth = -1
    val displaySource: VectorPointer[A] =
      if (dirty) iterator.asInstanceOf[VectorIterator[A]]
      else this
    val trunk: Array[AnyRef] =
      if      (endIndex <= (1 <<  5)) { depth = 0; displaySource.display0 }
      else if (endIndex <= (1 << 10)) { depth = 1; displaySource.display1.asInstanceOf[Array[AnyRef]] }
      else if (endIndex <= (1 << 15)) { depth = 2; displaySource.display2.asInstanceOf[Array[AnyRef]] }
      else if (endIndex <= (1 << 20)) { depth = 3; displaySource.display3.asInstanceOf[Array[AnyRef]] }
      else if (endIndex <= (1 << 25)) { depth = 4; displaySource.display4.asInstanceOf[Array[AnyRef]] }
      else  /* endIndex <=  1 << 30*/ { depth = 5; displaySource.display5.asInstanceOf[Array[AnyRef]] }
    val s = shape.shape match {
      case StepperShape.IntShape    => new IntVectorStepper   (startIndex, endIndex, depth, trunk)
      case StepperShape.LongShape   => new LongVectorStepper  (startIndex, endIndex, depth, trunk)
      case StepperShape.DoubleShape => new DoubleVectorStepper(startIndex, endIndex, depth, trunk)
      case _         => shape.parUnbox(new AnyVectorStepper[B](startIndex, endIndex, depth, trunk))
    }
    s.asInstanceOf[S with EfficientSplit]
  }

  // Ideally, clients will inline calls to map all the way down, including the iterator/builder methods.
  // In principle, escape analysis could even remove the iterator/builder allocations and do it
  // with local variables exclusively. But we're not quite there yet ...

  @throws[IndexOutOfBoundsException]
  def apply(index: Int): A = {
    val idx = checkRangeConvert(index)
    getElem(idx, idx ^ focus)
  }

  @throws[IndexOutOfBoundsException]
  private def checkRangeConvert(index: Int) = {
    val idx = index + startIndex
    if (index >= 0 && idx < endIndex)
      idx
    else
      throw new IndexOutOfBoundsException(s"$index is out of bounds (min 0, max ${endIndex-1})")
  }
  // requires structure is at pos oldIndex = xor ^ index
  private final def getElem(index: Int, xor: Int): A = {
    if (xor < (1 << 5)) { // level = 0
      (display0
        (index & 31).asInstanceOf[A])
    } else if (xor < (1 << 10)) { // level = 1
      (display1
        ((index >>> 5) & 31)
        (index & 31).asInstanceOf[A])
    } else if (xor < (1 << 15)) { // level = 2
      (display2
        ((index >>> 10) & 31)
        ((index >>> 5) & 31)
        (index & 31).asInstanceOf[A])
    } else if (xor < (1 << 20)) { // level = 3
      (display3
        ((index >>> 15) & 31)
        ((index >>> 10) & 31)
        ((index >>> 5) & 31)
        (index & 31).asInstanceOf[A])
    } else if (xor < (1 << 25)) { // level = 4
      (display4
        ((index >>> 20) & 31)
        ((index >>> 15) & 31)
        ((index >>> 10) & 31)
        ((index >>> 5) & 31)
        (index & 31).asInstanceOf[A])
    } else if (xor < (1 << 30)) { // level = 5
      (display5
        ((index >>> 25) & 31)
        ((index >>> 20) & 31)
        ((index >>> 15) & 31)
        ((index >>> 10) & 31)
        ((index >>> 5) & 31)
        (index & 31).asInstanceOf[A])
    } else { // level = 6
      throw new IllegalArgumentException()
    }
  }

  override def updated[B >: A](index: Int, elem: B): Vector[B] = updateAt(index, elem)

  override def take(n: Int): Vector[A] = {
    if (n <= 0)
      Vector.empty
    else if (startIndex < endIndex - n)
      dropBack0(startIndex + n)
    else
      this
  }

  override def drop(n: Int): Vector[A] = {
    if (n <= 0)
      this
    else if (startIndex < endIndex - n)
      dropFront0(startIndex + n)
    else
      Vector.empty
  }

  override def takeRight(n: Int): Vector[A] = {
    if (n <= 0)
      Vector.empty
    else if (endIndex - n > startIndex)
      dropFront0(endIndex - n)
    else
      this
  }

  override def dropRight(n: Int): Vector[A] = {
    if (n <= 0)
      this
    else if (endIndex - n > startIndex)
      dropBack0(endIndex - n)
    else
      Vector.empty
  }

  override def head: A = {
    if (isEmpty) throw new NoSuchElementException("empty.head")
    apply(0)
  }

  override def tail: Vector[A] = {
    if (isEmpty) throw new UnsupportedOperationException("empty.tail")
    drop(1)
  }

  override def last: A = {
    if (isEmpty) throw new UnsupportedOperationException("empty.last")
    apply(length - 1)
  }

  override def init: Vector[A] = {
    if (isEmpty) throw new UnsupportedOperationException("empty.init")
    dropRight(1)
  }

  override def appendedAll[B >: A](suffix: collection.IterableOnce[B]): Vector[B] =
    suffix match {
      case v: Vector[B] =>
        val thisLength = this.length
        val thatLength = v.length
        if (thisLength == 0) v
        else if (thatLength == 0) this
        else if (thatLength <= Vector.TinyAppendFaster) {
          // Often it's better to append small numbers of elements (or prepend if RHS is a vector)
          var v0: Vector[B] = this
          var i = 0
          while (i < thatLength) {
            v0 = v0.appended(v(i))
            i += 1
          }
          v0
        } else {
          if (thisLength < (thatLength >>> Vector.Log2ConcatFaster)) {
            var v0 = v
            val iter = this.reverseIterator
            while(iter.hasNext) {
              v0 = iter.next() +: v0
            }
            v0
          } else {
            new VectorBuilder[B]().addAll(this).addAll(suffix).result()
          }
        }
      case _ =>
        val thatKnownSize = suffix.knownSize
        if (thatKnownSize == 0) this
        else if (thatKnownSize > 0 && thatKnownSize <= Vector.TinyAppendFaster) {
          var v0: Vector[B] = this
          val iter = suffix.iterator
          while (iter.hasNext) {
            v0 = v0.appended(iter.next())
          }
          v0
        } else {
          val iter = suffix.iterator
          if (iter.hasNext) {
            new VectorBuilder[B]().addAll(this).addAll(suffix).result()
          } else this
        }
    }

  override def prependedAll[B >: A](prefix: collection.IterableOnce[B]): Vector[B] = {
    // Implementation similar to `appendAll`: when of the collections to concatenate (either `this` or `prefix`)
    // has a small number of elements compared to the other, then we add them using `:+` or `+:` in a loop
    import Vector.{Log2ConcatFaster, TinyAppendFaster}
    if (prefix.iterator.isEmpty) this
    else {
      prefix match {
        case prefix: collection.Iterable[B] =>
          prefix.size match {
            case n if n <= TinyAppendFaster || n < (this.size >>> Log2ConcatFaster) =>
              var v: Vector[B] = this
              val it = prefix.toIndexedSeq.reverseIterator
              while (it.hasNext) v = it.next() +: v
              v
            case n if this.size < (n >>> Log2ConcatFaster) && prefix.isInstanceOf[Vector[_]] =>
              var v = prefix.asInstanceOf[Vector[B]]
              val it = this.iterator
              while (it.hasNext) v = v :+ it.next()
              v
            case _ => super.prependedAll(prefix)
          }
        case _ =>
          super.prependedAll(prefix)
      }
    }
  }

  // semi-private api

  private[immutable] def updateAt[B >: A](index: Int, elem: B): Vector[B] = {
    val idx = checkRangeConvert(index)
    val s = new Vector[B](startIndex, endIndex, idx)
    s.initFrom(this)
    s.dirty = dirty
    s.gotoPosWritable(focus, idx, focus ^ idx)  // if dirty commit changes; go to new pos and prepare for writing
    s.display0(idx & 31) = elem.asInstanceOf[AnyRef]
    releaseFence()
    s
  }

  private def gotoPosWritable(oldIndex: Int, newIndex: Int, xor: Int) = if (dirty) {
    gotoPosWritable1(oldIndex, newIndex, xor)
  } else {
    gotoPosWritable0(newIndex, xor)
    dirty = true
  }

  private def gotoFreshPosWritable(oldIndex: Int, newIndex: Int, xor: Int) = if (dirty) {
    gotoFreshPosWritable1(oldIndex, newIndex, xor)
  } else {
    gotoFreshPosWritable0(oldIndex, newIndex, xor)
    dirty = true
  }

  override def prepended[B >: A](value: B): Vector[B] = {
    val thisLength = length
    val result =
      if (depth == 1 && thisLength < 32) {
        val s = new Vector(0, thisLength + 1, 0)
        s.depth = 1
        val newDisplay0 = new Array[AnyRef](thisLength + 1)
        System.arraycopy(display0, startIndex, newDisplay0, 1, thisLength)
        newDisplay0(0) = value.asInstanceOf[AnyRef]
        s.display0 = newDisplay0
        s
      } else if (thisLength > 0) {
        val blockIndex = (startIndex - 1) & ~31
        val lo = (startIndex - 1) & 31

        if (startIndex != blockIndex + 32) {
          val s = new Vector(startIndex - 1, endIndex, blockIndex)
          s.initFrom(this)
          s.dirty = dirty
          s.gotoPosWritable(focus, blockIndex, focus ^ blockIndex)
          s.display0(lo) = value.asInstanceOf[AnyRef]
          s
        } else {

          val freeSpace = (1 << (5 * depth)) - endIndex           // free space at the right given the current tree-structure depth
          val shift = freeSpace & ~((1 << (5 * (depth - 1))) - 1) // number of elements by which we'll shift right (only move at top level)
          val shiftBlocks = freeSpace >>> (5 * (depth - 1))       // number of top-level blocks

          if (shift != 0) {
            // case A: we can shift right on the top level
            if (depth > 1) {
              val newBlockIndex = blockIndex + shift
              val newFocus = focus + shift

              val s = new Vector(startIndex - 1 + shift, endIndex + shift, newBlockIndex)
              s.initFrom(this)
              s.dirty = dirty
              s.shiftTopLevel(0, shiftBlocks) // shift right by n blocks
              s.gotoFreshPosWritable(newFocus, newBlockIndex, newFocus ^ newBlockIndex) // maybe create pos; prepare for writing
              s.display0(lo) = value.asInstanceOf[AnyRef]
              s
            } else {
              val newBlockIndex = blockIndex + 32
              val newFocus = focus

              val s = new Vector(startIndex - 1 + shift, endIndex + shift, newBlockIndex)
              s.initFrom(this)
              s.dirty = dirty
              s.shiftTopLevel(0, shiftBlocks) // shift right by n elements
              s.gotoPosWritable(newFocus, newBlockIndex, newFocus ^ newBlockIndex) // prepare for writing
              s.display0(shift - 1) = value.asInstanceOf[AnyRef]
              s
            }
          } else if (blockIndex < 0) {
            // case B: we need to move the whole structure
            val move = (1 << (5 * (depth + 1))) - (1 << (5 * depth))
            val newBlockIndex = blockIndex + move
            val newFocus = focus + move

            val s = new Vector(startIndex - 1 + move, endIndex + move, newBlockIndex)
            s.initFrom(this)
            s.dirty = dirty
            s.gotoFreshPosWritable(newFocus, newBlockIndex, newFocus ^ newBlockIndex) // could optimize: we know it will create a whole branch
            s.display0(lo) = value.asInstanceOf[AnyRef]
            s
          } else {
            val newBlockIndex = blockIndex
            val newFocus = focus

            val s = new Vector(startIndex - 1, endIndex, newBlockIndex)
            s.initFrom(this)
            s.dirty = dirty
            s.gotoFreshPosWritable(newFocus, newBlockIndex, newFocus ^ newBlockIndex)
            s.display0(lo) = value.asInstanceOf[AnyRef]
            s
          }
        }
      } else Vector.single(value)

    releaseFence()
    result
  }

  override def appended[B >: A](value: B): Vector[B] = {
    val thisLength = length
    val result =
      if (depth == 1 && thisLength < 32) {
        val s = new Vector(0, thisLength + 1, 0)
        s.depth = 1
        val newDisplay0 = new Array[AnyRef](thisLength + 1)
        System.arraycopy(display0, startIndex, newDisplay0, 0, thisLength)
        newDisplay0(thisLength) = value.asInstanceOf[AnyRef]
        s.display0 = newDisplay0
        s
      } else if (thisLength > 0) {
        val blockIndex = endIndex & ~31 // round down to nearest 32
        val lo = endIndex & 31 // remainder of blockIndex / 32

        if (endIndex != blockIndex) {
          val s = new Vector(startIndex, endIndex + 1, blockIndex)
          s.initFrom(this)
          s.dirty = dirty
          s.gotoPosWritable(focus, blockIndex, focus ^ blockIndex)
          s.display0(lo) = value.asInstanceOf[AnyRef]
          s
        } else {
          val shift = startIndex & ~((1 << (5 * (depth - 1))) - 1)
          val shiftBlocks = startIndex >>> (5 * (depth - 1))

          if (shift != 0) {
            if (depth > 1) {
              val newBlockIndex = blockIndex - shift
              val newFocus = focus - shift

              val s = new Vector(startIndex - shift, endIndex + 1 - shift, newBlockIndex)
              s.initFrom(this)
              s.dirty = dirty
              s.shiftTopLevel(shiftBlocks, 0) // shift left by n blocks
              s.gotoFreshPosWritable(newFocus, newBlockIndex, newFocus ^ newBlockIndex)
              s.display0(lo) = value.asInstanceOf[AnyRef]
              s
            } else {
              val newBlockIndex = blockIndex - 32
              val newFocus = focus

              val s = new Vector(startIndex - shift, endIndex + 1 - shift, newBlockIndex)
              s.initFrom(this)
              s.dirty = dirty
              s.shiftTopLevel(shiftBlocks, 0) // shift right by n elements
              s.gotoPosWritable(newFocus, newBlockIndex, newFocus ^ newBlockIndex)

              if (s.display0.length < 32 - shift - 1) {
                val newDisplay0 = new Array[AnyRef](32 - shift - 1)
                s.display0.copyToArray(newDisplay0)
                s.display0 = newDisplay0
              }
              s.display0(32 - shift) = value.asInstanceOf[AnyRef]
              s
            }
          } else {
            val newBlockIndex = blockIndex
            val newFocus = focus

            val s = new Vector(startIndex, endIndex + 1, newBlockIndex)
            s.initFrom(this)
            s.dirty = dirty
            s.gotoFreshPosWritable(newFocus, newBlockIndex, newFocus ^ newBlockIndex)
            s.display0(lo) = value.asInstanceOf[AnyRef]
            s
          }
        }
      } else Vector.single(value)

    releaseFence()
    result
  }


  // low-level implementation (needs cleanup, maybe move to util class)

  private def shiftTopLevel(oldLeft: Int, newLeft: Int) = (depth - 1) match {
    case 0 => display0 = copyRange(display0, oldLeft, newLeft)
    case 1 => display1 = copyRange(display1, oldLeft, newLeft)
    case 2 => display2 = copyRange(display2, oldLeft, newLeft)
    case 3 => display3 = copyRange(display3, oldLeft, newLeft)
    case 4 => display4 = copyRange(display4, oldLeft, newLeft)
    case 5 => display5 = copyRange(display5, oldLeft, newLeft)
  }

  private def zeroLeft(array: Array[AnyRef], index: Int): Unit = {
    var i = 0
    while (i < index) {
      array(i) = null
      i += 1
    }
  }

  private def zeroRight(array: Array[AnyRef], index: Int): Unit = {
    var i = index
    while (i < array.length) {
      array(i) = null
      i += 1
    }
  }

  private def copyLeft[T <: AnyRef](array: Array[T], right: Int): Array[T] = {
    val copy = array.clone()
    java.util.Arrays.fill(copy.asInstanceOf[Array[AnyRef]], right, array.length, null)
    copy
  }
  private def copyRight[T <: AnyRef](array: Array[T], left: Int): Array[T] = {
    val copy = array.clone()
    java.util.Arrays.fill(copy.asInstanceOf[Array[AnyRef]], 0, left, null)
    copy
  }

  // requires structure is at index cutIndex and writable at level 0
  private def cleanLeftEdge(cutIndex: Int) = {
    if        (cutIndex < (1 <<  5)) {
      zeroLeft(display0, cutIndex)
    } else if (cutIndex < (1 << 10)) {
      zeroLeft(display0, cutIndex & 31)
      display1 = copyRight(display1,  cutIndex >>>  5)
    } else if (cutIndex < (1 << 15)) {
      zeroLeft(display0, cutIndex & 31)
      display1 = copyRight(display1, (cutIndex >>>  5) & 31)
      display2 = copyRight(display2,  cutIndex >>> 10)
    } else if (cutIndex < (1 << 20)) {
      zeroLeft(display0, cutIndex & 31)
      display1 = copyRight(display1, (cutIndex >>>  5) & 31)
      display2 = copyRight(display2, (cutIndex >>> 10) & 31)
      display3 = copyRight(display3,  cutIndex >>> 15)
    } else if (cutIndex < (1 << 25)) {
      zeroLeft(display0, cutIndex & 31)
      display1 = copyRight(display1, (cutIndex >>>  5) & 31)
      display2 = copyRight(display2, (cutIndex >>> 10) & 31)
      display3 = copyRight(display3, (cutIndex >>> 15) & 31)
      display4 = copyRight(display4,  cutIndex >>> 20)
    } else if (cutIndex < (1 << 30)) {
      zeroLeft(display0, cutIndex & 31)
      display1 = copyRight(display1, (cutIndex >>>  5) & 31)
      display2 = copyRight(display2, (cutIndex >>> 10) & 31)
      display3 = copyRight(display3, (cutIndex >>> 15) & 31)
      display4 = copyRight(display4, (cutIndex >>> 20) & 31)
      display5 = copyRight(display5,  cutIndex >>> 25)
    } else {
      throw new IllegalArgumentException()
    }
  }

  // requires structure is writable and at index cutIndex
  private def cleanRightEdge(cutIndex: Int) = {
    // we're actually sitting one block left if cutIndex lies on a block boundary
    // this means that we'll end up erasing the whole block!!

    if        (cutIndex <= (1 <<  5)) {
      zeroRight(display0, cutIndex)
    } else if (cutIndex <= (1 << 10)) {
      zeroRight(display0, ((cutIndex - 1) & 31) + 1)
      display1 = copyLeft(display1, cutIndex >>>  5)
    } else if (cutIndex <= (1 << 15)) {
      zeroRight(display0, ((cutIndex - 1) & 31) + 1)
      display1 = copyLeft(display1, (((cutIndex - 1) >>>  5) & 31) + 1)
      display2 = copyLeft(display2, cutIndex >>> 10)
    } else if (cutIndex <= (1 << 20)) {
      zeroRight(display0, ((cutIndex - 1) & 31) + 1)
      display1 = copyLeft(display1, (((cutIndex - 1) >>>  5) & 31) + 1)
      display2 = copyLeft(display2, (((cutIndex - 1) >>> 10) & 31) + 1)
      display3 = copyLeft(display3, cutIndex >>> 15)
    } else if (cutIndex <= (1 << 25)) {
      zeroRight(display0, ((cutIndex - 1) & 31) + 1)
      display1 = copyLeft(display1, (((cutIndex - 1) >>>  5) & 31) + 1)
      display2 = copyLeft(display2, (((cutIndex - 1) >>> 10) & 31) + 1)
      display3 = copyLeft(display3, (((cutIndex - 1) >>> 15) & 31) + 1)
      display4 = copyLeft(display4, cutIndex >>> 20)
    } else if (cutIndex <= (1 << 30)) {
      zeroRight(display0, ((cutIndex - 1) & 31) + 1)
      display1 = copyLeft(display1, (((cutIndex - 1) >>>  5) & 31) + 1)
      display2 = copyLeft(display2, (((cutIndex - 1) >>> 10) & 31) + 1)
      display3 = copyLeft(display3, (((cutIndex - 1) >>> 15) & 31) + 1)
      display4 = copyLeft(display4, (((cutIndex - 1) >>> 20) & 31) + 1)
      display5 = copyLeft(display5, cutIndex >>> 25)
    } else {
      throw new IllegalArgumentException()
    }
  }

  private def requiredDepth(xor: Int) = {
    if      (xor < (1 <<  5)) 1
    else if (xor < (1 << 10)) 2
    else if (xor < (1 << 15)) 3
    else if (xor < (1 << 20)) 4
    else if (xor < (1 << 25)) 5
    else if (xor < (1 << 30)) 6
    else throw new IllegalArgumentException()
  }

  private def dropFront0(cutIndex: Int): Vector[A] = {
    val blockIndex = cutIndex & ~31
    val xor = cutIndex ^ (endIndex - 1)
    val d = requiredDepth(xor)
    val shift = cutIndex & ~((1 << (5 * d)) - 1)

    // need to init with full display iff going to cutIndex requires swapping block at level >= d

    val s = new Vector(cutIndex - shift, endIndex - shift, blockIndex - shift)
    s.initFrom(this)
    s.dirty = dirty
    s.gotoPosWritable(focus, blockIndex, focus ^ blockIndex)
    s.preClean(d)
    s.cleanLeftEdge(cutIndex - shift)
    releaseFence()
    s
  }

  private def dropBack0(cutIndex: Int): Vector[A] = {
    val blockIndex = (cutIndex - 1) & ~31
    val xor = startIndex ^ (cutIndex - 1)
    val d = requiredDepth(xor)
    val shift = startIndex & ~((1 << (5 * d)) - 1)

    val s = new Vector(startIndex - shift, cutIndex - shift, blockIndex - shift)
    s.initFrom(this)
    s.dirty = dirty
    s.gotoPosWritable(focus, blockIndex, focus ^ blockIndex)
    s.preClean(d)
    s.cleanRightEdge(cutIndex - shift)
    releaseFence()
    s
  }
  override protected def applyPreferredMaxLength: Int = Vector.defaultApplyPreferredMaxLength

  override def equals(o: Any): Boolean = o match {
    case that: Vector[_] =>
      if (this eq that) true
      else if (this.length != that.length) false
      else if ( //
        this.startIndex == that.startIndex && //
          this.endIndex == that.endIndex && //
          (this.display0 eq that.display0) && //
          (this.display1 eq that.display1) && //
          (this.display2 eq that.display2) && //
          (this.display3 eq that.display3) && //
          (this.display4 eq that.display4) && //
          (this.display5 eq that.display5) //
      ) true
      else super.equals(o)
    case _ => super.equals(o)
  }

  override def copyToArray[B >: A](xs: Array[B], start: Int, len: Int): Int = iterator.copyToArray(xs, start, len)

  override def toVector: Vector[A] = this

  override protected[this] def className = "Vector"
}

//TODO: When making this class private, make it final as well.
@deprecated("This class is not intended for public consumption and will be made private in the future.","2.13.0")
class VectorIterator[+A](_startIndex: Int, private[this] var endIndex: Int)
  extends AbstractIterator[A]
    with VectorPointer[A @uncheckedVariance] {

  private[this] final var blockIndex: Int = _startIndex & ~31
  private[this] final var lo: Int = _startIndex & 31
  private[this] final var endLo = Math.min(endIndex - blockIndex, 32)

  override def hasNext: Boolean = _hasNext

  private[this] final var _hasNext = blockIndex + lo < endIndex

  private[this] def advanceToNextBlockIfNecessary(): Unit = {
    if (lo == endLo) {
      if (blockIndex + lo < endIndex) {
        val newBlockIndex = blockIndex + 32
        gotoNextBlockStart(newBlockIndex, blockIndex ^ newBlockIndex)

        blockIndex = newBlockIndex
        endLo = Math.min(endIndex - blockIndex, 32)
        lo = 0
      } else {
        _hasNext = false
      }
    }
  }

  override def take(n: Int): Iterator[A] = {
    if(n <= 0) {
      _hasNext = false
      endIndex = 0
    } else {
      val dropR = remainingElementCount - n
      if(dropR > 0) {
        endIndex -= dropR
        endLo = Math.min(endIndex - blockIndex, 32)
        _hasNext = blockIndex + lo < endIndex
      }
    }
    this
  }

  override def drop(n: Int): Iterator[A] = {
    if (n > 0) {
      val longLo = lo.toLong + n
      if (blockIndex + longLo < endIndex) {
        // We only need to adjust the block if we are outside the current block
        // We know that we are within the collection as < endIndex
        lo = longLo.toInt
        if (lo >= 32) {
          blockIndex = (blockIndex + lo) & ~31
          gotoNewBlockStart(blockIndex, depth)

          endLo = Math.min(endIndex - blockIndex, 32)
          lo = lo & 31
        }
      } else {
        _hasNext = false
        endIndex = 0
      }
    }
    this
  }

  override def slice(from: Int, until: Int): Iterator[A] = {
    val _until =
      if(from > 0) {
        drop(from)
        until - from
      } else until
    take(_until)
  }

  override def next(): A = {
    if (!_hasNext) throw new NoSuchElementException("reached iterator end")
    val res = display0(lo).asInstanceOf[A]
    lo += 1
    advanceToNextBlockIfNecessary()
    res
  }

  override def copyToArray[B >: A](xs: Array[B], start: Int, len: Int): Int = {
    val xsLen = xs.length
    val totalToBeCopied = IterableOnce.elemsToCopyToArray(remainingElementCount, xsLen, start, len)
    var totalCopied = 0
    while (hasNext && totalCopied < totalToBeCopied) {
      val _start = start + totalCopied
      val toBeCopied = IterableOnce.elemsToCopyToArray(endLo - lo, xsLen, _start, len - totalCopied)
      Array.copy(display0, lo, xs, _start, toBeCopied)
      totalCopied += toBeCopied
      lo += toBeCopied
      advanceToNextBlockIfNecessary()
    }
    totalCopied
  }

  private[collection] def remainingElementCount: Int = (endIndex - (blockIndex + lo)) max 0

  override def knownSize: Int = remainingElementCount

  /** Creates a new vector which consists of elements remaining in this iterator.
   *  Such a vector can then be split into several vectors using methods like `take` and `drop`.
   */
  private[collection] def remainingVector: Vector[A] = {
    if(!_hasNext) Vector.empty
    else {
      val v = new Vector(blockIndex + lo, endIndex, blockIndex + lo)
      v.initFrom(this)
      v
    }
  }
}

/** A class to build instances of `Vector`.  This builder is reusable. */
final class VectorBuilder[A]() extends ReusableBuilder[A, Vector[A]] with VectorPointer[A @uncheckedVariance] {

  // possible alternative: start with display0 = null, blockIndex = -32, lo = 32
  // to avoid allocating initial array if the result will be empty anyways

  display0 = new Array[AnyRef](32)
  depth = 1

  /** The index within the final Vector of `this.display0(0)` */
  private[this] var blockIndex = 0
  /** The index within `this.display0` which is the next available index to write to.
    * This value may be equal to display0.length, in which case before writing, a new block
    * should be created (see advanceToNextBlockIfNecessary)*/
  private[this] var lo = 0
  /** Indicates an offset of the final vector from the actual underlying array elements. This is
    * used for example in `drop(1)` where instead of copying the entire Vector, only the startIndex is changed.
    *
    * This is present in the Builder because we may be able to share structure with a Vector that is `addAll`'d to this.
    * In which case we must track that Vector's startIndex offset.
    * */
  private[this] var startIndex = 0

  def size: Int = (blockIndex & ~31) + lo - startIndex
  def isEmpty: Boolean = size == 0
  def nonEmpty: Boolean = size != 0

  override def knownSize: Int = size

  private[this] def advanceToNextBlockIfNecessary(): Unit = {
    if (lo >= display0.length) {
      val newBlockIndex = blockIndex + 32
      gotoNextBlockStartWritable(newBlockIndex, blockIndex ^ newBlockIndex)
      blockIndex = newBlockIndex
      lo = 0
    }
  }

  def addOne(elem: A): this.type = {
    advanceToNextBlockIfNecessary()
    display0(lo) = elem.asInstanceOf[AnyRef]
    lo += 1
    this
  }

  override def addAll(xs: IterableOnce[A]): this.type = {

    xs match {
      case v: Vector[A] if this.isEmpty && v.length >= 32 =>
        depth = v.depth
        blockIndex = (v.endIndex - 1) & ~31
        lo = v.endIndex - blockIndex
        startIndex = v.startIndex

        /** `initFrom` will overwrite display0. Keep reference to it so we can reuse the array.*/
        val initialDisplay0 = display0
        initFrom(v)
        stabilize(v.focus)
        gotoPosWritable1(v.focus, blockIndex, v.focus ^ blockIndex, initialDisplay0)

        depth match {
          case 2 =>
            display1((blockIndex >>> 5) & 31) = display0
          case 3 =>
            display1((blockIndex >>> 5) & 31) = display0
            display2((blockIndex >>> 10) & 31) = display1
          case 4 =>
            display1((blockIndex >>> 5) & 31) = display0
            display2((blockIndex >>> 10) & 31) = display1
            display3((blockIndex >>> 15) & 31) = display2
          case 5 =>
            display1((blockIndex >>> 5) & 31) = display0
            display2((blockIndex >>> 10) & 31) = display1
            display3((blockIndex >>> 15) & 31) = display2
            display4((blockIndex >>> 20) & 31) = display3
          case 6 =>
            display1((blockIndex >>> 5) & 31) = display0
            display2((blockIndex >>> 10) & 31) = display1
            display3((blockIndex >>> 15) & 31) = display2
            display4((blockIndex >>> 20) & 31) = display3
            display5((blockIndex >>> 25) & 31) = display4
          case _ => ()
        }
      case _ =>
        val it = (xs.iterator : Iterator[A]).asInstanceOf[Iterator[AnyRef]]
        while (it.hasNext) {
          advanceToNextBlockIfNecessary()
          lo += it.copyToArray(xs = display0, start = lo, len = display0.length - lo)
        }
    }
    this
  }

  def result(): Vector[A] = {
    val size = this.size
    if (size == 0)
      return Vector.empty
    val s = new Vector[A](startIndex, blockIndex + lo, 0) // should focus front or back?
    s.initFrom(this)
    if (depth > 1) s.gotoPos(startIndex, blockIndex + lo - 1) // we're currently focused to size - 1, not size!
    releaseFence()
    s
  }

  def clear(): Unit = {
    preClean(1)
    display0 = new Array[AnyRef](32)
    blockIndex = 0
    lo = 0
    startIndex = 0
  }
}

private[immutable] trait VectorPointer[T] {
    private[immutable] var depth:    Int = _
    private[immutable] var display0: Array[AnyRef] = _
    private[immutable] var display1: Array[Array[AnyRef]] = _
    private[immutable] var display2: Array[Array[Array[AnyRef]]] = _
    private[immutable] var display3: Array[Array[Array[Array[AnyRef]]]] = _
    private[immutable] var display4: Array[Array[Array[Array[Array[AnyRef]]]]] = _
    private[immutable] var display5: Array[Array[Array[Array[Array[Array[AnyRef]]]]]] = _

    protected def preClean(depth: Int): Unit = {
      this.depth = depth
      (depth - 1) match {
        case 0 =>
          display1 = null
          display2 = null
          display3 = null
          display4 = null
          display5 = null
        case 1 =>
          display2 = null
          display3 = null
          display4 = null
          display5 = null
        case 2 =>
          display3 = null
          display4 = null
          display5 = null
        case 3 =>
          display4 = null
          display5 = null
        case 4 =>
          display5 = null
        case 5 =>
      }
    }


  // used
    private[immutable] final def initFrom[U](that: VectorPointer[U]): Unit = initFrom(that, that.depth)

    private[immutable] final def initFrom[U](that: VectorPointer[U], depth: Int) = {
      this.depth = depth
      (depth - 1) match {
        case -1 =>
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

    // go to specific position
    // requires structure is at pos oldIndex = xor ^ index,
    // ensures structure is at pos index
    private[immutable] final def gotoPos(index: Int, xor: Int): Unit = {
      if        (xor < (1 <<  5)) { // level = 0
        // we're already at the block start pos
      } else if (xor < (1 << 10)) { // level = 1
        display0 = display1((index >>>  5) & 31)
      } else if (xor < (1 << 15)) { // level = 2
        display1 = display2((index >>> 10) & 31)
        display0 = display1((index >>>  5) & 31)
      } else if (xor < (1 << 20)) { // level = 3
        display2 = display3((index >>> 15) & 31)
        display1 = display2((index >>> 10) & 31)
        display0 = display1((index >>>  5) & 31)
      } else if (xor < (1 << 25)) { // level = 4
        display3 = display4((index >>> 20) & 31)
        display2 = display3((index >>> 15) & 31)
        display1 = display2((index >>> 10) & 31)
        display0 = display1((index >>>  5) & 31)
      } else if (xor < (1 << 30)) { // level = 5
        display4 = display5((index >>> 25) & 31)
        display3 = display4((index >>> 20) & 31)
        display2 = display3((index >>> 15) & 31)
        display1 = display2((index >>> 10) & 31)
        display0 = display1((index >>>  5) & 31)
      } else {                      // level = 6
        throw new IllegalArgumentException()
      }
    }

    // USED BY ITERATOR

    // xor: oldIndex ^ index
    private[immutable] final def gotoNextBlockStart(index: Int, xor: Int): Unit = { // goto block start pos
      if        (xor < (1 << 10)) { // level = 1
        display0 = display1((index >>>  5) & 31)
      } else if (xor < (1 << 15)) { // level = 2
        display1 = display2((index >>> 10) & 31)
        display0 = display1(0)
      } else if (xor < (1 << 20)) { // level = 3
        display2 = display3((index >>> 15) & 31)
        display1 = display2(0)
        display0 = display1(0)
      } else if (xor < (1 << 25)) { // level = 4
        display3 = display4((index >>> 20) & 31)
        display2 = display3(0)
        display1 = display2(0)
        display0 = display1(0)
      } else if (xor < (1 << 30)) { // level = 5
        display4 = display5((index >>> 25) & 31)
        display3 = display4(0)
        display2 = display3(0)
        display1 = display2(0)
        display0 = display1(0)
      } else {                      // level = 6
        throw new IllegalArgumentException()
      }
    }
  private[immutable] final def gotoNewBlockStart(index: Int, depth: Int): Unit = {
    if (depth > 5) display4 = display5((index >>> 25) & 31)
    if (depth > 4) display3 = display4((index >>> 20) & 31)
    if (depth > 3) display2 = display3((index >>> 15) & 31)
    if (depth > 2) display1 = display2((index >>> 10) & 31)
    if (depth > 1) display0 = display1((index >>> 5) & 31)
  }

    // USED BY BUILDER

    // xor: oldIndex ^ index
    private[immutable] final def gotoNextBlockStartWritable(index: Int, xor: Int): Unit = { // goto block start pos
      if        (xor < (1 << 10)) { // level = 1
        if (depth == 1) { display1 = new Array(32); display1(0) = display0; depth += 1 }
        display0 = new Array(32)
        display1((index >>>   5) & 31) = display0
      } else if (xor < (1 << 15)) { // level = 2
        if (depth == 2) { display2 = new Array(32); display2(0) = display1; depth += 1 }
        display0 = new Array(32)
        display1 = new Array(32)
        display1((index >>>   5) & 31) = display0
        display2((index >>>  10) & 31) = display1
      } else if (xor < (1 << 20)) { // level = 3
        if (depth == 3) { display3 = new Array(32); display3(0) = display2; depth += 1 }
        display0 = new Array(32)
        display1 = new Array(32)
        display2 = new Array(32)
        display1((index >>>   5) & 31) = display0
        display2((index >>>  10) & 31) = display1
        display3((index >>>  15) & 31) = display2
      } else if (xor < (1 << 25)) { // level = 4
        if (depth == 4) { display4 = new Array(32); display4(0) = display3; depth += 1 }
        display0 = new Array(32)
        display1 = new Array(32)
        display2 = new Array(32)
        display3 = new Array(32)
        display1((index >>>   5) & 31) = display0
        display2((index >>>  10) & 31) = display1
        display3((index >>>  15) & 31) = display2
        display4((index >>>  20) & 31) = display3
      } else if (xor < (1 << 30)) { // level = 5
        if (depth == 5) { display5 = new Array(32); display5(0) = display4; depth += 1 }
        display0 = new Array(32)
        display1 = new Array(32)
        display2 = new Array(32)
        display3 = new Array(32)
        display4 = new Array(32)
        display1((index >>>   5) & 31) = display0
        display2((index >>>  10) & 31) = display1
        display3((index >>>  15) & 31) = display2
        display4((index >>>  20) & 31) = display3
        display5((index >>>  25) & 31) = display4
      } else {                      // level = 6
        throw new IllegalArgumentException()
      }
    }

    // STUFF BELOW USED BY APPEND / UPDATE

  /** Sets array(index) to null and returns an array with same contents as what was previously at array(index)
    *
    * If `destination` array is not null, original contents of array(index) will be copied to it, and it will be returned.
    * Otherwise array(index).clone() is returned
    */
    private[immutable] final def nullSlotAndCopy[T <: AnyRef](array: Array[Array[T]], index: Int, destination: Array[T] = null): Array[T] = {
      val x = array(index)
      array(index) = null
      if (destination == null) x.clone()
      else {
        x.copyToArray(destination, 0)
        destination
      }
    }

    // make sure there is no aliasing
    // requires structure is at pos index
    // ensures structure is clean and at pos index and writable at all levels except 0

    private[immutable] final def stabilize(index: Int) = (depth - 1) match {
      case 5 =>
        display5 = display5.clone()
        display4 = display4.clone()
        display3 = display3.clone()
        display2 = display2.clone()
        display1 = display1.clone()
        display5((index >>> 25) & 31) = display4
        display4((index >>> 20) & 31) = display3
        display3((index >>> 15) & 31) = display2
        display2((index >>> 10) & 31) = display1
        display1((index >>>  5) & 31) = display0
      case 4 =>
        display4 = display4.clone()
        display3 = display3.clone()
        display2 = display2.clone()
        display1 = display1.clone()
        display4((index >>> 20) & 31) = display3
        display3((index >>> 15) & 31) = display2
        display2((index >>> 10) & 31) = display1
        display1((index >>>  5) & 31) = display0
      case 3 =>
        display3 = display3.clone()
        display2 = display2.clone()
        display1 = display1.clone()
        display3((index >>> 15) & 31) = display2
        display2((index >>> 10) & 31) = display1
        display1((index >>>  5) & 31) = display0
      case 2 =>
        display2 = display2.clone()
        display1 = display1.clone()
        display2((index >>> 10) & 31) = display1
        display1((index >>>  5) & 31) = display0
      case 1 =>
        display1 = display1.clone()
        display1((index >>>  5) & 31) = display0
      case 0 =>
    }


    /// USED IN UPDATE AND APPEND BACK

    // prepare for writing at an existing position

    // requires structure is clean and at pos oldIndex = xor ^ newIndex,
    // ensures structure is dirty and at pos newIndex and writable at level 0
    private[immutable] final def gotoPosWritable0(newIndex: Int, xor: Int): Unit = (depth - 1) match {
      case 5 =>
        display5 = display5.clone()
        display4 = nullSlotAndCopy(display5, (newIndex >>> 25) & 31)
        display3 = nullSlotAndCopy(display4, (newIndex >>> 20) & 31)
        display2 = nullSlotAndCopy(display3, (newIndex >>> 15) & 31)
        display1 = nullSlotAndCopy(display2, (newIndex >>> 10) & 31)
        display0 = nullSlotAndCopy(display1, (newIndex >>>  5) & 31)
      case 4 =>
        display4 = display4.clone()
        display3 = nullSlotAndCopy(display4, (newIndex >>> 20) & 31)
        display2 = nullSlotAndCopy(display3, (newIndex >>> 15) & 31)
        display1 = nullSlotAndCopy(display2, (newIndex >>> 10) & 31)
        display0 = nullSlotAndCopy(display1, (newIndex >>>  5) & 31)
      case 3 =>
        display3 = display3.clone()
        display2 = nullSlotAndCopy(display3, (newIndex >>> 15) & 31)
        display1 = nullSlotAndCopy(display2, (newIndex >>> 10) & 31)
        display0 = nullSlotAndCopy(display1, (newIndex >>>  5) & 31)
      case 2 =>
        display2 = display2.clone()
        display1 = nullSlotAndCopy(display2, (newIndex >>> 10) & 31)
        display0 = nullSlotAndCopy(display1, (newIndex >>>  5) & 31)
      case 1 =>
        display1 = display1.clone()
        display0 = nullSlotAndCopy(display1, (newIndex >>>  5) & 31)
      case 0 =>
        display0 = display0.clone()
    }

    // requires structure is dirty and at pos oldIndex,
    // ensures structure is dirty and at pos newIndex and writable at level 0
    private[immutable] final def gotoPosWritable1(oldIndex: Int, newIndex: Int, xor: Int, reuseDisplay0: Array[AnyRef] = null): Unit = {
      if        (xor < (1 <<  5)) { // level = 0
        display0 = display0.clone()
      } else if (xor < (1 << 10)) { // level = 1
        display1 = display1.clone()
        display1((oldIndex >>>  5) & 31) = display0
        display0 = nullSlotAndCopy(display1, (newIndex >>>  5) & 31)
      } else if (xor < (1 << 15)) { // level = 2
        display1 = display1.clone()
        display2 = display2.clone()
        display1((oldIndex >>>  5) & 31) = display0
        display2((oldIndex >>> 10) & 31) = display1
        display1 = nullSlotAndCopy(display2, (newIndex >>> 10) & 31)
        display0 = nullSlotAndCopy(display1, (newIndex >>>  5) & 31, reuseDisplay0)
      } else if (xor < (1 << 20)) { // level = 3
        display1 = display1.clone()
        display2 = display2.clone()
        display3 = display3.clone()
        display1((oldIndex >>>  5) & 31) = display0
        display2((oldIndex >>> 10) & 31) = display1
        display3((oldIndex >>> 15) & 31) = display2
        display2 = nullSlotAndCopy(display3, (newIndex >>> 15) & 31)
        display1 = nullSlotAndCopy(display2, (newIndex >>> 10) & 31)
        display0 = nullSlotAndCopy(display1, (newIndex >>>  5) & 31, reuseDisplay0)
      } else if (xor < (1 << 25)) { // level = 4
        display1 = display1.clone()
        display2 = display2.clone()
        display3 = display3.clone()
        display4 = display4.clone()
        display1((oldIndex >>>  5) & 31) = display0
        display2((oldIndex >>> 10) & 31) = display1
        display3((oldIndex >>> 15) & 31) = display2
        display4((oldIndex >>> 20) & 31) = display3
        display3 = nullSlotAndCopy(display4, (newIndex >>> 20) & 31)
        display2 = nullSlotAndCopy(display3, (newIndex >>> 15) & 31)
        display1 = nullSlotAndCopy(display2, (newIndex >>> 10) & 31)
        display0 = nullSlotAndCopy(display1, (newIndex >>>  5) & 31, reuseDisplay0)
      } else if (xor < (1 << 30)) { // level = 5
        display1 = display1.clone()
        display2 = display2.clone()
        display3 = display3.clone()
        display4 = display4.clone()
        display5 = display5.clone()
        display1((oldIndex >>>  5) & 31) = display0
        display2((oldIndex >>> 10) & 31) = display1
        display3((oldIndex >>> 15) & 31) = display2
        display4((oldIndex >>> 20) & 31) = display3
        display5((oldIndex >>> 25) & 31) = display4
        display4 = nullSlotAndCopy(display5, (newIndex >>> 25) & 31)
        display3 = nullSlotAndCopy(display4, (newIndex >>> 20) & 31)
        display2 = nullSlotAndCopy(display3, (newIndex >>> 15) & 31)
        display1 = nullSlotAndCopy(display2, (newIndex >>> 10) & 31)
        display0 = nullSlotAndCopy(display1, (newIndex >>>  5) & 31, reuseDisplay0)
      } else {                      // level = 6
        throw new IllegalArgumentException()
      }
    }


    // USED IN DROP

    private[immutable] final def copyRange[T <: AnyRef](array: Array[T], oldLeft: Int, newLeft: Int) = {
      val elems = java.lang.reflect.Array.newInstance(array.getClass.getComponentType, 32).asInstanceOf[Array[T]]
      java.lang.System.arraycopy(array, oldLeft, elems, newLeft, 32 - Math.max(newLeft, oldLeft))
      elems
    }


    // USED IN APPEND
    // create a new block at the bottom level (and possibly nodes on its path) and prepares for writing

    // requires structure is clean and at pos oldIndex,
    // ensures structure is dirty and at pos newIndex and writable at level 0
    private[immutable] final def gotoFreshPosWritable0(oldIndex: Int, newIndex: Int, xor: Int): Unit = { // goto block start pos
      if        (xor < (1 <<  5)) { // level = 0
        // we're already at the block start
      } else if (xor < (1 << 10)) { // level = 1
        if (depth == 1) {
          display1 = new Array(32)
          display1((oldIndex >>>  5) & 31) = display0
          depth += 1
        }
        display0 = new Array(32)
      } else if (xor < (1 << 15)) { // level = 2
        if (depth == 2) {
          display2 = new Array(32)
          display2((oldIndex >>> 10) & 31) = display1
          depth += 1
        }
        display1 = display2((newIndex >>> 10) & 31)
        if (display1 == null) display1 = new Array(32)
        display0 = new Array(32)
      } else if (xor < (1 << 20)) { // level = 3
        if (depth == 3) {
          display3 = new Array(32)
          display3((oldIndex >>> 15) & 31) = display2
          depth += 1
        }
        display2 = display3((newIndex >>> 15) & 31)
        if (display2 == null) display2 = new Array(32)
        display1 = display2((newIndex >>> 10) & 31)
        if (display1 == null) display1 = new Array(32)
        display0 = new Array(32)
      } else if (xor < (1 << 25)) { // level = 4
        if (depth == 4) {
          display4 = new Array(32)
          display4((oldIndex >>> 20) & 31) = display3
          depth += 1
        }
        display3 = display4((newIndex >>> 20) & 31)
        if (display3 == null) display3 = new Array(32)
        display2 = display3((newIndex >>> 15) & 31)
        if (display2 == null) display2 = new Array(32)
        display1 = display2((newIndex >>> 10) & 31)
        if (display1 == null) display1 = new Array(32)
        display0 = new Array(32)
      } else if (xor < (1 << 30)) { // level = 5
        if (depth == 5) {
          display5 = new Array(32)
          display5((oldIndex >>> 25) & 31) = display4
          depth += 1
        }
        display4 = display5((newIndex >>> 25) & 31)
        if (display4 == null) display4 = new Array(32)
        display3 = display4((newIndex >>> 20) & 31)
        if (display3 == null) display3 = new Array(32)
        display2 = display3((newIndex >>> 15) & 31)
        if (display2 == null) display2 = new Array(32)
        display1 = display2((newIndex >>> 10) & 31)
        if (display1 == null) display1 = new Array(32)
        display0 = new Array(32)
      } else {                      // level = 6
        throw new IllegalArgumentException()
      }
    }

    // requires structure is dirty and at pos oldIndex,
    // ensures structure is dirty and at pos newIndex and writable at level 0
    private[immutable] final def gotoFreshPosWritable1(oldIndex: Int, newIndex: Int, xor: Int): Unit = {
      stabilize(oldIndex)
      gotoFreshPosWritable0(oldIndex, newIndex, xor)
    }
}
