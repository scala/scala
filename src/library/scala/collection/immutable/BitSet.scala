/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
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

import BitSetOps.{LogWL, updateArray}
import mutable.Builder
import scala.annotation.{implicitNotFound, nowarn}

/** A class for immutable bitsets.
  *  $bitsetinfo
  *  @see [[https://docs.scala-lang.org/overviews/collections-2.13/concrete-immutable-collection-classes.html#immutable-bitsets "Scala's Collection Library overview"]]
  *  section on `Immutable BitSets` for more information.
  *
  *  @define Coll `immutable.BitSet`
  *  @define coll immutable bitset
  */
sealed abstract class BitSet
  extends AbstractSet[Int]
    with SortedSet[Int]
    with SortedSetOps[Int, SortedSet, BitSet]
    with StrictOptimizedSortedSetOps[Int, SortedSet, BitSet]
    with collection.BitSet
    with collection.BitSetOps[BitSet]
    with Serializable {

  override def unsorted: Set[Int] = this

  override protected def fromSpecific(coll: IterableOnce[Int]): BitSet = bitSetFactory.fromSpecific(coll)
  override protected def newSpecificBuilder: Builder[Int, BitSet] = bitSetFactory.newBuilder
  override def empty: BitSet = bitSetFactory.empty

  def bitSetFactory: BitSet.type = BitSet

  protected[collection] def fromBitMaskNoCopy(elems: Array[Long]): BitSet = BitSet.fromBitMaskNoCopy(elems)

  def incl(elem: Int): BitSet = {
    require(elem >= 0, "bitset element must be >= 0")
    if (contains(elem)) this
    else {
      val idx = elem >> LogWL
      updateWord(idx, word(idx) | (1L << elem))
    }
  }

  def excl(elem: Int): BitSet = {
    require(elem >= 0, "bitset element must be >= 0")
    if (contains(elem)) {
      val idx = elem >> LogWL
      updateWord(idx, word(idx) & ~(1L << elem))
    } else this
  }

  /** Update word at index `idx`; enlarge set if `idx` outside range of set.
    */
  protected def updateWord(idx: Int, w: Long): BitSet

  override def map(f: Int => Int): BitSet = strictOptimizedMap(newSpecificBuilder, f)
  override def map[B](f: Int => B)(implicit @implicitNotFound(collection.BitSet.ordMsg) ev: Ordering[B]): SortedSet[B] =
    super[StrictOptimizedSortedSetOps].map(f)

  override def flatMap(f: Int => IterableOnce[Int]): BitSet = strictOptimizedFlatMap(newSpecificBuilder, f)
  override def flatMap[B](f: Int => IterableOnce[B])(implicit @implicitNotFound(collection.BitSet.ordMsg) ev: Ordering[B]): SortedSet[B] =
    super[StrictOptimizedSortedSetOps].flatMap(f)

  override def collect(pf: PartialFunction[Int, Int]): BitSet = strictOptimizedCollect(newSpecificBuilder, pf)
  override def collect[B](pf: scala.PartialFunction[Int, B])(implicit @implicitNotFound(collection.BitSet.ordMsg) ev: Ordering[B]): SortedSet[B] =
    super[StrictOptimizedSortedSetOps].collect(pf)

  // necessary for disambiguation
  override def zip[B](that: scala.IterableOnce[B])(implicit @implicitNotFound(collection.BitSet.zipOrdMsg) ev: Ordering[(Int, B)]): SortedSet[(Int, B)] =
    super.zip(that)

  protected[this] def writeReplace(): AnyRef = new BitSet.SerializationProxy(this)
}

/**
  * $factoryInfo
  * @define Coll `immutable.BitSet`
  * @define coll immutable bitset
  */
@nowarn("cat=deprecation&msg=Implementation classes of BitSet should not be accessed directly")
@SerialVersionUID(3L)
object BitSet extends SpecificIterableFactory[Int, BitSet] {

  def fromSpecific(it: scala.collection.IterableOnce[Int]): BitSet =
    it match {
      case bs: BitSet => bs
      case _          => (newBuilder ++= it).result()
    }

  final val empty: BitSet = new BitSet1(0L)

  def newBuilder: Builder[Int, BitSet] =
    mutable.BitSet.newBuilder.mapResult(bs => fromBitMaskNoCopy(bs.elems))

  private def createSmall(a: Long, b: Long): BitSet = if (b == 0L) new BitSet1(a) else new BitSet2(a, b)

  /** A bitset containing all the bits in an array */
  def fromBitMask(elems: Array[Long]): BitSet = {
    val len = elems.length
    if (len == 0) empty
    else if (len == 1) new BitSet1(elems(0))
    else if (len == 2) createSmall(elems(0), elems(1))
    else {
      val a = java.util.Arrays.copyOf(elems, len)
      new BitSetN(a)
    }
  }

  /** A bitset containing all the bits in an array, wrapping the existing
    *  array without copying.
    */
  def fromBitMaskNoCopy(elems: Array[Long]): BitSet = {
    val len = elems.length
    if (len == 0) empty
    else if (len == 1) new BitSet1(elems(0))
    else if (len == 2) createSmall(elems(0), elems(1))
    else new BitSetN(elems)
  }

  @deprecated("Implementation classes of BitSet should not be accessed directly", "2.13.0")
  class BitSet1(val elems: Long) extends BitSet {
    protected[collection] def nwords = 1
    protected[collection] def word(idx: Int) = if (idx == 0) elems else 0L
    protected[collection] def updateWord(idx: Int, w: Long): BitSet =
      if (idx == 0) new BitSet1(w)
      else if (idx == 1) createSmall(elems, w)
      else this.fromBitMaskNoCopy(updateArray(Array(elems), idx, w))


    override def diff(other: collection.Set[Int]): BitSet = other match {
      case bs: collection.BitSet => bs.nwords match {
        case 0 => this
        case _ =>
          val newElems = elems & ~bs.word(0)
          if (newElems == 0L) this.empty else new BitSet1(newElems)
      }
      case _ => super.diff(other)
    }

    override def filterImpl(pred: Int => Boolean, isFlipped: Boolean): BitSet = {
      val _elems = BitSetOps.computeWordForFilter(pred, isFlipped, elems, 0)
      if (_elems == 0L) this.empty else new BitSet1(_elems)
    }
  }

  @deprecated("Implementation classes of BitSet should not be accessed directly", "2.13.0")
  class BitSet2(val elems0: Long, val elems1: Long) extends BitSet {
    protected[collection] def nwords = 2
    protected[collection] def word(idx: Int) = if (idx == 0) elems0 else if (idx == 1) elems1 else 0L
    protected[collection] def updateWord(idx: Int, w: Long): BitSet =
      if (idx == 0) new BitSet2(w, elems1)
      else if (idx == 1) createSmall(elems0, w)
      else this.fromBitMaskNoCopy(updateArray(Array(elems0, elems1), idx, w))


    override def diff(other: collection.Set[Int]): BitSet = other match {
      case bs: collection.BitSet => bs.nwords match {
        case 0 => this
        case 1 =>
          new BitSet2(elems0 & ~bs.word(0), elems1)
        case _ =>
          val _elems0 = elems0 & ~bs.word(0)
          val _elems1 = elems1 & ~bs.word(1)

          if (_elems1 == 0L) {
            if (_elems0 == 0L) {
              this.empty
            } else {
              new BitSet1(_elems0)
            }
          } else {
            new BitSet2(_elems0, _elems1)
          }
      }
      case _ => super.diff(other)
    }

    override def filterImpl(pred: Int => Boolean, isFlipped: Boolean): BitSet = {
      val _elems0 = BitSetOps.computeWordForFilter(pred, isFlipped, elems0, 0)
      val _elems1 = BitSetOps.computeWordForFilter(pred, isFlipped, elems1, 1)

      if (_elems1 == 0L) {
        if (_elems0 == 0L) {
          this.empty
        }
        else new BitSet1(_elems0)
      }
      else new BitSet2(_elems0, _elems1)
    }
  }

  @deprecated("Implementation classes of BitSet should not be accessed directly", "2.13.0")
  class BitSetN(val elems: Array[Long]) extends BitSet {
    protected[collection] def nwords = elems.length

    protected[collection] def word(idx: Int) = if (idx < nwords) elems(idx) else 0L

    protected[collection] def updateWord(idx: Int, w: Long): BitSet = this.fromBitMaskNoCopy(updateArray(elems, idx, w))

    override def diff(that: collection.Set[Int]): BitSet = that match {
      case bs: collection.BitSet =>
        /*
          * Algorithm:
          *
          * We iterate, word-by-word, backwards from the shortest of the two bitsets (this, or bs) i.e. the one with
          * the fewer words. Two extra concerns for optimization are described below.
          *
          * Array Shrinking:
          * If `this` is not longer than `bs`, then since we must iterate through the full array of words,
          * we can track the new highest index word which is non-zero, at little additional cost. At the end, the new
          * Array[Long] allocated for the returned BitSet will only be of size `maxNonZeroIndex + 1`
          *
          * Tracking Changes:
          * If the two sets are disjoint, then we can return `this`. Therefor, until at least one change is detected,
          * we check each word for if it has changed from its corresponding word in `this`. Once a single change is
          * detected, we stop checking because the cost of the new Array must be paid anyways.
          */

        val bsnwords = bs.nwords
        val thisnwords = nwords
        if (bsnwords >= thisnwords) {
          // here, we may have opportunity to shrink the size of the array
          // so, track the highest index which is non-zero. That ( + 1 ) will be our new array length
          var i = thisnwords - 1
          var currentWord = 0L
          // if there are never any changes, we can return `this` at the end
          var anyChanges = false
          while (i >= 0 && currentWord == 0L) {
            val oldWord = word(i)
            currentWord = oldWord & ~bs.word(i)
            anyChanges ||= currentWord != oldWord
            i -= 1
          }
          i match {
            case -1 =>
              if (anyChanges) {
                if (currentWord == 0) {
                  this.empty
                } else {
                  new BitSet1(currentWord)
                }
              } else {
                this
              }
            case 0 =>
              val oldFirstWord = word(0)
              val firstWord = oldFirstWord & ~bs.word(0)
              anyChanges ||= firstWord != oldFirstWord
              if (anyChanges) {
                new BitSet2(firstWord, currentWord)
              } else {
                this
              }
            case _ =>
              val minimumNonZeroIndex: Int = i + 1
              while (!anyChanges && i >= 0) {
                val oldWord = word(i)
                currentWord = oldWord & ~bs.word(i)
                anyChanges ||= currentWord != oldWord
                i -= 1
              }
              if (anyChanges) {
                val newArray = elems.take(minimumNonZeroIndex + 1)
                newArray(i + 1) = currentWord
                while (i >= 0) {
                  newArray(i) = word(i) & ~bs.word(i)
                  i -= 1
                }
                new BitSetN(newArray)
              } else {
                this
              }
          }
        } else {
          var i = bsnwords - 1
          var anyChanges = false
          var currentWord = 0L
          while (i >= 0 && !anyChanges) {
            val oldWord = word(i)
            currentWord = oldWord & ~bs.word(i)
            anyChanges ||= currentWord != oldWord
            i -= 1
          }
          if (anyChanges) {
            val newElems = elems.clone()
            newElems(i + 1) = currentWord
            while (i >= 0) {
              newElems(i) = word(i) & ~bs.word(i)
              i -= 1
            }
            this.fromBitMaskNoCopy(newElems)
          } else {
            this
          }
        }
      case _ => super.diff(that)
    }


    override def filterImpl(pred: Int => Boolean, isFlipped: Boolean): BitSet = {
      // here, we may have opportunity to shrink the size of the array
      // so, track the highest index which is non-zero. That ( + 1 ) will be our new array length
      var i = nwords - 1
      var currentWord = 0L
      // if there are never any changes, we can return `this` at the end
      var anyChanges = false
      while (i >= 0 && currentWord == 0L) {
        val oldWord = word(i)
        currentWord = BitSetOps.computeWordForFilter(pred, isFlipped, oldWord, i)
        anyChanges ||= currentWord != oldWord
        i -= 1
      }
      i match {
        case -1 =>
          if (anyChanges) {
            if (currentWord == 0) {
              this.empty
            } else {
              new BitSet1(currentWord)
            }
          } else {
            this
          }
        case 0 =>
          val oldFirstWord = word(0)
          val firstWord = BitSetOps.computeWordForFilter(pred, isFlipped, oldFirstWord, 0)
          anyChanges ||= firstWord != oldFirstWord
          if (anyChanges) {
            new BitSet2(firstWord, currentWord)
          } else {
            this
          }
        case _ =>
          val minimumNonZeroIndex: Int = i + 1
          while (!anyChanges && i >= 0) {
            val oldWord = word(i)
            currentWord = BitSetOps.computeWordForFilter(pred, isFlipped, oldWord, i)
            anyChanges ||= currentWord != oldWord
            i -= 1
          }
          if (anyChanges) {
            val newArray = elems.take(minimumNonZeroIndex + 1)
            newArray(i + 1) = currentWord
            while (i >= 0) {
              newArray(i) = BitSetOps.computeWordForFilter(pred, isFlipped, word(i), i)
              i -= 1
            }
            new BitSetN(newArray)
          } else {
            this
          }
      }
    }

    override def toBitMask: Array[Long] = elems.clone()
  }

  @SerialVersionUID(3L)
  private final class SerializationProxy(coll: BitSet) extends scala.collection.BitSet.SerializationProxy(coll) {
    protected[this] def readResolve(): Any = BitSet.fromBitMaskNoCopy(elems)
  }
}
