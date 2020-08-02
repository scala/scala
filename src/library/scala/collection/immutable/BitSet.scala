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

import generic._
import BitSetLike.{LogWL, updateArray}
import mutable.Builder
import scala.collection.BitMask._
import scala.runtime.AbstractFunction1

/** A class for immutable bitsets.
 * $bitsetinfo
 *
 * @see [[http://docs.scala-lang.org/overviews/collections/concrete-immutable-collection-classes.html#immutable-bitsets"Scala's Collection Library overview"]]
 *      section on `Immutable BitSets` for more information.
 * @define Coll `immutable.BitSet`
 * @define coll immutable bitset
 */
@SerialVersionUID(1611436763290191562L)
abstract class BitSet extends scala.collection.AbstractSet[Int]
  with SortedSet[Int]
  with scala.collection.BitSet
  with BitSetLike[BitSet]
  with Serializable {
  override def empty = BitSet.empty

  override protected[this] def copyOrSelf: BitSet = this

  protected def fromBitMaskNoCopy(elems: Array[Long]): BitSet = BitSet.fromBitMaskNoCopy(elems)

  /** Update word at index `idx`; enlarge set if `idx` outside range of set.
   */
  protected def updateWord(idx: Int, w: Long): BitSet

  override final protected[scala] def withUpdateWord(wordIndex: Int, wordValue: Long): BitSet =
    updateWord(wordIndex, wordValue)
  /** Adds element to bitset, returning a new set.
   */
  def +(elem: Int): BitSet = {
    require(elem >= 0, "bitset element must be >= 0")
    val idx      = elem >> LogWL
    val bit      = 1L << elem
    val thisWord = word(idx)
    if ((thisWord & bit) != 0L) this
    else updateWord(idx, thisWord | bit)
  }

  /** Removes element from bitset, returning a new set
   */
  def -(elem: Int): BitSet = {
    if (elem < 0) this
    else {
      val idx      = elem >> LogWL
      val bit      = 1L << elem
      val thisWord = word(idx)
      if ((thisWord & bit) == 0L) this
      else updateWord(idx, thisWord ^ bit)
    }
  }

  @inline private def createFromAddResult(word0: Long, word1: Long, bitMask: Array[Long]): BitSet = {
    if (bitMask ne null) {
      fromBitMaskNoCopy(bitMask)
    } else if ((word0 == word(0)) && (word1 == word(1))) BitSet.this
           else BitSet.createSmall(word0, word1)
  }

  @inline def readForAdd(elemWordIndex: Int, word0: Long, word1: Long, bitMask: Array[Long]): Long = {
    if (bitMask ne null) bitMask(elemWordIndex)
    else if (elemWordIndex == 0) word0
         else if (elemWordIndex == 1) word1
              else word(elemWordIndex)

  }
  @inline private def createAdderBitMask(highestWordInAddedData: Int, word0: Long, word1: Long): Array[Long] = {
    val lws     = lastWordSet
    val bitMask = newArray(1 + Math.max(lws, highestWordInAddedData), 2, lws + 1)
    bitMask(0) = word0
    bitMask(1) = word1
    bitMask
  }

  /**
   * Add all the elements, returning `this` if unchanged, or a new BitSet otherwise
   * The general flow is to hope/assume that there is going to be not change, and optimise for that path
   * If there is a change, optimise for the case were the result will fit into 2 words (to match the specialisations),
   * and if it does we can avoid the extra allocation of the array
   * If we do have to allocate the array, then pre-determine the size (with the cost of an additional scan)
   * otherwise it is unlikely that the array will be sized appropriately, and we will have to perform another
   * allocation and scan
   *
   * Optimisations exist for adding a LinearSeq, and in BitSetN, which build on this basic flow
   */
  override protected[scala] def addAllTraversable(toAdd: collection.Traversable[Int]): BitSet = {
    object immutableAdder extends AbstractFunction1[Int, Unit] {
      def addAll(elems: TraversableOnce[Int]): BitSet = {
        elems.foreach(this)
        createFromAddResult(word0, word1, bitMask)
      }
      // if bitMask is set then the values in there are the master
      // otherwise the values in word0/1 are the master values for the lower 2 words
      var bitMask: Array[Long] = null

      var word0: Long = word(0)
      var word1: Long = word(1)
      override def apply(elem: Int): Unit = {
        require(elem >= 0, "bitset element must be >= 0")
        val elemWordIndex = elem >> LogWL
        val elemBit       = 1L << elem
        var elemWord      = readForAdd(elemWordIndex, word0, word1, bitMask)
        if ((elemWord & elemBit) == 0L) {
          elemWord |= elemBit
          if (elemWordIndex > 1 && (bitMask eq null))
            bitMask = createAdderBitMask(BitMask.wordCapacity(toAdd), word0, word1)
          if (bitMask ne null) {
            bitMask(elemWordIndex) = elemWord
          } else if (elemWordIndex == 0) word0 = elemWord
                 else word1 = elemWord
        }
      }
    }
    immutableAdder.addAll(toAdd)
  }

  /**
   * builds on the basic algorithm of addAllTraversable with the follwoing optimisations
   */

  override protected[scala] def addAllLinearSeq(toAdd: collection.LinearSeq[Int]): BitSet = {
    var remaining = toAdd

    // if bitMask is set then the values in there are the master
    // otherwise the values in word0/1 are the master values for the lower 2 words
    var bitMask: Array[Long] = null
    var word0  : Long        = word(0)
    var word1  : Long        = word(1)


    do {
      val elem = remaining.head
      require(elem >= 0, "bitset element must be >= 0")
      val elemWordIndex = elem >> LogWL
      val elemBit       = 1L << elem
      var elemWord      = readForAdd(elemWordIndex, word0, word1, bitMask)
      if ((elemWord & elemBit) == 0L) {
        elemWord |= elemBit
        if (elemWordIndex > 1 && (bitMask eq null))
          bitMask = createAdderBitMask(BitMask.wordCapacity(remaining), word0, word1)
        if (bitMask ne null)
          bitMask(elemWordIndex) = elemWord
        else if (elemWordIndex == 0) word0 = elemWord
             else word1 = elemWord
      }
      remaining = remaining.tail
    } while (!remaining.isEmpty)
    createFromAddResult(word0, word1, bitMask)
  }
  /**
   * Add all the elements, returning `this` if unchanged, or a new BitSet otherwise
   *
   * The optimisation is to cope with small immutable bitset, by holding the word0/1 lccally
   * manipulating there and checking to see the they are different at the end of the traversal
   */
  override protected[scala] def removeAll(toRemove: collection.TraversableOnce[Int]): BitSet = {
    object immutableRemover extends AbstractFunction1[Int, Unit] {
      def removeAll(elems: TraversableOnce[Int]): BitSet = {
        elems.foreach(this)
        if (word0 == word(0) && word1 == word(1)) BitSet.this
        else BitSet.createSmall(word0, word1)
      }
      var word0: Long = word(0)
      var word1: Long = word(1)
      override def apply(elem: Int): Unit = {
        val elemWordIndex = elem >> LogWL
        if (elemWordIndex >= 0 && elemWordIndex < 2) {
          val elemBitMask = ~(1L << elem)
          if (elemWordIndex == 0) word0 &= elemBitMask
          else word1 &= elemBitMask
        }
      }
    }
    if (nwords <= 2) immutableRemover.removeAll(toRemove)
    else super.removeAll(toRemove)
  }

  /**
   * builds on the basic algorithm of removeAll but avoid allocation of the Functon1, and uses haed/tail
   * based iteration
   */

  override protected[scala] def removeAllLinearSeq(toRemove: collection.LinearSeq[Int]): BitSet =
    if (nwords > 2) super.removeAllLinearSeq(toRemove)
    else {
      var remaining   = toRemove
      var word0: Long = word(0)
      var word1: Long = word(1)
      while (!remaining.isEmpty) {
        val elem          = remaining.head
        val elemWordIndex = elem >> LogWL
        if (elemWordIndex >= 0 && elemWordIndex < 2) {
          val elemBitMask = ~(1L << elem)
          if (elemWordIndex == 0) word0 &= elemBitMask
          else word1 &= elemBitMask
        }
        remaining = remaining.tail
      }
      if (word0 == word(0) && word1 == word(1)) BitSet.this
      else BitSet.createSmall(word0, word1)
    }

}

/** $factoryInfo
 *
 * @define Coll `immutable.BitSet`
 * @define coll immutable bitset
 */
object BitSet extends BitSetFactory[BitSet] {
  /** The empty bitset */
  def empty: BitSet = _empty

  override def apply(elems: Int*): BitSet = {
    elems match {
      case wa: mutable.WrappedArray.ofInt =>
        // for wrapped arrays, as traversal is cheap, and the elements are unboxed
        // we traverse the array twice, once to find the highest value and check for < 0,
        // and the second time to populate the bitmask, which is minimally sized
        //
        var highestValue = -1
        var i            = 0
        val array        = wa.array
        while (i < array.length) {
          highestValue = checkRange(highestValue, array(i))
          i += 1
        }
        val capacity = (highestValue >> LogWL) + 1
        i = 0
        if (capacity <= 2) {
          var w0 = 0L
          var w1 = 0L
          while (i < array.length) {
            val elem  = array(i)
            val bit   = 1L << elem
            val index = elem >> LogWL
            if (index == 0) w0 |= bit
            else w1 |= bit
            i += 1
          }
          createSmall(w0, w1)
        } else {
          val bitMask = arrayOfWords(capacity)
          while (i < array.length) {
            addToBitMask(bitMask, array(i))
            i += 1
          }
          fromBitMaskNoCopy(bitMask)

        }
      case ls: collection.LinearSeq[Int]  =>
        val capacity = wordCapacity(ls)
        if (capacity <= 2) {
          var w0   = 0L
          var w1   = 0L
          var rest = ls

          while (!rest.isEmpty) {
            val elem = rest.head
            require(elem >= 0, "bitset element must be >= 0")
            val bit   = 1L << elem
            val index = elem >> LogWL
            if (index == 0) w0 |= bit
            else w1 |= bit
            rest = rest.tail
          }
          createSmall(w0, w1)
        } else {
          val bitMask = arrayOfWords(capacity)
          setBits(bitMask, elems)
          fromBitMaskNoCopy(bitMask)
        }
      case _                              =>
        //for the general case we dont know the cost of iteration vs reallocation
        //so probably not worth assuming
        //if there are other hot cases then we can revisit
        (newBuilder ++= elems).result
    }
  }

  private object _empty extends BitSet {
    override protected def word(idx: Int): Long = 0
    override def nwords: Int = 0
    override private[collection] def lastWordSet = -1
    override def |(other: collection.BitSet): BitSet =
      other match {
        case bs: immutable.BitSet         => bs
        case _ if (other.lastWordSet > 1) => super.|(other)
        case _                            => createSmall(other._word(0), other._word(1))
      }
    override def &(other: collection.BitSet): BitSet = this
    override def &~(other: collection.BitSet): BitSet = this
    override def --(elems: GenTraversableOnce[Int]): BitSet = this
    override def contains(elem: Int): Boolean = false
    override def subsetOf(that: GenSet[Int]): Boolean = true
    /** Update word at index `idx`; enlarge set if `idx` outside range of set.
     */
    override protected def updateWord(idx: Int, w: Long): BitSet = {
      if (idx == 0) createSmall(w, 0L)
      else if (idx == 1) createSmall(0L, w)
      else {
        val array = new Array[Long](idx + 1)
        array(idx) = w
        fromBitMaskNoCopy(array)
      }
    }
    override private[scala] def filterImpl(p: Int => Boolean, isFlipped: Boolean) = this
  }

  private[BitSet] def createSmall(a: Long, b: Long): BitSet =
    if (b != 0L) new BitSet2(a, b)
    else if (a != 0) new BitSet1(a)
    else empty

  /** A builder that takes advantage of mutable BitSets. */
  def newBuilder: Builder[Int, BitSet] = new Builder[Int, BitSet] {
    private[this] val b = new mutable.BitSet
    override def ++=(xs: TraversableOnce[Int]): this.type = {
      b ++= xs;
      this
    }
    def +=(x: Int) = {
      b += x;
      this
    }
    def clear() = b.clear()
    def result() = b.toImmutable
  }

  /** $bitsetCanBuildFrom */
  implicit val canBuildFrom: CanBuildFrom[BitSet, Int, BitSet] = bitsetCanBuildFrom

  /** A bitset containing all the bits in an array
   * but trimming zeros from the RHS
   */
  def fromBitMask(elems: Array[Long]): BitSet =
    fromBitMaskArray(elems, false)

  /** A bitset containing all the bits in an array,
   * trimming zeros from the RHS.
   * sharing the underlyingArray if there are no zores on the RHS.
   */
  def fromBitMaskNoCopy(elems: Array[Long]): BitSet =
    fromBitMaskArray(elems, true)

  private def fromBitMaskArray(elems: Array[Long], canShareArray: Boolean): BitSet = {
    var len = elems.length
    while (len > 0 && (elems(len - 1) == 0L)) len -= 1
    if (len == 0) empty
    else if (len == 1) new BitSet1(elems(0))
    else if (len == 2) createSmall(elems(0), elems(1))
    else if (canShareArray && len == elems.length) new BitSetN(elems)
    else new BitSetN(java.util.Arrays.copyOf(elems, len))
  }

  @SerialVersionUID(2260107458435649300L)
  class BitSet1(val elems: Long) extends BitSet {
    protected def nwords = 1
    protected def word(idx: Int) = if (idx == 0) elems else 0L
    protected def updateWord(idx: Int, w: Long): BitSet =
      if (idx == 0)
        if (w == 0L) empty
        else new BitSet1(w)
      else if (idx == 1) createSmall(elems, w)
      else {
        val array = new Array[Long](idx + 1)
        array(0) = elems
        array(idx) = w
        fromBitMaskNoCopy(array)
      }
    override def head: Int =
      if (elems == 0L) throw new NoSuchElementException("Empty BitSet")
      else java.lang.Long.numberOfTrailingZeros(elems)
    override def tail: BitSet =
      if (elems == 0L) throw new NoSuchElementException("Empty BitSet")
      else createSmall(elems ^ java.lang.Long.lowestOneBit(elems), 0L)

    override def |(other: collection.BitSet): BitSet = {
      val lws = other.lastWordSet
      if (lws > 1) super.|(other)
      else {
        val w0 = elems | other._word(0)
        val w1 = other._word(1)
        if (w0 == elems && w1 == 0L) this
        else createSmall(w0, w1)
      }
    }
    override def &(other: collection.BitSet): BitSet = {
      val w0 = elems & other._word(0)
      if (w0 == elems) this
      else createSmall(w0, 0L)
    }
    override def &~(other: collection.BitSet): BitSet = {
      val w0 = elems & ~other._word(0)
      if (w0 == elems) this
      else createSmall(w0, 0L)
    }
  }

  class BitSet2(val elems0: Long, elems1: Long) extends BitSet {
    protected def nwords = 2
    protected def word(idx: Int) = if (idx == 0) elems0 else if (idx == 1) elems1 else 0L
    protected def updateWord(idx: Int, w: Long): BitSet =
      if (idx == 0) createSmall(w, elems1)
      else if (idx == 1) createSmall(elems0, w)
      else {
        val array = new Array[Long](idx + 1)
        array(0) = elems0
        array(1) = elems1
        array(idx) = w
        fromBitMaskNoCopy(array)
      }
    override def head: Int =
      if (elems0 == 0L) {
        if (elems1 == 0) throw new NoSuchElementException("Empty BitSet")
        64 + java.lang.Long.numberOfTrailingZeros(elems1)
      }
      else java.lang.Long.numberOfTrailingZeros(elems0)
    override def tail: BitSet =
      if (elems0 == 0L) {
        if (elems1 == 0L) throw new NoSuchElementException("Empty BitSet")
        createSmall(elems0, elems1 ^ java.lang.Long.lowestOneBit(elems1))
      }
      else createSmall(elems0 ^ java.lang.Long.lowestOneBit(elems0), elems1)

    override def |(other: collection.BitSet): BitSet = {
      val lws = other.lastWordSet
      if (lws > 1) super.|(other)
      else {
        val w0 = elems0 | other._word(0)
        val w1 = elems1 | other._word(1)
        if (w0 == elems0 && w1 == elems1) this
        else createSmall(w0, w1)
      }
    }
    override def &(other: collection.BitSet): BitSet = {
      val w0 = elems0 & other._word(0)
      val w1 = elems1 & other._word(1)
      if (w0 == elems0 && w1 == elems1) this
      else createSmall(w0, w1)
    }
    override def &~(other: collection.BitSet): BitSet = {
      val w0 = elems0 & ~other._word(0)
      val w1 = elems1 & ~other._word(1)
      if (w0 == elems0 && w1 == elems1) this
      else createSmall(w0, w1)
    }
  }

  /** The implementing class for bit sets with elements >= 128 (exceeding
   * the capacity of two long values). The constructor wraps an existing
   * bit mask without copying, thus exposing a mutable part of the internal
   *  implementation. Care needs to be taken not to modify the exposed
   * array.
   */
  class BitSetN(val elems: Array[Long]) extends BitSet {
    protected def nwords = elems.length
    protected def word(idx: Int) = if (idx < nwords) elems(idx) else 0L
    protected def updateWord(idx: Int, w: Long): BitSet = {
      val lws = lastWordSet
      if (w == 0 && elems.length > 2 && idx >= lws) {
        // its either ogg the end  (so a no-op) or a shrink
        // for a shrink  - we can avoid creating and then resizing he array
        // or creating the array at all if its now small
        if (idx > lws) this
        else {
          var i = idx - 1
          while (i >= 2 && elems(i) == 0L) i -= 1
          if (i <= 1) return createSmall(elems(0), elems(1))
          else fromBitMaskNoCopy(newArray(i, 0, i + 1))
        }
      } else
          fromBitMaskNoCopy(updateArray(elems, idx, w))
    }
    override def tail: BitSet = {
      val n = nwords
      var i = 0
      while (i < n) {
        val wi = word(i)
        if (wi != 0L) return updateWord(i, wi ^ java.lang.Long.lowestOneBit(wi))
        i += 1
      }
      throw new NoSuchElementException("Empty BitSet")
    }
    override def |(other: collection.BitSet): BitSet = if (other eq this) this else {
      var idx       = other.lastWordSet
      var canBeThis = idx < elems.length
      while (idx >= 0 && canBeThis) {
        canBeThis = (other._word(idx) | elems(idx)) == elems(idx)
        idx -= 1
      }
      if (canBeThis) this
      else super.|(other)
    }
    override def &(other: collection.BitSet): BitSet = if (other eq this) this else {
      var idx       = lastWordSet
      var canBeThis = true
      while (idx >= 0 && canBeThis) {
        canBeThis = (other._word(idx) & elems(idx)) == elems(idx)
        idx -= 1
      }
      if (canBeThis) this
      else super.&(other)
    }
    override def &~(other: collection.BitSet): BitSet = if (other eq this) this else {
      var idx       = lastWordSet
      var canBeThis = true
      while (idx >= 0 && canBeThis) {
        canBeThis = (other._word(idx) & ~elems(idx)) == elems(idx)
        idx -= 1
      }
      if (canBeThis) this
      else super.&~(other)
    }
    override private[scala] def newArray(newSize: Int, minIndexToCopy: Int, maxIndexToCopy: Int = Int.MaxValue): Array[Long] = {
      val maxIndex = Math.min(elems.length, Math.min(newSize, maxIndexToCopy))
      val res      = new Array[Long](newSize)
      if (maxIndex > minIndexToCopy)
        System.arraycopy(elems, minIndexToCopy, res, minIndexToCopy, maxIndex - minIndexToCopy)
      res
    }

    /** optimised override to simplify iteration, as we don't need to consider small BitSets */
    override protected[scala] def addAllTraversable(toAdd: collection.Traversable[Int]) = {
      object bitSetNAdder extends AbstractFunction1[Int, Unit] {
        def addAll: BitSet = {
          toAdd.foreach(this)
          if (bitMask eq null) BitSetN.this
          else fromBitMaskNoCopy(bitMask)
        }

        var bitMask: Array[Long] = _
        override def apply(elem: Int): Unit = {
          require(elem >= 0, "bitset element must be >= 0")
          val elemWordIndex = elem >> LogWL
          val elemBit       = 1L << elem
          val elemWord      = if (elemWordIndex >= elems.length) 0L else elems(elemWordIndex)
          if ((elemWord & elemBit) == 0L) {
            if (bitMask eq null) {
              val highestWord = BitMask.wordCapacity(toAdd)
              val lws         = lastWordSet
              bitMask = newArray(1 + Math.max(lws, highestWord), 0, lws + 1)
            }
            bitMask(elemWordIndex) |= elemBit
          }
        }
      }
      bitSetNAdder.addAll
    }
    /** optimised override to simplify iteration, as we don't need to consider small BitSets */
    override protected[scala] def addAllLinearSeq(toAdd: collection.LinearSeq[Int]): BitSet = {
      var remaining            = toAdd
      var bitMask: Array[Long] = null

      while (!remaining.isEmpty) {
        val elem = remaining.head
        require(elem >= 0, "bitset element must be >= 0")
        val elemWordIndex = elem >> LogWL
        val elemBit       = 1L << elem
        val elemWord      = if (elemWordIndex >= elems.length) 0L else elems(elemWordIndex)
        if ((elemWord & elemBit) == 0L) {
          if (bitMask eq null) {
            val highestWord = BitMask.wordCapacity(remaining)
            val lws         = lastWordSet
            bitMask = newArray(1 + Math.max(lws, highestWord), 0, lws + 1)
          }
          bitMask(elemWordIndex) |= elemBit
        }
        remaining = remaining.tail
      }
      if (bitMask eq null) BitSetN.this
      else fromBitMaskNoCopy(bitMask)
    }

    /** optimised override to simplify iteration, as we can access the array directly
     * and don't need to consider small BitSets */
    override protected[scala] def removeAll(toRemove: collection.TraversableOnce[Int]) = {
      object bitSetNAdder extends AbstractFunction1[Int, Unit] {
        def addAll: BitSet = {
          toRemove.foreach(this)
          if (bitMask eq null) BitSetN.this
          else fromBitMaskNoCopy(bitMask)
        }

        var bitMask: Array[Long] = _
        override def apply(elem: Int): Unit = {
          val elemWordIndex = elem >> LogWL
          if (elemWordIndex >= 0 && elemWordIndex < elems.length) {
            val elemBit  = 1L << elem
            val elemWord = elems(elemWordIndex)
            if ((elemWord & elemBit) != 0L) {
              if (bitMask eq null)
                bitMask = elems.clone
              bitMask(elemWordIndex) &= ~elemBit
            }
          }
        }
      }
      bitSetNAdder.addAll
    }
    /** optimised override to simplify iteration, as we can access the array directly
     * and don't need to consider small BitSets */
    override protected[scala] def removeAllLinearSeq(toAdd: collection.LinearSeq[Int]): BitSet = {
      var remaining            = toAdd
      var bitMask: Array[Long] = null

      while (!remaining.isEmpty) {
        val elem          = remaining.head
        val elemWordIndex = elem >> LogWL
        if (elemWordIndex >= 0 && elemWordIndex < elems.length) {
          val elemBit = 1L << elem
          if ((elems(elemWordIndex) & elemBit) != 0L) {
            if (bitMask eq null)
              bitMask = elems.clone
            bitMask(elemWordIndex) &= ~elemBit
          }
        }
        remaining = remaining.tail
      }
      if (bitMask eq null) BitSetN.this
      else fromBitMaskNoCopy(bitMask)
    }

  }

}
