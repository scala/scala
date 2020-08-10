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

import BitSetLike._
import mutable.StringBuilder
import scala.util.hashing.MurmurHash3
import java.lang.{Long => jLong}
import java.util

import scala.runtime.AbstractFunction1

/** A template trait for bitsets.
 * $bitsetinfo
 *
 * This trait provides most of the operations of a `BitSet` independently of its representation.
 * It is inherited by all concrete implementations of bitsets.
 *
 * @tparam  This the type of the bitset itself.
 * @define bitsetinfo
 * Bitsets are sets of non-negative integers which are represented as
 * variable-size arrays of bits packed into 64-bit words. The memory footprint of a bitset is
 * determined by the largest number stored in it.
 * @author Martin Odersky
 * @since 2.8
 * @define coll bitset
 * @define Coll `BitSet`
 */
trait BitSetLike[+This <: BitSetLike[This] with SortedSet[Int]] extends SortedSetLike[Int, This] {
  self =>

  def empty: This
  protected[this] def copyOrSelf: This = fromBitMaskNoCopy(toBitMask)

  /** The number of words (each with 64 bits) making up the set */
  protected def nwords: Int
  /** the index of highest word that has any bits set, or -1 if empty */
  private[collection] def lastWordSet: Int = {
    var idx = nwords - 1
    while (idx >= 0 && word(idx) == 0L)
      idx -= 1
    idx
  }

  /** The words at index `idx`, or 0L if outside the range of the set
   * '''Note:''' requires `idx >= 0`
   */
  protected def word(idx: Int): Long
  @inline private[collection] final def _word(idx: Int) = word(idx)

  /** Creates a new set of this kind from an array of longs
   */
  protected def fromBitMaskNoCopy(elems: Array[Long]): This

  /** Creates a bit mask for this set as a new array of longs
   */
  def toBitMask: Array[Long] = newArray(nwords, 0)

  override def size: Int = {
    var s = 0
    var i = nwords
    while (i > 0) {
      i -= 1
      s += java.lang.Long.bitCount(word(i))
    }
    s
  }

  override def isEmpty: Boolean = {
    var wordIndex = 0
    val wordCount = this.nwords
    while (wordIndex < wordCount) {
      if (word(wordIndex) != 0L) return false
      wordIndex += 1
    }
    true
  }

  implicit def ordering: Ordering[Int] = Ordering.Int

  def rangeImpl(from: Option[Int], until: Option[Int]): This = {
    // if the result is empty (because from > last or until < head) return empty
    // if the result is the same because from <= head && until > last return copyOrSelf
    // lazily generate the bitmask no larger that needed in other paths

    if (isEmpty) empty
    else {
      var result: Array[Long] = null
      if (until.isDefined) {
        val u = Math.max(until.get, 0)
        if (u <= last) {
          val untilWord = u >> LogWL
          val untilBit  = u & (WordLength - 1)
          if (untilBit == 0) result = newArray(untilWord, 0)
          else {
            result = newArray(untilWord + 1, 0, untilWord)
            result(untilWord) = word(untilWord) & ((1L << untilBit) - 1)
          }
        }
      }
      if (from.isDefined) {
        val f = Math.max(from.get, 0)
        if (f > headOr(f)) {
          // we know here that we are nonEmpty
          if (f > last) return empty
          val fromWord = f >> LogWL
          val fromBit  = f & (WordLength - 1)

          if (result eq null)
            result = newArray(lastWordSet + 1, fromWord)
          else util.Arrays.fill(result, 0, fromWord, 0L)
          if (fromBit > 0) result(fromWord) &= ~((1L << fromBit) - 1)
        }
      }
      if (result eq null) copyOrSelf else fromBitMaskNoCopy(result)
    }
  }

  def iterator: Iterator[Int] = iteratorFrom(0)

  override def keysIteratorFrom(start: Int) = {
    val s = Math.max(0, start)

    new AbstractIterator[Int] {
      var wordIndex = s >> LogWL
      val wordCount = nwords
      var thisWord  = if (wordIndex < wordCount)
                        word(wordIndex) & (-1L << s)
                      else 0L

      def hasNext: Boolean = {
        while (thisWord == 0 && wordIndex < wordCount) {
          wordIndex += 1
          if (wordIndex < wordCount)
            thisWord = word(wordIndex)
        }
        wordIndex < wordCount
      }
      def next(): Int =
        if (hasNext) {
          val indexInWord = jLong.numberOfTrailingZeros(thisWord)
          thisWord &= thisWord - 1
          wordIndex * WordLength + indexInWord
        } else Iterator.empty.next()
    }
  }

  override def foreach[U](f: Int => U) {
    var wordIndex = 0
    val wordCount = this.nwords
    while (wordIndex < wordCount) {
      var thisWord  = word(wordIndex)
      val baseIndex = wordIndex * WordLength
      while (thisWord != 0L) {
        val indexInWord = jLong.numberOfTrailingZeros(thisWord)
        thisWord &= thisWord - 1
        f(baseIndex + indexInWord)
      }
      wordIndex += 1
    }
  }


  /**
   * find the first index where p(index) == wanted, or -1 if there is no such index
   */
  private def findImpl(p: Int => Boolean, wanted: Boolean): Int = p match {
    case _ =>
      var wordIndex = 0
      val wordCount = this.nwords
      while (wordIndex < wordCount) {
        var thisWord  = word(wordIndex)
        val baseIndex = wordIndex * WordLength
        while (thisWord != 0L) {
          val indexInWord = jLong.numberOfTrailingZeros(thisWord)
          thisWord &= thisWord - 1
          if (wanted == p(baseIndex + indexInWord))
            return baseIndex + indexInWord
        }
        wordIndex += 1
      }
      -1
  }

  override def find(p: Int => Boolean): Option[Int] = {
    val index = p match {
      case that: BitSet =>
        findFirstIntersect(that)
      case _            =>
        findImpl(p, true)
    }
    if (index == -1) None
    else Some(index)
  }

  override def forall(p: Int => Boolean): Boolean =
    p match {
      case that: BitSet =>
        this subsetOf that
      case _            =>
        findImpl(p, false) == -1
    }


  override def exists(p: Int => Boolean): Boolean =
    p match {
      case that: BitSet =>
        findFirstIntersect(that) != -1
      case _            =>
        findImpl(p, true) != -1
    }


  /** Computes the union between this bitset and another bitset by performing
   * a bitwise "or".
   *
   * @param   other the bitset to form the union with.
   * @return a new bitset consisting of all bits that are in this
   *         bitset or in the given bitset `other`.
   */
  def |(other: BitSet): This = {
    var words: Array[Long] = null
    var idx                = Math.max(this.lastWordSet, other.lastWordSet)
    while (idx >= 0) {
      val word = this.word(idx) | other.word(idx)
      if (word != 0) {
        if (words eq null)
          words = new Array[Long](idx + 1)
        words(idx) = word
      }
      idx -= 1
    }
    if (words eq null) empty else fromBitMaskNoCopy(words)
  }

  /** Computes the intersection between this bitset and another bitset by performing
   * a bitwise "and".
   *
   * @param   other the bitset to intersect with.
   * @return a new bitset consisting of all elements that are both in this
   *         bitset and in the given bitset `other`.
   */
  def &(other: BitSet): This = {
    var words: Array[Long] = null
    var idx                = Math.min(this.lastWordSet, other.lastWordSet)
    while (idx >= 0) {
      val word = this.word(idx) & other.word(idx)
      if (word != 0) {
        if (words eq null)
          words = new Array[Long](idx + 1)
        words(idx) = word
      }
      idx -= 1
    }
    if (words eq null) empty else fromBitMaskNoCopy(words)
  }

  /** Computes the difference of this bitset and another bitset by performing
   * a bitwise "and-not".
   *
   * @param other the set of bits to exclude.
   * @return a bitset containing those bits of this
   *         bitset that are not also contained in the given bitset `other`.
   */
  def &~(other: BitSet): This = {
    var words: Array[Long] = null
    var idx                = this.nwords - 1
    while (idx >= 0) {
      val word = this.word(idx) & ~other.word(idx)
      if (word != 0) {
        if (words eq null)
          words = new Array[Long](idx + 1)
        words(idx) = word
      }
      idx -= 1
    }
    if (words eq null) empty else fromBitMaskNoCopy(words)
  }

  /** Computes the symmetric difference of this bitset and another bitset by performing
   * a bitwise "exclusive-or".
   *
   * @param other the other bitset to take part in the symmetric difference.
   * @return a bitset containing those bits of this
   *         bitset or the other bitset that are not contained in both bitsets.
   */
  def ^(other: BitSet): This = {
    if (other.isEmpty) copyOrSelf
    else {
      var words: Array[Long] = null
      var idx                = Math.max(this.nwords, other.nwords) - 1
      while (idx >= 0) {
        val word = this.word(idx) ^ other.word(idx)
        if (word != 0) {
          if (words eq null)
            words = new Array[Long](idx + 1)
          words(idx) = word
        }
        idx -= 1
      }
      if (words eq null) empty else fromBitMaskNoCopy(words)
    }
  }
  private def fromWordOrBitMask(wordIndex: Int, wordValue: Long, bitMask: Array[Long]): This = {
    if (wordIndex == -1) copyOrSelf
    else if (wordIndex >= 0) withUpdateWord(wordIndex, wordValue)
         else fromBitMaskNoCopy(bitMask)
  }

  protected[scala] def withUpdateWord(wordIndex: Int, wordValue: Long): This = {
    val array = newArray(Math.max(wordIndex, lastWordSet) + 1, 0)
    array(wordIndex) = wordValue
    fromBitMaskNoCopy(array)
  }
  protected[scala] def addAllLinearSeq(toAdd: LinearSeq[Int]): This = {
    var remaining              = toAdd
    var wordIndex: Int         = -1
    var wordValue: Long        = 0L
    var bitMask  : Array[Long] = null

    while (!remaining.isEmpty) {
      val elem = remaining.head
      require(elem >= 0, "bitset element must be >= 0")
      val elemWordIndex = elem >> LogWL
      val elemBit       = 1L << elem
      val elemWord      = word(elemWordIndex)
      if ((elemWord & elemBit) == 0L)
        if (wordIndex == -1) {
          wordIndex = elemWordIndex
          wordValue = elemWord | elemBit
        } else if (wordIndex == elemWordIndex) {
          wordValue = wordValue | elemBit
        } else {
          if (bitMask eq null) {
            val lws = lastWordSet
            bitMask = newArray(1 + (wordIndex max elemWordIndex max lws), 0, lws + 1)
            bitMask(wordIndex) = wordValue
            wordIndex = -2
          } else if (bitMask.length <= elemWordIndex) {
            //copy and round up size to next order
            bitMask = util.Arrays.copyOf(bitMask, Integer.highestOneBit(elemWordIndex) << 1)
          }
          bitMask(elemWordIndex) |= elemBit
        }
      remaining = remaining.tail
    }
    fromWordOrBitMask(wordIndex, wordValue, bitMask)
  }
  protected[scala] def addAllArray(toAdd: Array[Int]): This = {
    var index                  = 0
    var wordIndex: Int         = -1
    var wordValue: Long        = 0L
    var bitMask  : Array[Long] = null

    while (index < toAdd.length) {
      val elem = toAdd(index)
      require(elem >= 0, "bitset element must be >= 0")
      val elemWordIndex = elem >> LogWL
      val elemBit       = 1L << elem
      val elemWord      = word(elemWordIndex)
      if ((elemWord & elemBit) == 0L)
        if (wordIndex == -1) {
          wordIndex = elemWordIndex
          wordValue = elemWord | elemBit
        } else if (wordIndex == elemWordIndex) {
          wordValue = wordValue | elemBit
        } else {
          if (bitMask eq null) {
            val lws = lastWordSet
            bitMask = newArray(1 + (wordIndex max elemWordIndex max lws), 0, lws + 1)
            bitMask(wordIndex) = wordValue
            wordIndex = -2
          } else if (bitMask.length <= elemWordIndex) {
            //copy and round up size to next order
            bitMask = util.Arrays.copyOf(bitMask, Integer.highestOneBit(elemWordIndex) << 1)
          }
          bitMask(elemWordIndex) |= elemBit
        }
      index += 1
    }
    fromWordOrBitMask(wordIndex, wordValue, bitMask)
  }
  protected[scala] def addAllRange(toAdd: Range): This = {
    var index                  = 0
    var wordIndex: Int         = -1
    var wordValue: Long        = 0L
    var bitMask  : Array[Long] = null

    val length = toAdd.length

    while (index < length) {
      val elem = toAdd(index)
      require(elem >= 0, "bitset element must be >= 0")
      val elemWordIndex = elem >> LogWL
      val elemBit       = 1L << elem
      val elemWord      = word(elemWordIndex)
      if ((elemWord & elemBit) == 0L)
        if (wordIndex == -1) {
          wordIndex = elemWordIndex
          wordValue = elemWord | elemBit
        } else if (wordIndex == elemWordIndex) {
          wordValue = wordValue | elemBit
        } else {
          if (bitMask eq null) {
            val lws = lastWordSet
            bitMask = newArray(1 + (wordIndex max elemWordIndex max lws), 0, lws + 1)
            bitMask(wordIndex) = wordValue
            wordIndex = -2
          } else if (bitMask.length <= elemWordIndex) {
            //copy and round up size to next order
            bitMask = util.Arrays.copyOf(bitMask, Integer.highestOneBit(elemWordIndex) << 1)
          }
          bitMask(elemWordIndex) |= elemBit
        }
      index += 1
    }
    fromWordOrBitMask(wordIndex, wordValue, bitMask)
  }
  protected[scala] def addAllTraversable(elems: Traversable[Int]): This =
    addAll(elems)
  protected[scala] def addAll(elems: TraversableOnce[Int]): This =
    new Adder(elems).addAll

  protected[scala] class Adder(val elems: TraversableOnce[Int]) extends AbstractFunction1[Int, Unit] {

    def addAll: This = {
      elems.foreach(this)
      fromWordOrBitMask(wordIndex, wordValue, bitMask)
    }

    var wordIndex: Int         = -1
    var wordValue: Long        = _
    var bitMask  : Array[Long] = _
    override def apply(elem: Int): Unit = {
      require(elem >= 0, "bitset element must be >= 0")
      val elemWordIndex = elem >> LogWL
      val elemBit       = 1L << elem
      val elemWord      = word(elemWordIndex)
      if ((elemWord & elemBit) == 0L)
        if (wordIndex == -1) {
          wordIndex = elemWordIndex
          wordValue = elemWord | elemBit
        } else if (wordIndex == elemWordIndex) {
          wordValue = wordValue | elemBit
        } else {
          if (bitMask eq null) {
            val lws = lastWordSet
            val highestIndex = elems match {
              case t: Traversable[Int] => BitMask.wordCapacity(t)
              case _ => wordIndex max elemWordIndex max lws
            }
            bitMask = newArray(1 + highestIndex, 0, lws + 1)
            bitMask(wordIndex) = wordValue
            wordIndex = -2
          } else if (bitMask.length <= elemWordIndex) {
            //copy and round up size to next order
            bitMask = util.Arrays.copyOf(bitMask, Integer.highestOneBit(elemWordIndex) << 1)
          }
          bitMask(elemWordIndex) |= elemBit
        }
    }
  }

  override def ++(elems: GenTraversableOnce[Int]): This = elems match {
    case bs: BitSet                     => this | bs
    case l: collection.LinearSeq[Int]                         =>
      if (l.isEmpty) copyOrSelf
      else addAllLinearSeq(l)
    case wa: mutable.WrappedArray.ofInt =>
      if (wa.isEmpty) copyOrSelf
      else addAllArray(wa.array)
    case r: Range                         =>
      if (r.isEmpty) copyOrSelf
      else addAllRange(r)
    case l: collection.Traversable[Int] =>
      if (l.isEmpty) copyOrSelf
      else addAllTraversable(l)
    case _                              =>
      addAll(elems.seq)
  }

  protected[scala] def removeAll(seq: TraversableOnce[Int]): This = {
    // optimise to reduce allocation,
    // for the case where we remove nothing so wordIndex == -1,
    // or where there is a single word changed removed so wordIndex/wordValue holds that index/value
    object subtract extends AbstractFunction1[Int, Unit] {
      def removeAll = {
        seq.foreach(subtract)
        fromWordOrBitMask(wordIndex, wordValue, bitMask)
      }
      val lws                    = lastWordSet
      var wordIndex: Int         = -1
      var wordValue: Long        = _
      var bitMask  : Array[Long] = _
      override def apply(elem: Int): Unit = if (elem >= 0) {
        val elemWordIndex = elem >> LogWL
        if (elemWordIndex <= lws && elemWordIndex >= 0) {
          val elemBit  = 1L << elem
          val elemWord = word(elemWordIndex)
          if ((elemWord & elemBit) != 0L) {
            if (wordIndex == -1) {
              wordIndex = elemWordIndex
              wordValue = elemWord & ~elemBit
            } else if (wordIndex == elemWordIndex) {
              wordValue = wordValue & ~elemBit
            } else {
              if (bitMask eq null) {
                bitMask = newArray(1 + lws, 0, lws + 1)
                bitMask(wordIndex) = wordValue
                wordIndex = -2
              }
              bitMask(elemWordIndex) = bitMask(elemWordIndex) & ~elemBit
            }
          }
        }
      }
    }
    subtract.removeAll
  }

  protected[scala] def removeAllLinearSeq(toRemove: LinearSeq[Int]): This = {
    val lws                    = lastWordSet
    var remaining              = toRemove
    var wordIndex: Int         = -1
    var wordValue: Long        = 0L
    var bitMask  : Array[Long] = null

    while (!remaining.isEmpty) {
      val elem          = remaining.head
      val elemWordIndex = elem >> LogWL
      if (elemWordIndex <= lws && elemWordIndex >= 0) {
        val elemBit  = 1L << elem
        val elemWord = word(elemWordIndex)
        if ((elemWord & elemBit) != 0L) {
          if (wordIndex == -1) {
            wordIndex = elemWordIndex
            wordValue = elemWord & ~elemBit
          } else if (wordIndex == elemWordIndex) {
            wordValue = wordValue & ~elemBit
          } else {
            if (bitMask eq null) {
              bitMask = newArray(1 + lws, 0, lws + 1)
              bitMask(wordIndex) = wordValue
              wordIndex = -2
            }
            bitMask(elemWordIndex) = bitMask(elemWordIndex) & ~elemBit
          }
        }
      }
      remaining = remaining.tail
    }
    fromWordOrBitMask(wordIndex, wordValue, bitMask)
  }
  protected[scala] def removeAllArray(toRemove: Array[Int]): This = {
    val lws                    = lastWordSet
    var index                  = 0
    var wordIndex: Int         = -1
    var wordValue: Long        = 0L
    var bitMask  : Array[Long] = null

    while (index < toRemove.length) {
      val elem          = toRemove(index)
      val elemWordIndex = elem >> LogWL
      if (elemWordIndex <= lws && elemWordIndex >= 0) {
        val elemBit  = 1L << elem
        val elemWord = word(elemWordIndex)
        if ((elemWord & elemBit) != 0L) {
          if (wordIndex == -1) {
            wordIndex = elemWordIndex
            wordValue = elemWord & ~elemBit
          } else if (wordIndex == elemWordIndex) {
            wordValue = wordValue & ~elemBit
          } else {
            if (bitMask eq null) {
              bitMask = newArray(1 + lws, 0, lws + 1)
              bitMask(wordIndex) = wordValue
              wordIndex = -2
            }
            bitMask(elemWordIndex) = bitMask(elemWordIndex) & ~elemBit
          }
        }
      }
      index += 1
    }
    fromWordOrBitMask(wordIndex, wordValue, bitMask)
  }
  protected[scala] def removeAllRange(toRemove: Range): This = {
    val lws                    = lastWordSet
    var index                  = 0
    var wordIndex: Int         = -1
    var wordValue: Long        = 0L
    var bitMask  : Array[Long] = null

    val length = toRemove.length
    while (index < length) {
      val elem          = toRemove(index)
      val elemWordIndex = elem >> LogWL
      if (elemWordIndex <= lws && elemWordIndex >= 0) {
        val elemBit  = 1L << elem
        val elemWord = word(elemWordIndex)
        if ((elemWord & elemBit) != 0L) {
          if (wordIndex == -1) {
            wordIndex = elemWordIndex
            wordValue = elemWord & ~elemBit
          } else if (wordIndex == elemWordIndex) {
            wordValue = wordValue & ~elemBit
          } else {
            if (bitMask eq null) {
              bitMask = newArray(1 + lws, 0, lws + 1)
              bitMask(wordIndex) = wordValue
              wordIndex = -2
            }
            bitMask(elemWordIndex) = bitMask(elemWordIndex) & ~elemBit
          }
        }
      }
      index += 1
    }
    fromWordOrBitMask(wordIndex, wordValue, bitMask)
  }

  override def --(elems: GenTraversableOnce[Int]): This = elems match {
    case bs: BitSet                   => this &~ bs
    case l: collection.LinearSeq[Int] =>
      if (l.isEmpty) copyOrSelf
      else removeAllLinearSeq(l)
    case wa: mutable.WrappedArray.ofInt =>
      if (wa.isEmpty) copyOrSelf
      else removeAllArray(wa.array)
    case r: Range                         =>
      if (r.isEmpty) copyOrSelf
      else removeAllRange(r)
    case _                            =>
      if (elems.isEmpty) copyOrSelf
      else removeAll(elems.seq)
  }

  private[scala] def newArray(newSize: Int, minIndexToCopy: Int, maxIndexToCopy: Int = Int.MaxValue): Array[Long] = {
    val res = new Array[Long](newSize)
    var i   = Math.min(newSize, maxIndexToCopy) - 1
    while (i >= minIndexToCopy) {
      res(i) = word(i)
      i -= 1
    }
    res
  }


  override private[scala] def filterImpl(p: Int => Boolean, isFlipped: Boolean) = {
    p match {
      case bs: BitSet => if (isFlipped) this &~ bs else this & bs
      case _          =>
        //optimise to reduce allocation
        //special cases are removing everything, or removing nothing
        //if we cant hit either of those cases then we use a bitmap
        //and create from there
        var removeAll            = true
        var removeNone           = true
        var bitMask: Array[Long] = null
        val lastWordSet          = this.lastWordSet
        var wordIndex            = lastWordSet
        while (wordIndex >= 0) {
          val originalWord = word(wordIndex)
          if (originalWord != 0L) {
            var resultWord = 0L
            var thisWord   = originalWord
            val baseIndex  = wordIndex * WordLength
            do {
              val indexInWord = jLong.numberOfTrailingZeros(thisWord)
              thisWord &= thisWord - 1
              if (p(baseIndex + indexInWord) != isFlipped)
                resultWord += 1L << indexInWord
            } while (thisWord != 0L)
            if (bitMask eq null) {
              //so removeAll || removeNone == true
              val nextRemoveAll  = removeAll && resultWord == 0L
              val nextRemoveNone = removeNone && resultWord == originalWord
              if (!nextRemoveAll && !nextRemoveNone)
                bitMask = if (removeAll) new Array[Long](wordIndex + 1)
                          else newArray(lastWordSet + 1, wordIndex + 1)
              removeAll = nextRemoveAll
              removeNone = nextRemoveNone
            }

            if (bitMask ne null) {
              bitMask(wordIndex) = resultWord
            }
          }
          wordIndex -= 1
        }
        if (removeAll) empty
        else if (removeNone) copyOrSelf
             else fromBitMaskNoCopy(bitMask)
    }
  }
  override def intersect(elems: GenSet[Int]): This = elems match {
    case bs: BitSet => this & bs
    case _          => super.intersect(elems)
  }

  override def union(elems: GenSet[Int]): This = elems match {
    case bs: BitSet => this | bs
    case _          => super.union(elems)
  }

  override def diff(elems: GenSet[Int]): This = elems match {
    case bs: BitSet => this &~ bs
    case _          => super.diff(elems)
  }

  def contains(elem: Int): Boolean =
    0 <= elem && (word(elem >> LogWL) & (1L << elem)) != 0L

  /** Tests whether this bitset is a subset of another bitset.
   *
   * @param other the bitset to test.
   * @return `true` if this bitset is a subset of `other`, i.e. if
   *         every bit of this set is also an element in `other`.
   */
  def subsetOf(other: BitSet): Boolean = (this eq other) || {
    val n = nwords
    var i = 0
    while (i < n) {
      val thisWord = this.word(i)
      val thatWord = other.word(i)
      if ((thisWord | thatWord) != thatWord) return false
      i += 1
    }
    true
  }
  /** Index of the first intersection
   * or -1 if the is no intersection
   *
   */
  private def findFirstIntersect(other: BitSet): Int = {
    val n = Math.min(nwords, other.nwords)
    var i = 0
    while (i < n) {
      val thisWord      = this.word(i)
      val thatWord      = other.word(i)
      val wordIntersect = thisWord & thatWord
      if (wordIntersect != 0)
        return i * WordLength + java.lang.Long.numberOfTrailingZeros(wordIntersect)
      i += 1
    }
    -1
  }

  override def headOption: Option[Int] = {
    val res = headOr(-1)
    if (res == -1) None
    else Some(res)
  }
  override def head: Int = {
    val res = headOr(-1)
    if (res == -1) throw new NoSuchElementException("Empty BitSet")
    else res
  }
  private def headOr(ifEmpty: Int): Int = {
    val n = nwords
    var i = 0
    while (i < n) {
      val wi = word(i)
      if (wi != 0L) return WordLength * i + java.lang.Long.numberOfTrailingZeros(wi)
      i += 1
    }
    ifEmpty
  }

  override def lastOption: Option[Int] = {
    val res = lastOr(-1)
    if (res == -1) None
    else Some(res)
  }
  override def last: Int = {
    val res = lastOr(-1)
    if (res == -1) throw new NoSuchElementException("Empty BitSet")
    else res
  }
  private def lastOr(ifEmpty: Int): Int = {
    var i = nwords - 1
    while (i >= 0) {
      val wi = word(i)
      if (wi != 0L) return WordLength * i + 63 - java.lang.Long.numberOfLeadingZeros(wi)
      i -= 1
    }
    ifEmpty
  }

  override def addString(sb: StringBuilder, start: String, sep: String, end: String) = {
    sb append start
    var pre       = ""
    var wordIndex = 0
    val wordCount = this.nwords
    while (wordIndex < wordCount) {
      var thisWord  = word(wordIndex)
      val baseIndex = wordIndex * WordLength
      while (thisWord != 0L) {
        val indexInWord = jLong.numberOfTrailingZeros(thisWord)
        thisWord &= thisWord - 1
        sb.append(pre).append(baseIndex + indexInWord)
        pre = sep
      }
      wordIndex += 1
    }
    sb append end
  }
  override def hashCode(): Int = {
    var i       = 0
    var a, b, n = 0
    var c       = 1
    while (i < nwords) {
      var thisWord = word(i)
      val j        = i * WordLength
      while (thisWord != 0L) {
        val indexInWord = jLong.numberOfTrailingZeros(thisWord)
        val h           = j + indexInWord
        a += h
        b ^= h
        if (h != 0) c *= h
        n += 1
        thisWord &= thisWord - 1
      }
      i += 1
    }
    var h = MurmurHash3.setSeed
    h = MurmurHash3.mix(h, a)
    h = MurmurHash3.mix(h, b)
    h = MurmurHash3.mixLast(h, c)
    MurmurHash3.finalizeHash(h, n)
  }

  override def equals(that: Any): Boolean = that match {
    case that: BitSet =>
      (this eq that) ||
        (that canEqual this) && {
          // we compare the words, working downwards, as if bitset have different nwords,
          // they are likely to have differences in the higher indexes as higher word-count
          // typically only get created if there are some bits to set
          var idx = Math.max(this.nwords, that.nwords) - 1
          while (idx >= 0 && this.word(idx) == that.word(idx)) idx -= 1
          idx == -1
        }
    case _            => super.equals(that)
  }
  override def stringPrefix = "BitSet"
}

/** Companion object for BitSets. Contains private data only */
object BitSetLike {
  /* Final vals can sometimes be inlined as constants (faster) */
  private[collection] final val LogWL      = 6
  private final             val WordLength = 64
  private[collection] final val MaxSize    = (Int.MaxValue >> LogWL) + 1

  private[collection] def updateArray(elems: Array[Long], idx: Int, w: Long): Array[Long] = {
    var len = elems.length
    while (len > 0 && (elems(len - 1) == 0L || w == 0L && idx == len - 1)) len -= 1
    var newlen = len
    if (idx >= newlen && w != 0L) newlen = idx + 1
    val newelems = java.util.Arrays.copyOf(elems, newlen)
    if (idx < newlen) newelems(idx) = w
    else assert(w == 0L)
    newelems
  }
}
