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

import java.util

import generic._
import scala.collection.parallel.immutable.ParHashSet
import scala.annotation.tailrec
import scala.runtime.AbstractFunction1

/** This class implements immutable sets using a hash trie.
 *
 *  '''Note:''' The builder of this hash set may return specialized representations for small sets.
 *
 *  @tparam A      the type of the elements contained in this hash set.
 *
 *  @author  Martin Odersky
 *  @author  Tiark Rompf
 *  @since   2.3
 *  @define Coll `immutable.HashSet`
 *  @define coll immutable hash set
 */
@SerialVersionUID(2L)
sealed class HashSet[A] extends AbstractSet[A]
                    with Set[A]
                    with GenericSetTemplate[A, HashSet]
                    with SetLike[A, HashSet[A]]
                    with CustomParallelizable[A, ParHashSet[A]]
                    with Serializable
{
  import HashSet.{nullToEmpty, bufferSize}

  override def companion: GenericCompanion[HashSet] = HashSet

  //class HashSet[A] extends Set[A] with SetLike[A, HashSet[A]] {

  override def par = ParHashSet.fromTrie(this)

  override def size: Int = 0

  override def empty = HashSet.empty[A]

  def iterator: Iterator[A] = Iterator.empty

  override def foreach[U](f: A => U): Unit = ()

  def contains(e: A): Boolean = get0(e, computeHash(e), 0)

  override def subsetOf(that: GenSet[A]) = that match {
    case that:HashSet[A] =>
      // call the specialized implementation with a level of 0 since both this and that are top-level hash sets
      subsetOf0(that, 0)
    case _ =>
      // call the generic implementation
      super.subsetOf(that)
  }

  /**
   * A specialized implementation of subsetOf for when both this and that are HashSet[A] and we can take advantage
   * of the tree structure of both operands and the precalculated hashcodes of the HashSet1 instances.
   * @param that the other set
   * @param level the level of this and that hashset
   *              The purpose of level is to keep track of how deep we are in the tree.
   *              We need this information for when we arrive at a leaf and have to call get0 on that
   *              The value of level is 0 for a top-level HashSet and grows in increments of 5
   * @return true if all elements of this set are contained in that set
   */
  protected def subsetOf0(that: HashSet[A], level: Int) = {
    // The default implementation is for the empty set and returns true because the empty set is a subset of all sets
    true
  }

  override def + (e: A): HashSet[A] = updated0(e, computeHash(e), 0)

  override def + (elem1: A, elem2: A, elems: A*): HashSet[A] =
    this + elem1 + elem2 ++ elems

  override def union(that: GenSet[A]): HashSet[A] = that match {
    case that: HashSet[A] =>
      if (this eq that) this
      else nullToEmpty(union0(that, 0))
    case _ =>
      if (that.isEmpty) this else {
        //avoid the LazyRef as we don't have an @eager object
        class acc extends AbstractFunction1[A, Unit] {
          var res = HashSet.this
          override def apply(v1: A): Unit = res += v1
        }
        val acc = new acc
        if (that.isInstanceOf[Set[A]])
          that foreach acc
        else
          that.iterator foreach acc
        acc.res
      }
  }

  override def intersect(that: GenSet[A]): HashSet[A] = that match {
    case that: HashSet[A] =>
      val buffer = new Array[HashSet[A]](bufferSize(this.size min that.size))
      nullToEmpty(intersect0(that, 0, buffer, 0))
    case _ => super.intersect(that)
  }

  override def diff(that: GenSet[A]): HashSet[A] = that match {
    case that: HashSet[A] =>
      val buffer = new Array[HashSet[A]](bufferSize(this.size))
      nullToEmpty(diff0(that, 0, buffer, 0))
    case _ => super.diff(that)
  }

  /**
   * Union with a HashSet at a given level
   * @param that a HashSet
   * @param level the depth in the tree. We need to keep track of the level to know how deep we are in the tree
   * @return The union of this and that at the given level. Unless level is zero, the result is not a self-contained
   *         HashSet but needs to be stored at the correct depth
   */
  private[immutable] def union0(that: HashSet[A], level: Int): HashSet[A] = {
    // the default implementation is for the empty set, so we just return that
    that
  }

  /**
   * Intersection with another hash set at a given level
   * @param level the depth in the tree. We need to keep track of the level to know how deep we are in the tree
   * @param buffer a temporary buffer that is used for temporarily storing elements when creating new branch nodes
   * @param offset0 the first offset into the buffer in which we are allowed to write
   * @return The intersection of this and that at the given level. Unless level is zero, the result is not a
   *         self-contained HashSet but needs to be stored at the correct depth
   */
  private[immutable] def intersect0(that: HashSet[A], level: Int, buffer: Array[HashSet[A]], offset0: Int): HashSet[A] = {
    // the default implementation is for the empty set, so we just return the empty set
    null
  }

  /**
   * Diff with another hash set at a given level
   * @param level the depth in the tree. We need to keep track of the level to know how deep we are in the tree
   * @param buffer a temporary buffer that is used for temporarily storing elements when creating new branch nodes
   * @param offset0 the first offset into the buffer in which we are allowed to write
   * @return The diff of this and that at the given level. Unless level is zero, the result is not a
   *         self-contained HashSet but needs to be stored at the correct depth
   */
  private[immutable] def diff0(that: HashSet[A], level: Int, buffer: Array[HashSet[A]], offset0: Int): HashSet[A] = {
    // the default implementation is for the empty set, so we just return the empty set
    null
  }

  def - (e: A): HashSet[A] =
    nullToEmpty(removed0(e, computeHash(e), 0))

  override def tail: HashSet[A] = this - head

  override def filter(p: A => Boolean) = p match {
    case hs: HashSet[A] =>
      intersect(hs)
    case _ =>
      val buffer = new Array[HashSet[A]](bufferSize(size))
      nullToEmpty(filter0(p, false, 0, buffer, 0))
  }

  override def filterNot(p: A => Boolean) = p match {
    case hs: HashSet[A] =>
      diff(hs)
    case _ =>
      val buffer = new Array[HashSet[A]](bufferSize(size))
      nullToEmpty(filter0(p, true, 0, buffer, 0))
  }

  protected def filter0(p: A => Boolean, negate: Boolean, level: Int, buffer: Array[HashSet[A]], offset0: Int): HashSet[A] = null

  protected def elemHashCode(key: A) = key.##

  protected final def improve(hcode: Int) = {
    var h: Int = hcode + ~(hcode << 9)
    h = h ^ (h >>> 14)
    h = h + (h << 4)
    h ^ (h >>> 10)
  }

  private[collection] def computeHash(key: A) = improve(elemHashCode(key))

  protected def get0(key: A, hash: Int, level: Int): Boolean = false

  private[collection] def updated0(key: A, hash: Int, level: Int): HashSet[A] =
    new HashSet.HashSet1(key, hash)

  protected def removed0(key: A, hash: Int, level: Int): HashSet[A] = this

  protected def writeReplace(): AnyRef = new HashSet.SerializationProxy(this)

  override def toSet[B >: A]: Set[B] = this.asInstanceOf[HashSet[B]]
}

/** $factoryInfo
 *  @define Coll `immutable.HashSet`
 *  @define coll immutable hash set
 *
 *  @author  Tiark Rompf
 *  @since   2.3
 *  @define Coll `immutable.HashSet`
 *  @define coll immutable hash set
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
object HashSet extends ImmutableSetFactory[HashSet] {
  override def newBuilder[A]: mutable.Builder[A, HashSet[A]] = new HashSetBuilder[A]

  /** $setCanBuildFromInfo */
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, HashSet[A]] =
    ReusableCBF.asInstanceOf[CanBuildFrom[Coll, A, HashSet[A]]]
  private[this] val ReusableCBF = setCanBuildFrom[Any]

  private object EmptyHashSet extends HashSet[Any] {
    override def head: Any = throw new NoSuchElementException("Empty Set")
    override def tail: HashSet[Any] = throw new NoSuchElementException("Empty Set")
  }
  private[collection] def emptyInstance: HashSet[Any] = EmptyHashSet

  // utility method to create a HashTrieSet from two leaf HashSets (HashSet1 or HashSetCollision1) with non-colliding hash code)
  private def makeHashTrieSet[A](hash0:Int, elem0:HashSet[A], hash1:Int, elem1:HashSet[A], level:Int, newSize: Int) : HashTrieSet[A] = {
    // assert elem0.size + elem1.size == newSize
    val index0 = (hash0 >>> level) & 0x1f
    val index1 = (hash1 >>> level) & 0x1f
    if(index0 != index1) {
      val bitmap = (1 << index0) | (1 << index1)
      val elems = new Array[HashSet[A]](2)
      if(index0 < index1) {
        elems(0) = elem0
        elems(1) = elem1
      } else {
        elems(0) = elem1
        elems(1) = elem0
      }
      new HashTrieSet[A](bitmap, elems, newSize)
    } else {
      val bitmap = (1 << index0)
      val child = makeHashTrieSet(hash0, elem0, hash1, elem1, level + 5, newSize)
      val elems = new Array[HashSet[A]](1)
      elems(0) = child
      new HashTrieSet[A](bitmap, elems,  newSize)
    }
  }

  /**
   * Common superclass of HashSet1 and HashSetCollision1, which are the two possible leaves of the Trie
   */
  @SerialVersionUID(-8788235040812980474L)
  private[HashSet] sealed abstract class LeafHashSet[A](private[HashSet] final val hash: Int) extends HashSet[A]

  @SerialVersionUID(7828248784025959392L)
  class HashSet1[A](private[HashSet] val key: A, hash: Int) extends LeafHashSet[A](hash) {
    override def size = 1

    override protected def get0(key: A, hash: Int, level: Int): Boolean =
      (hash == this.hash && key == this.key)

    override def equals(other: Any): Boolean = {
      other match {
        case that: HashSet1[A] =>
          (this eq that) || (this.hash == that.hash && this.key == that.key)
        case _ : HashSet[_] => false
        case _ => super.equals(other)
      }
    }

    override protected def subsetOf0(that: HashSet[A], level: Int) = {
      // check if that contains this.key
      // we use get0 with our key and hash at the correct level instead of calling contains,
      // which would not work since that might not be a top-level HashSet
      // and in any case would be inefficient because it would require recalculating the hash code
      (this eq that) || that.get0(key, hash, level)
    }

    override private[collection] def updated0(key: A, hash: Int, level: Int): HashSet[A] =
      if (hash == this.hash)
        if (key == this.key) this
        else
        // 32-bit hash collision (rare, but not impossible)
          new HashSetCollision1(hash, ListSet.empty + this.key + key, 2)
      else
        //size known to be 2 because this is a HashSet1, and so is the created set
        makeHashTrieSet(this.hash, this, hash, new HashSet1(key, hash), level, 2)

    override private[immutable] def union0(that: HashSet[A], level: Int) =
      that match {
        case that: HashSet1[A] =>
          if (this.hash == that.hash)
            if (this.key == that.key) this
            else {
              // 32-bit hash collision (rare, but not impossible)
              new HashSetCollision1[A](hash, ListSet.empty + this.key + that.key, 2)
            }
          else {
            // different hash code, so just create a branch node containing the two.
            // size known to be 2 because this is a HashSet1, and so is the created set
            makeHashTrieSet(this.hash, this, that.hash, that, level, 2)
          }

        case _ => //Trie, collision or empty
          // we can exchange the arguments because union is symmetrical
          // generally we prefer to return this where we can,
          // but the result cannot be this for trie and collision (as they have size > 1)
          // and is this for empty
          that.union0(this, level)
      }

    override private[immutable] def intersect0(that: HashSet[A], level: Int, buffer: Array[HashSet[A]], offset0: Int): HashSet[A] =
      if (that.get0(key, hash, level)) this else null

    override private[immutable] def diff0(that: HashSet[A], level: Int, buffer: Array[HashSet[A]], offset0: Int): HashSet[A] =
      if (that.get0(key, hash, level)) null else this

    override protected def removed0(key: A, hash: Int, level: Int): HashSet[A] =
      if (hash == this.hash && key == this.key) null else this

    override protected def filter0(p: A => Boolean, negate: Boolean, level: Int, buffer: Array[HashSet[A]], offset0: Int): HashSet[A] =
      if (negate ^ p(key)) this else null

    override def iterator: Iterator[A] = Iterator(key)
    override def foreach[U](f: A => U): Unit = f(key)
  }

  @SerialVersionUID(-4499898620567995040L)
  private[immutable] class HashSetCollision1[A](hash: Int, val ks: ListSet[A],  override val size: Int) extends LeafHashSet[A](hash) {

    override protected def get0(key: A, hash: Int, level: Int): Boolean =
      if (hash == this.hash) ks.contains(key) else false

    override def equals(other: Any): Boolean = {
      other match {
        case that: HashSetCollision1[A] =>
          (this eq that) || (this.hash == that.hash && this.ks == that.ks)
        case miss : HashSet[_] => false
        case _ => super.equals(other)
      }
    }

    override protected def subsetOf0(that: HashSet[A], level: Int) = {
      // we have to check each element
      // we use get0 with our hash at the correct level instead of calling contains,
      // which would not work since that might not be a top-level HashSet
      // and in any case would be inefficient because it would require recalculating the hash code
      (this eq that) || ks.forall(key => that.get0(key, hash, level))
    }

    override private[collection] def updated0(key: A, hash: Int, level: Int): HashSet[A] =
      if (hash == this.hash)  {
        val ks1 = ks + key
        // ListSet is guaranteed to return itself if key was already present
        if (ks1 eq ks)
          this
        else
          // create a new HashSetCollision with the existing hash
          // we don't have to check for size=1 because union is never going to remove elements
          new HashSetCollision1[A](hash, ks1, size + 1)
      }
      else {
        // size known to be one larger then my size
        makeHashTrieSet(this.hash, this, hash, new HashSet1(key, hash), level, size + 1)
      }

    override private[immutable] def union0(that: HashSet[A], level: Int): HashSet[A] = that match {
      case that: HashSet1[A] =>
        if (that.hash != this.hash)
        // Just create a branch node containing the two.
        // size known to be one larger then my size
          makeHashTrieSet(this.hash, this, that.hash, that, level, size + 1)
        else {
          val ks1 = ks + that.key
          // ListSet is guaranteed to return itself if key was already present
          if (ks1 eq ks)
            this
          else
            // create a new HashSetCollision with the existing hash
            // we don't have to check for size=1 because union is never going to remove elements
            new HashSetCollision1[A](hash, ks1, size + 1)
        }
      case that: HashSetCollision1[A] =>
        if (that.hash != this.hash)
        // Just create a branch node containing the two.
        // size unknown - just the sum of the sizes
          makeHashTrieSet(this.hash, this, that.hash, that, level, size + that.size)
        else if (this eq that) this
        else {
          val ks1 = this.ks ++ that.ks
          // ListSet is guaranteed to return itself when all elements are already in the set,
          if (ks1 eq ks) this
          else {
            val newSize = ks1.size
            if (newSize == that.size)
            // we have to check this as well, since we don't want to create a new instance if this is a subset of that
            // we dont care about the ordering on the ListSet
              that
            else
            // create a new HashSetCollision with the existing hash
            // we don't have to check for size=1 because union is never going to remove elements
              new HashSetCollision1[A](hash, ks1, newSize)
          }
        }
      case _ =>
        // we can swap this and that because union is symmetrical and that is either a
        // HashTrieSet - in which case the result could not be this
        // EmptyHashSet - in which case the result will be this
        that.union0(this, level)
    }

    override private[immutable] def intersect0(that: HashSet[A], level: Int, buffer: Array[HashSet[A]], offset0: Int): HashSet[A] =
      if (this eq that) this else {
      // filter the keys, taking advantage of the fact that we know their hash code
      val ks1 = ks.filter(that.get0(_, hash, level))
      ks1.size match {
        case 0 =>
          // the empty set
          null
        case size if size == this.size =>
          // unchanged
          // We do this check first since even if the result is of size 1 since
          // it is preferable to return the existing set for better structural sharing
          this
        case size if size == that.size =>
          // the other set
          // We do this check first since even if the result is of size 1 since
          // it is preferable to return the existing set for better structural sharing
          that
        case 1 =>
          // create a new HashSet1 with the hash we already know
          new HashSet1(ks1.head, hash)
        case newSize =>
          // create a new HashSetCollision with the hash we already know and the new keys
          new HashSetCollision1(hash, ks1, newSize)
      }
    }

    override private[immutable] def diff0(that: HashSet[A], level: Int, buffer: Array[HashSet[A]], offset0: Int): HashSet[A] =
      if (this eq that) null else {
      val ks1 = ks.filterNot(that.get0(_, hash, level))
      ks1.size match {
        case 0 =>
          // the empty set
          null
        case size if size == this.size =>
          // unchanged
          // We do this check first since even if the result is of size 1 since
          // it is preferable to return the existing set for better structural sharing
          this
        case 1 =>
          // create a new HashSet1 with the hash we already know
          new HashSet1(ks1.head, hash)
        case newSize =>
          // create a new HashSetCollision with the hash we already know and the new keys
          new HashSetCollision1(hash, ks1, newSize)
      }
    }

    override protected def removed0(key: A, hash: Int, level: Int): HashSet[A] =
      if (hash == this.hash) {
        val ks1 = ks - key
        // ListSet guarantees to return itself if `key` is not present
        if (ks1 eq ks) this
        else if (size == 2) {
          // our size was 2, its changed via removing one element, so it must be 1 now
          // create a new HashSet1 with the hash we already know
          new HashSet1(ks1.head, hash)
        } else {
          // we know our size, and we only removed one element, so it must be size -1 now
          // create a new HashSetCollision with the hash we already know and the new keys
          new HashSetCollision1(hash, ks1, size  -1)
        }
      } else this

    override protected def filter0(p: A => Boolean, negate: Boolean, level: Int, buffer: Array[HashSet[A]], offset0: Int): HashSet[A] = {
      val ks1 = if(negate) ks.filterNot(p) else ks.filter(p)
      if (ks1 eq ks) this
      else ks1.size match {
        case 0 =>
          null
        case 1 =>
          new HashSet1(ks1.head, hash)
        case x if x == size =>
          this
        case newSize =>
          new HashSetCollision1(hash, ks1, newSize)
      }
    }

    override def iterator: Iterator[A] = ks.iterator
    override def foreach[U](f: A => U): Unit = ks.foreach(f)

  }

  /**
   * A branch node of the HashTrieSet with at least one and up to 32 children.
   *
   * @param bitmap encodes which element corresponds to which child
   * @param elems the up to 32 children of this node.
   *              the number of children must be identical to the number of 1 bits in bitmap
   * @param size the total number of elements. This is stored just for performance reasons.
   * @tparam A      the type of the elements contained in this hash set.
   *
   * How levels work:
   *
   * When looking up or adding elements, the part of the hashcode that is used to address the children array depends
   * on how deep we are in the tree. This is accomplished by having a level parameter in all internal methods
   * that starts at 0 and increases by 5 (32 = 2^5) every time we go deeper into the tree.
   *
   * hashcode (binary): 00000000000000000000000000000000
   * level=0 (depth=0)                             ^^^^^
   * level=5 (depth=1)                        ^^^^^
   * level=10 (depth=2)                  ^^^^^
   * ...
   *
   * Be careful: a non-toplevel HashTrieSet is not a self-contained set, so e.g. calling contains on it will not work!
   * It relies on its depth in the Trie for which part of a hash to use to address the children, but this information
   * (the level) is not stored due to storage efficiency reasons but has to be passed explicitly!
   *
   * How bitmap and elems correspond:
   *
   * A naive implementation of a HashTrieSet would always have an array of size 32 for children and leave the unused
   * children empty (null). But that would be very wasteful regarding memory. Instead, only non-empty children are
   * stored in elems, and the bitmap is used to encode which elem corresponds to which child bucket. The lowest 1 bit
   * corresponds to the first element, the second-lowest to the second, etc.
   *
   * bitmap (binary): 00010000000000000000100000000000
   * elems: [a,b]
   * children:        ---b----------------a-----------
   */
  @SerialVersionUID(-1260675327783828535L)
  class HashTrieSet[A](private[HashSet] var bitmap: Int, private[collection] var elems: Array[HashSet[A]], private[HashSet] var size0: Int)
        extends HashSet[A] {
    @inline override final def size = size0
    // assert(Integer.bitCount(bitmap) == elems.length)
    // assertion has to remain disabled until scala/bug#6197 is solved
    // assert(elems.length > 1 || (elems.length == 1 && elems(0).isInstanceOf[HashTrieSet[_]]))

    override protected def get0(key: A, hash: Int, level: Int): Boolean = {
      val index = (hash >>> level) & 0x1f
      val mask = (1 << index)
      if (bitmap == - 1) {
        elems(index & 0x1f).get0(key, hash, level + 5)
      } else if ((bitmap & mask) != 0) {
        val offset = Integer.bitCount(bitmap & (mask-1))
        elems(offset).get0(key, hash, level + 5)
      } else
        false
    }

    override private[collection] def updated0(key: A, hash: Int, level: Int): HashSet[A] = {
      val index = (hash >>> level) & 0x1f
      val mask = (1 << index)
      val offset = Integer.bitCount(bitmap & (mask-1))
      if ((bitmap & mask) != 0) {
        val sub = elems(offset)
        val subNew = sub.updated0(key, hash, level + 5)
        if (sub eq subNew) this
        else {
          val elemsNew = new Array[HashSet[A]](elems.length)
          System.arraycopy(elems, 0, elemsNew, 0, elems.length)
          elemsNew(offset) = subNew
          // assert (subNew.size - sub.size == 1)
          new HashTrieSet(bitmap, elemsNew, size + 1)
        }
      } else {
        val elemsNew = new Array[HashSet[A]](elems.length + 1)
        System.arraycopy(elems, 0, elemsNew, 0, offset)
        elemsNew(offset) = new HashSet1(key, hash)
        System.arraycopy(elems, offset, elemsNew, offset + 1, elems.length - offset)
        val bitmapNew = bitmap | mask
        new HashTrieSet(bitmapNew, elemsNew, size + 1)
      }
    }


    override private[immutable] def union0(that: HashSet[A], level: Int) = that match {
      case that if that eq this =>
        // shortcut for when that is this
        // this happens often for nodes deeper in the tree, especially when that and this share a common "heritage"
        // e.g. you have a large set A and do some small operations (adding and removing elements) to it to create B
        // then A and B will have the vast majority of nodes in common, and this eq check will allow not even looking
        // at these nodes.
        this
      case that: LeafHashSet[A] =>
        val index = (that.hash >>> level) & 0x1f
        val mask = (1 << index)
        val offset = Integer.bitCount(bitmap & (mask - 1))
        if ((bitmap & mask) != 0) {
          val sub = elems(offset)
          if (sub eq that) this
          else {
            val sub1 = sub.union0(that, level + 5)
            if (sub eq sub1) this
            else {
              val elems1 = elems.clone()
              // its just a little faster than new Array[HashSet[A]](elems.length); System.arraycopy(elems, 0, elems1, 0, elems.length)
              elems1(offset) = sub1
              new HashTrieSet(bitmap, elems1, size + (sub1.size - sub.size))
            }
          }
        } else {
          val elems1 = new Array[HashSet[A]](elems.length + 1)
          System.arraycopy(elems, 0, elems1, 0, offset)
          elems1(offset) = that
          System.arraycopy(elems, offset, elems1, offset + 1, elems.length - offset)
          val bitmap1 = bitmap | mask
          new HashTrieSet(bitmap1, elems1, size + that.size)
        }
      case that: HashTrieSet[A] =>
        def addMaybeSubset(larger: HashTrieSet[A], smaller: HashTrieSet[A]): HashTrieSet[A] = {
          var resultElems: Array[HashSet[A]] = null
          var ai = 0
          var bi = 0
          var abm = larger.bitmap
          var bbm = smaller.bitmap
          val a = larger.elems
          val b = smaller.elems

          //larger has all the bits or smaller, and if they have the same bits, is at least the bigger
          //so we try to merge `smaller`into `larger`and hope that `larger is a superset

          //the additional size in the results, so the eventual size of the result is larger.size + additionalSize
          var additionalSize = 0

          // could be lsb = Integer.lowestOneBit(abm)
          //but is this faster!!
          // keep fastest in step with adjustments in the loop
          //we know abm contains all of the bits in bbm, we only loop through bbm
          //bsb is the next lowest bit in smaller
          var bsb = bbm ^ (bbm & (bbm - 1))
          while (bsb != 0) {
            val skippedBitsInA = abm & (bsb - 1)
            ai += Integer.bitCount(skippedBitsInA)
            abm ^= skippedBitsInA
            val aai = a(ai)
            val bbi = b(bi)

            val result = if (aai eq bbi) aai
            else aai.union0(bbi, level + 5)
            if (result ne aai) {
              if (resultElems eq null)
                resultElems = a.clone()
              additionalSize += result.size - aai.size
              //assert (result.size > aai.size)
              resultElems(ai) = result
            }
            abm ^= bsb
            bbm ^= bsb
            bsb = bbm ^ (bbm & (bbm - 1))

            ai += 1
            bi += 1
          }
          // we don't have to check whether the result is a leaf, since union will only make the set larger
          // and this is not a leaf to begin with.
          if (resultElems eq null) larger // happy days - no change
          else new HashTrieSet(larger.bitmap, resultElems, larger.size + additionalSize)
        }

        def addDistinct(that: HashTrieSet[A]): HashTrieSet[A] = {

          // the sets are distinct, so its a bit simpler to combine
          // and we can avoid all of the quite expensive size calls on the children

          var ai = 0
          var bi = 0
          var offset = 0
          val abm = this.bitmap
          val bbm = that.bitmap
          val a = this.elems
          val b = that.elems
          var allBits = abm | bbm

          val resultElems = new Array[HashSet[A]](Integer.bitCount(allBits))
          // could be lsb = Integer.lowestOneBit(abm)
          //but is this faster!!
          // keep fastest in step with adjustments in the loop
          // lowest remaining bit
          var lsb = allBits ^ (allBits & (allBits - 1))

          while (lsb != 0) {
            if ((lsb & abm) != 0) {
              resultElems(offset) = a(ai)
              ai += 1
            } else {
              resultElems(offset) = b(bi)
              bi += 1
            }
            offset += 1
            allBits ^= lsb
            lsb = allBits ^ (allBits & (allBits - 1))
          }
          // we don't have to check whether the result is a leaf, since union will only make the set larger
          // and this is not a leaf to begin with.
          new HashTrieSet(abm | bbm, resultElems, this.size + that.size)
        }

        def addCommon(that: HashTrieSet[A]): HashTrieSet[A] = {
          var ai = 0
          var bi = 0
          val abm = this.bitmap
          val bbm = that.bitmap
          val a = this.elems
          val b = that.elems
          var allBits = abm | bbm
          val resultElems = new Array[HashSet[A]](Integer.bitCount(allBits))

          //output index
          var offset = 0

          // the size of the results so far
          var rs = 0

          // could be alsb = Integer.lowestOneBit(abm)
          //but is this faster!!
          // keep fastest in step with adjustments in the loop
          // lowest remaining bit
          var lsb = allBits ^ (allBits & (allBits - 1))

          var result: HashSet[A] = null
          // loop as long as there are bits left in either abm or bbm
          while (lsb != 0) {
            if ((lsb & abm) != 0) {
              if ((lsb & bbm) != 0) {
                // lsb is in a and b, so combine
                val aai = a(ai)
                val bbi = b(bi)

                result = if (aai eq bbi) aai
                else aai.union0(bbi, level + 5)
                ai += 1
                bi += 1
              } else {
                // lsb is in a
                result = a(ai)
                ai += 1
              }
            } else {
              // lsb is in b
              result = b(bi)
              bi += 1
            }
            // update lsb
            allBits ^= lsb
            lsb = allBits ^ (allBits & (allBits - 1))

            resultElems(offset) = result
            rs += result.size
            offset += 1
          }
          // we don't have to check whether the result is a leaf, since union will only make the set larger
          // and this is not a leaf to begin with.
          new HashTrieSet(abm | bbm, resultElems, rs)

        }

        // if we have a subset/superset relationship, then we can merge and not allocate if thats a real subset
        // we check on that relationship based on the bitssets, and if he bitsets are the same than we look at the size
        // to work out the subset vs the superset
        // a superset here is  a trie that has all the bits of the other and is possible to be a superset
        //
        // if the bits are distinct we can skip some processing so we have a path for that
        // otherwise the general case

        val abm = this.bitmap
        val bbm = that.bitmap
        val allBits = abm | bbm

        if (allBits == abm && (allBits != bbm || this.size >= that.size)) addMaybeSubset(this, that)
        else if (allBits == bbm) addMaybeSubset(that, this)
        else if ((abm & bbm) == 0) addDistinct(that)
        else addCommon(that)

      case _ => this
    }

    override private[immutable] def intersect0(that: HashSet[A], level: Int, buffer: Array[HashSet[A]], offset0: Int): HashSet[A] = that match {
      case that if that eq this =>
        // shortcut for when that is this
        // this happens often for nodes deeper in the tree, especially when that and this share a common "heritage"
        // e.g. you have a large set A and do some small operations (adding and removing elements) to it to create B
        // then A and B will have the vast majority of nodes in common, and this eq check will allow not even looking
        // at these nodes!
        this
      case that: LeafHashSet[A] =>
        // when that is a leaf, we can switch to the simpler Tree/Leaf implementation
        // it is OK to swap the arguments because intersect is symmetric
        // (we can't do this in case of diff, which is not symmetric)
        that.intersect0(this, level, buffer, offset0)
      case that: HashTrieSet[A] =>
        val a = this.elems
        var abm = this.bitmap
        var ai = 0

        val b = that.elems
        var bbm = that.bitmap
        var bi = 0

        // if the bitmasks do not overlap, the result is definitely empty so we can abort here
        if ((abm & bbm) == 0)
          return null

        // fetch a new temporary array that is guaranteed to be big enough (32 elements)
        var offset = offset0
        var rs = 0
        var rbm = 0

        // loop as long as there are bits left that are set in both abm and bbm
        while ((abm & bbm) != 0) {
          // highest remaining bit in abm
          val alsb = abm ^ (abm & (abm - 1))
          // highest remaining bit in bbm
          val blsb = bbm ^ (bbm & (bbm - 1))
          if (alsb == blsb) {
            val sub1 = a(ai).intersect0(b(bi), level + 5, buffer, offset)
            if (sub1 ne null) {
              rs += sub1.size
              rbm |= alsb
              buffer(offset) = sub1
              offset += 1
            }
            // clear lowest remaining one bit in abm and increase the a index
            abm &= ~alsb
            ai += 1
            // clear lowest remaining one bit in bbm and increase the b index
            bbm &= ~blsb
            bi += 1
          } else if (unsignedCompare(alsb - 1, blsb - 1)) {
            // alsb is smaller than blsb, or alsb is set and blsb is 0
            // in any case, alsb is guaranteed to be set here!
            // clear lowest remaining one bit in abm and increase the a index
            abm &= ~alsb
            ai += 1
          } else {
            // blsb is smaller than alsb, or blsb is set and alsb is 0
            // in any case, blsb is guaranteed to be set here!
            // clear lowest remaining one bit in bbm and increase the b index
            bbm &= ~blsb
            bi += 1
          }
        }

        if (rbm == 0) {
          // if the result bitmap is empty, the result is the empty set
          null
        } else if (rs == size) {
          // if the result has the same number of elements as this, it must be identical to this,
          // so we might as well return this
          this
        } else if (rs == that.size) {
          // if the result has the same number of elements as that, it must be identical to that,
          // so we might as well return that
          that
        } else {
          val length = offset - offset0
          if (length == 1 && !buffer(offset0).isInstanceOf[HashTrieSet[A]])
            buffer(offset0)
          else {
            val elems = new Array[HashSet[A]](length)
            System.arraycopy(buffer, offset0, elems, 0, length)
            new HashTrieSet[A](rbm, elems, rs)
          }
        }
      case _ => null
    }

    override private[immutable] def diff0(that: HashSet[A], level: Int, buffer: Array[HashSet[A]], offset0: Int): HashSet[A] = that match {
      case that if that eq this =>
        // shortcut for when that is this
        // this happens often for nodes deeper in the tree, especially when that and this share a common "heritage"
        // e.g. you have a large set A and do some small operations (adding and removing elements) to it to create B
        // then A and B will have the vast majority of nodes in common, and this eq check will allow not even looking
        // at these nodes!
        null
      case that: HashSet1[A] =>
        removed0(that.key, that.hash, level)
      case that: HashTrieSet[A] =>
        val a = this.elems
        var abm = this.bitmap
        var ai = 0

        val b = that.elems
        var bbm = that.bitmap
        var bi = 0

        // fetch a new temporary array that is guaranteed to be big enough (32 elements)
        var offset = offset0
        var rs = 0
        var rbm = 0

        // loop until there are no more bits in abm
        while(abm!=0) {
          // highest remaining bit in abm
          val alsb = abm ^ (abm & (abm - 1))
          // highest remaining bit in bbm
          val blsb = bbm ^ (bbm & (bbm - 1))
          if (alsb == blsb) {
            val sub1 = a(ai).diff0(b(bi), level + 5, buffer, offset)
            if (sub1 ne null) {
              rs += sub1.size
              rbm |= alsb
              buffer(offset) = sub1
              offset += 1
            }
            // clear lowest remaining one bit in abm and increase the a index
            abm &= ~alsb; ai += 1
            // clear lowest remaining one bit in bbm and increase the b index
            bbm &= ~blsb; bi += 1
          } else if (unsignedCompare(alsb - 1, blsb - 1)) {
            // alsb is smaller than blsb, or alsb is set and blsb is 0
            // in any case, alsb is guaranteed to be set here!
            val sub1 = a(ai)
            rs += sub1.size
            rbm |= alsb
            buffer(offset) = sub1; offset += 1
            // clear lowest remaining one bit in abm and increase the a index
            abm &= ~alsb; ai += 1
          } else {
            // blsb is smaller than alsb, or blsb is set and alsb is 0
            // in any case, blsb is guaranteed to be set here!
            // clear lowest remaining one bit in bbm and increase the b index
            bbm &= ~blsb; bi += 1
          }
        }
        if (rbm == 0) {
          null
        } else if (rs == this.size) {
          // if the result has the same number of elements as this, it must be identical to this,
          // so we might as well return this
          this
        } else {
          val length = offset - offset0
          if (length == 1 && !buffer(offset0).isInstanceOf[HashTrieSet[A]])
            buffer(offset0)
          else {
            val elems = new Array[HashSet[A]](length)
            System.arraycopy(buffer, offset0, elems, 0, length)
            new HashTrieSet[A](rbm, elems, rs)
          }
        }
      case that: HashSetCollision1[A] =>
        // we remove the elements using removed0 so we can use the fact that we know the hash of all elements
        // to be removed
        @tailrec def removeAll(s:HashSet[A], r:ListSet[A]) : HashSet[A] =
          if(r.isEmpty || (s eq null)) s
          else removeAll(s.removed0(r.head, that.hash, level), r.tail)
        removeAll(this, that.ks)
      case _ => this
    }

    override protected def removed0(key: A, hash: Int, level: Int): HashSet[A] = {
      val index = (hash >>> level) & 0x1f
      val mask = (1 << index)
      val offset = Integer.bitCount(bitmap & (mask-1))
      if ((bitmap & mask) != 0) {
        val sub = elems(offset)
        val subNew = sub.removed0(key, hash, level + 5)
        if (sub eq subNew) this
        else if (subNew eq null) {
          val bitmapNew = bitmap ^ mask
          if (bitmapNew != 0) {
            if (elems.length == 2 && !elems(offset ^ 1).isInstanceOf[HashTrieSet[_]] ) {
              // if we have only one child, which is not a HashTrieSet but a self-contained set like
              // HashSet1 or HashSetCollision1, return the child instead
              elems(offset ^ 1)
            } else {
              val elemsNew = new Array[HashSet[A]](elems.length - 1)
              System.arraycopy(elems, 0, elemsNew, 0, offset)
              System.arraycopy(elems, offset + 1, elemsNew, offset, elems.length - offset - 1)
              //assert (sub.size == 1)
              val sizeNew = size - 1
              new HashTrieSet(bitmapNew, elemsNew, sizeNew)
            }
          } else
            null
        } else if(elems.length == 1 && !subNew.isInstanceOf[HashTrieSet[_]]) {
          subNew
        } else {
          val elemsNew = new Array[HashSet[A]](elems.length)
          System.arraycopy(elems, 0, elemsNew, 0, elems.length)
          elemsNew(offset) = subNew
          //assert (subNew.size - sub.size == -1)
          val sizeNew = size -1
          new HashTrieSet(bitmap, elemsNew, sizeNew)
        }
      } else {
        this
      }
    }

    override def equals(other: Any): Boolean = {
      other match {
        case that: HashTrieSet[A] =>
          (this eq that) || (this.bitmap == that.bitmap && this.size == that.size &&
            util.Arrays.equals(this.elems.asInstanceOf[Array[AnyRef]], that.elems.asInstanceOf[Array[AnyRef]]))
        case _ : HashSet[_] => false
        case _ => super.equals(other)
      }
    }

    override protected def subsetOf0(that: HashSet[A], level: Int): Boolean = (that eq this) || (that match {
      case that: HashTrieSet[A]
        if (this.bitmap & ~that.bitmap) == 0
          && this.size <= that.size =>
        // create local mutable copies of members
        var abm = this.bitmap
        val a = this.elems
        var ai = 0
        val b = that.elems
        var bbm = that.bitmap
        var bi = 0
        if ((abm & bbm) == abm) {
          // I tried rewriting this using tail recursion, but the generated java byte code was less than optimal
          while(abm!=0) {
            // highest remaining bit in abm
            val alsb = abm ^ (abm & (abm - 1))
            // highest remaining bit in bbm
            val blsb = bbm ^ (bbm & (bbm - 1))
            // if both trees have a bit set at the same position, we need to check the subtrees
            if (alsb == blsb) {
              // we are doing a comparison of a child of this with a child of that,
              // so we have to increase the level by 5 to keep track of how deep we are in the tree
              if (!a(ai).subsetOf0(b(bi), level + 5))
                return false
              // clear lowest remaining one bit in abm and increase the a index
              abm &= ~alsb; ai += 1
            }
            // clear lowermost remaining one bit in bbm and increase the b index
            // we must do this in any case
            bbm &= ~blsb; bi += 1
          }
          true
        } else {
          // the bitmap of this contains more one bits than the bitmap of that,
          // so this can not possibly be a subset of that
          false
        }
      case _ =>
        // if the other set is a HashTrieSet but has less elements than this, it can not be a subset
        // if the other set is a HashSet1, we can not be a subset of it because we are a HashTrieSet with at least two children (see assertion)
        // if the other set is a HashSetCollision1, we can not be a subset of it because we are a HashTrieSet with at least two different hash codes
        // if the other set is the empty set, we are not a subset of it because we are not empty
        false
    })

    override protected def filter0(p: A => Boolean, negate: Boolean, level: Int, buffer: Array[HashSet[A]], offset0: Int): HashSet[A] = {
      // current offset
      var offset = offset0
      // result size
      var rs = 0
      // bitmap for kept elems
      var kept = 0
      // loop over all elements
      var i = 0
      while (i < elems.length) {
        val result = elems(i).filter0(p, negate, level + 5, buffer, offset)
        if (result ne null) {
          buffer(offset) = result
          offset += 1
          // add the result size
          rs += result.size
          // mark the bit i as kept
          kept |= (1 << i)
        }
        i += 1
      }
      if (offset == offset0) {
        // empty
        null
      } else if (rs == size) {
        // unchanged
        this
      } else if (offset == offset0 + 1 && !buffer(offset0).isInstanceOf[HashTrieSet[A]]) {
        // leaf
        buffer(offset0)
      } else {
        // we have to return a HashTrieSet
        val length = offset - offset0
        val elems1 = new Array[HashSet[A]](length)
        System.arraycopy(buffer, offset0, elems1, 0, length)
        val bitmap1 = if (length == elems.length) {
          // we can reuse the original bitmap
          bitmap
        } else {
          // calculate new bitmap by keeping just bits in the kept bitmask
          keepBits(bitmap, kept)
        }
        new HashTrieSet(bitmap1, elems1, rs)
      }
    }

    override def iterator = new TrieIterator[A](elems.asInstanceOf[Array[Iterable[A]]]) {
      final override def getElem(cc: AnyRef): A = cc.asInstanceOf[HashSet1[A]].key
    }

    override def foreach[U](f: A => U): Unit = {
      var i = 0
      while (i < elems.length) {
        elems(i).foreach(f)
        i += 1
      }
    }
  }
  protected def elemHashCode(key: Any) = key.##

  protected final def improve(hcode: Int) = {
    var h: Int = hcode + ~(hcode << 9)
    h = h ^ (h >>> 14)
    h = h + (h << 4)
    h ^ (h >>> 10)
  }

  private[collection] def computeHash(key: Any) = improve(elemHashCode(key))

  /**
   * Calculates the maximum buffer size given the maximum possible total size of the trie-based collection
   * @param size the maximum size of the collection to be generated
   * @return the maximum buffer size
   */
  @inline private def bufferSize(size: Int): Int = (size + 6) min (32 * 7)

  /**
   * In many internal operations the empty set is represented as null for performance reasons. This method converts
   * null to the empty set for use in public methods
   */
  @inline private def nullToEmpty[A](s: HashSet[A]): HashSet[A] = if (s eq null) empty[A] else s

  /**
   * Utility method to keep a subset of all bits in a given bitmap
   *
   * Example
   *    bitmap (binary): 00000001000000010000000100000001
   *    keep (binary):                               1010
   *    result (binary): 00000001000000000000000100000000
   *
   * @param bitmap the bitmap
   * @param keep a bitmask containing which bits to keep
   * @return the original bitmap with all bits where keep is not 1 set to 0
   */
  private def keepBits(bitmap: Int, keep: Int): Int = {
    var result = 0
    var current = bitmap
    var kept = keep
    while (kept != 0) {
      // lowest remaining bit in current
      val lsb = current ^ (current & (current - 1))
      if ((kept & 1) != 0) {
        // mark bit in result bitmap
        result |= lsb
      }
      // clear lowest remaining one bit in abm
      current &= ~lsb
      // look at the next kept bit
      kept >>>= 1
    }
    result
  }

  // unsigned comparison
  @inline private[this] def unsignedCompare(i: Int, j: Int) =
    (i < j) ^ (i < 0) ^ (j < 0)

  @SerialVersionUID(2L) private class SerializationProxy[A,B](@transient private var orig: HashSet[A]) extends Serializable {
    private def writeObject(out: java.io.ObjectOutputStream) {
      val s = orig.size
      out.writeInt(s)
      for (e <- orig) {
        out.writeObject(e)
      }
    }

    private def readObject(in: java.io.ObjectInputStream) {
      orig = empty
      val s = in.readInt()
      for (i <- 0 until s) {
        val e = in.readObject().asInstanceOf[A]
        orig = orig + e
      }
    }

    private def readResolve(): AnyRef = orig
  }


  /** Builder for HashSet.
   */
  private[collection] final class HashSetBuilder[A] extends mutable.ReusableBuilder[A, HashSet[A]] {


    /* Nodes in the tree are either regular HashSet1, HashTrieSet, HashSetCollision1, or a mutable HashTrieSet
      mutable HashTrieSet nodes are designated by having size == -1

      mutable HashTrieSet nodes can have child nodes that are mutable, or immutable
      immutable HashTrieSet child nodes can only be immutable

      mutable HashTrieSet elems are always a Array of size 32,size -1, bitmap -1
     */

    /** The root node of the partially build hashmap */
    private var rootNode: HashSet[A] = HashSet.empty

    private def isMutable(hs: HashSet[A]) = {
      hs.isInstanceOf[HashTrieSet[A]] && hs.size == -1
    }

    private def makeMutable(hs: HashTrieSet[A]): HashTrieSet[A] = {
      if (isMutable(hs)) hs
      else {
        val elems = new Array[HashSet[A]](32)
        var bit = 0
        var iBit = 0
        while (bit < 32) {
          if ((hs.bitmap & (1 << bit)) != 0) {
            elems(bit) = hs.elems(iBit)
            iBit += 1
          }
          bit += 1
        }
        new HashTrieSet[A](-1, elems, -1)
      }
    }

    private def makeImmutable(hs: HashSet[A]): HashSet[A] = {
      hs match {
        case trie: HashTrieSet[A] if isMutable(trie) =>
          var bit = 0
          var bitmap = 0
          var size = 0
          while (bit < 32) {
            if (trie.elems(bit) ne null)
              trie.elems(bit) = makeImmutable(trie.elems(bit))
            if (trie.elems(bit) ne null) {
              bitmap |= 1 << bit
              size += trie.elems(bit).size
            }
            bit += 1
          }
          Integer.bitCount(bitmap) match {
            case 0 => null
            case 1
              if trie.elems(Integer.numberOfTrailingZeros(bitmap)).isInstanceOf[LeafHashSet[A]] =>
              trie.elems(Integer.numberOfTrailingZeros(bitmap))

            case bc =>
              val elems = if (bc == 32) trie.elems else {
                val elems = new Array[HashSet[A]](bc)
                var oBit = 0
                bit = 0
                while (bit < 32) {
                  if (trie.elems(bit) ne null) {
                    elems(oBit) = trie.elems(bit)
                    oBit += 1
                  }
                  bit += 1
                }
                assert(oBit == bc)
                elems
              }
              trie.size0 = size
              trie.elems = elems
              trie.bitmap = bitmap
              trie
          }
        case _ => hs
      }
    }

    override def clear(): Unit = {
      rootNode match {
        case trie: HashTrieSet[A] if isMutable(trie) =>
          util.Arrays.fill(trie.elems.asInstanceOf[Array[AnyRef]], null)
        case _ => rootNode = HashSet.empty
      }
    }

    override def result(): HashSet[A] = {
      rootNode = nullToEmpty(makeImmutable(rootNode))
      VM.releaseFence()
      rootNode
    }


    override def +=(elem1: A, elem2: A, elems: A*): HashSetBuilder.this.type = {
      this += elem1
      this += elem2
      this ++= elems
    }

    override def +=(elem: A): HashSetBuilder.this.type = {
      val hash = computeHash(elem)
      rootNode = addOne(rootNode, elem, hash, 0)
      this
    }

    override def ++=(xs: TraversableOnce[A]): HashSetBuilder.this.type = xs match {
      case hs: HashSet[A] =>
        if (rootNode.isEmpty) {
          if (!hs.isEmpty)
            rootNode = hs
        } else
          rootNode = addHashSet(rootNode, hs, 0)
        this
      //      case hs: HashMap.HashMapKeys =>
      //      //TODO
      case hs: mutable.HashSet[A] =>
        //TODO
        super.++=(xs)
      case _ =>
        super.++=(xs)
    }

    def makeMutableTrie(aLeaf: LeafHashSet[A], bLeaf: LeafHashSet[A], level: Int): HashTrieSet[A] = {
      val elems = new Array[HashSet[A]](32)
      val aRawIndex = (aLeaf.hash >>> level) & 0x1f
      val bRawIndex = (bLeaf.hash >>> level) & 0x1f
      if (aRawIndex == bRawIndex) {
        elems(aRawIndex) = makeMutableTrie(aLeaf, bLeaf, level + 5)
      } else {
        elems(aRawIndex) = aLeaf
        elems(bRawIndex) = bLeaf
      }
      new HashTrieSet[A](-1, elems, -1)
    }

    def makeMutableTrie(leaf: LeafHashSet[A], elem: A, elemImprovedHash: Int, level: Int): HashTrieSet[A] = {
      makeMutableTrie(leaf, new HashSet1(elem, elemImprovedHash), level)
    }

    private def addOne(toNode: HashSet[A], elem: A, improvedHash: Int, level: Int): HashSet[A] = {
      toNode match {
        case leaf: LeafHashSet[A] =>
          if (leaf.hash == improvedHash)
            leaf.updated0(elem, improvedHash, level)
          else makeMutableTrie(leaf, elem, improvedHash, level)

        case trie: HashTrieSet[A] if isMutable((trie)) =>
          val arrayIndex = (improvedHash >>> level) & 0x1f
          val old = trie.elems(arrayIndex)
          trie.elems(arrayIndex) = if (old eq null) new HashSet1(elem, improvedHash)
          else addOne(old, elem, improvedHash, level + 5)
          trie

        case trie: HashTrieSet[A] =>
          val rawIndex = (improvedHash >>> level) & 0x1f
          val arrayIndex = compressedIndex(trie, rawIndex)
          if (arrayIndex == -1)
            addOne(makeMutable(trie), elem, improvedHash, level)
          else {
            val old = trie.elems(arrayIndex)
            val merged = if (old eq null) new HashSet1(elem, improvedHash)
            else addOne(old, elem, improvedHash, level + 5)

            if (merged eq old) trie
            else {
              val newMutableTrie = makeMutable(trie)
              newMutableTrie.elems(rawIndex) = merged
              newMutableTrie
            }
          }
        case empty if empty.isEmpty => toNode.updated0(elem, improvedHash, level)
      }
    }

    /** return the bit index of the rawIndex in the bitmap of the trie, or -1 if the bit is not in the bitmap */
    private def compressedIndex(trie: HashTrieSet[A], rawIndex: Int): Int = {
      if (trie.bitmap == -1) rawIndex
      else if ((trie.bitmap & (1 << rawIndex)) == 0) {
        //the value is not in this index
        -1
      } else {
        Integer.bitCount(((1 << rawIndex) - 1) & trie.bitmap)
      }
    }
    /** return the array index for the rawIndex, in the trie elem array
     * The trei may be mutable, or immutable
     * returns -1 if the trie is compressed and the index in not in the array */
    private def trieIndex(trie: HashTrieSet[A], rawIndex: Int): Int = {
      if (isMutable(trie) || trie.bitmap == -1) rawIndex
      else compressedIndex(trie, rawIndex)
    }

    private def addHashSet(toNode: HashSet[A], toBeAdded: HashSet[A], level: Int): HashSet[A] = {
      toNode match {
        case aLeaf: LeafHashSet[A] => addToLeafHashSet(aLeaf, toBeAdded, level)
        case trie: HashTrieSet[A] => addToTrieHashSet(trie, toBeAdded, level)
        case empty if empty.isEmpty => toNode
      }
    }

    private def addToTrieHashSet(toNode: HashTrieSet[A], toBeAdded: HashSet[A], level: Int): HashSet[A] = {
      if (toNode eq toBeAdded) toNode
      else toBeAdded match {
        case bLeaf: LeafHashSet[A] =>
          val rawIndex = (bLeaf.hash >>> level) & 0x1f
          val arrayIndex = trieIndex(toNode, rawIndex)
          if (arrayIndex == -1) {
            val newToNode = makeMutable(toNode)
            newToNode.elems(rawIndex) = toBeAdded
            newToNode
          } else {
            val old = toNode.elems(arrayIndex)
            if (old eq toBeAdded) toNode
            else if (old eq null) {
              assert(isMutable(toNode))
              toNode.elems(arrayIndex) = toBeAdded
              toNode
            } else {
              val result = addHashSet(old, toBeAdded, level + 5)
              if (result eq old) toNode
              else {
                val newToNode = makeMutable(toNode)
                newToNode.elems(rawIndex) = result
                newToNode
              }
            }
          }
        case bTrie: HashTrieSet[A] =>
          var result = toNode
          var bBitSet = bTrie.bitmap
          var bArrayIndex = 0
          while (bBitSet != 0) {
            val bValue = bTrie.elems(bArrayIndex)
            val rawIndex = Integer.numberOfTrailingZeros(bBitSet)
            val aArrayIndex = trieIndex(result, rawIndex)
            if (aArrayIndex == -1) {
              result = makeMutable(result)
              result.elems(rawIndex) = bValue
            } else {
              val aValue = result.elems(aArrayIndex)
              if (aValue ne bValue) {
                if (aValue eq null) {
                  assert(isMutable(result))
                  result.elems(rawIndex) = bValue
                } else {
                  val resultAtIndex = addHashSet(aValue, bValue, level + 5)
                  if (resultAtIndex ne aValue) {
                    result = makeMutable(result)
                    result.elems(rawIndex) = resultAtIndex
                  }
                }
              }
            }
            bBitSet ^= 1 << rawIndex
            bArrayIndex += 1
          }
          result
        case empty if empty.isEmpty => toNode
      }
    }
    private def addToLeafHashSet(toNode: LeafHashSet[A], toBeAdded: HashSet[A], level: Int): HashSet[A] = {
      if (toNode eq toBeAdded) toNode
      else toBeAdded match {
        case bLeaf: LeafHashSet[A] =>
          if (toNode.hash == bLeaf.hash) toNode.union0(bLeaf, level)
          else makeMutableTrie(toNode, bLeaf, level)
        case bTrie: HashTrieSet[A] =>
          val rawIndex = (toNode.hash >>> level) & 0x1f
          val arrayIndex = compressedIndex(bTrie, rawIndex)
          if (arrayIndex == -1) {
            val result = makeMutable(bTrie)
            result.elems(rawIndex) = toNode
            result
          } else {
            val newEle = addToLeafHashSet(toNode, bTrie.elems(arrayIndex), level + 5)
            if (newEle eq toBeAdded)
              toBeAdded
            else {
              val result = makeMutable(bTrie)
              result.elems(rawIndex) = newEle
              result
            }
          }
        case empty if empty.isEmpty =>
          toNode
      }
    }
  }
}
