/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection
package immutable

import annotation.unchecked.uncheckedVariance

object TrieIteratorBase {
  final val TRIE_TYPE = 0
  final val CONTAINER_TYPE = 1
  final val COLLISION_TYPE = 2
}
import TrieIteratorBase._

private[immutable] abstract class TrieIteratorBase[+T, CC >: Null <: Iterable[T]](elems: Array[CC]) extends Iterator[T] {
  private[immutable] def recreateIterator(): This

  // Since we can't match on abstract types, we call determineType to
  // find out what it is and let the casting gods do the remainder.
  private implicit def fixCC[U <: CC](x: CC): U = x.asInstanceOf[U]

  protected var depth                = 0
  protected var arrayStack           = newDeepArray(6)
  protected var posStack             = new Array[Int](6)
  protected var arrayD               = elems
  protected var posD                 = 0
  protected var subIter: Iterator[T @uncheckedVariance] = null // to traverse collision nodes

  private[immutable] type TrieType <: CC
  private[immutable] type ContainerType <: CC
  private[immutable] type CollisionType <: CC

  // Returns one of the constants defined in TrieIteratorBase to determine type.
  private[immutable] def determineType(x: CC): Int
  private[immutable] def getElem(cc: ContainerType): T
  private[immutable] def getElems(t: TrieType): Array[CC]
  private[immutable] def collisionToArray(c: CollisionType): Array[CC]
  private[immutable] def newThisType(xs: Array[CC]): Iterator[T]
  private[immutable] def newDeepArray(size: Int): Array[Array[CC]]
  private[immutable] def newSingleArray(el: CC): Array[CC]

  protected type This <: TrieIteratorBase[T, CC]
  private type SplitIterators = ((Iterator[T], Int), Iterator[T])

  def dupIterator: This = {
    val t = recreateIterator()

    t.depth      = depth
    t.arrayStack = arrayStack
    t.posStack   = posStack
    t.arrayD     = arrayD
    t.posD       = posD
    t.subIter    = subIter

    t
  }

  private def iteratorWithSize(arr: Array[CC]): (Iterator[T], Int) =
    (newThisType(arr), arr map (_.size) sum)

  private def arrayToIterators(arr: Array[CC]): SplitIterators = {
    val (fst, snd) = arr.splitAt(arr.length / 2)

    (iteratorWithSize(snd), newThisType(fst))
  }
  private def splitArray(ad: Array[CC]): SplitIterators =
    if (ad.length > 1) arrayToIterators(ad)
    else determineType(ad(0)) match {
      case COLLISION_TYPE => arrayToIterators(collisionToArray(ad(0)))
      case TRIE_TYPE      => splitArray(getElems(ad(0)))
    }

  def hasNext = (subIter ne null) || depth >= 0
  def next: T = {
    if (subIter ne null) {
      val el = subIter.next
      if (!subIter.hasNext)
        subIter = null
      el
    } else
      next0(arrayD, posD)
  }

  @scala.annotation.tailrec private[this] def next0(elems: Array[CC], i: Int): T = {
    if (i == elems.length-1) { // reached end of level, pop stack
      depth -= 1
      if (depth >= 0) {
        arrayD = arrayStack(depth)
        posD = posStack(depth)
        arrayStack(depth) = null
      } else {
        arrayD = null
        posD = 0
      }
    } else
      posD += 1

    val m = elems(i)
    determineType(m) match {
      case TRIE_TYPE =>
        if (depth >= 0) {
          arrayStack(depth) = arrayD
          posStack(depth) = posD
        }
        depth += 1
        arrayD = getElems(m)
        posD = 0
        next0(getElems(m), 0)
      case CONTAINER_TYPE =>
        getElem(m)   // push current pos onto stack and descend
      case _       =>
        subIter = m.iterator
        next
    }
  }

  // assumption: contains 2 or more elements
  // splits this iterator into 2 iterators
  // returns the 1st iterator, its number of elements, and the second iterator
  def split: SplitIterators = {
    // 0) simple case: no elements have been iterated - simply divide arrayD
    if (arrayD != null && depth == 0 && posD == 0)
      return splitArray(arrayD)

    // otherwise, some elements have been iterated over
    // 1) collision case: if we have a subIter, we return subIter and elements after it
    if (subIter ne null) {
      val buff = subIter.toBuffer
      subIter = null
      ((buff.iterator, buff.length), this)
    }
    else {
      // otherwise find the topmost array stack element
      if (depth > 0) {
        // 2) topmost comes before (is not) arrayD
        //    steal a portion of top to create a new iterator
        val topmost = arrayStack(0)
        if (posStack(0) == arrayStack(0).length - 1) {
          // 2a) only a single entry left on top
          // this means we have to modify this iterator - pop topmost
          val snd = newSingleArray(arrayStack(0).last)
          val szsnd = snd(0).size
          // modify this - pop
          depth -= 1
          1 until arrayStack.length foreach (i => arrayStack(i - 1) = arrayStack(i))
          arrayStack(arrayStack.length - 1) = newSingleArray(null)
          posStack = posStack.tail ++ Array[Int](0)
          // we know that `this` is not empty, since it had something on the arrayStack and arrayStack elements are always non-empty
          ((newThisType(snd), szsnd), this)
        } else {
          // 2b) more than a single entry left on top
          val (fst, snd) = arrayStack(0).splitAt(arrayStack(0).length - (arrayStack(0).length - posStack(0) + 1) / 2)
          arrayStack(0) = fst
          (iteratorWithSize(snd), this)
        }
      } else {
        // 3) no topmost element (arrayD is at the top)
        //    steal a portion of it and update this iterator
        if (posD == arrayD.length - 1) {
          // 3a) positioned at the last element of arrayD
          val m = arrayD(posD)
          val arr: Array[CC] = determineType(m) match {
            case COLLISION_TYPE => collisionToArray(m)
            case TRIE_TYPE      => getElems(m)
            case _              => error("cannot divide single element")
          }
          arrayToIterators(arr)
        }
        else {
          // 3b) arrayD has more free elements
          val (fst, snd) = arrayD.splitAt(arrayD.length - (arrayD.length - posD + 1) / 2)
          arrayD = fst
          (iteratorWithSize(snd), this)
        }
      }
    }
  }
}