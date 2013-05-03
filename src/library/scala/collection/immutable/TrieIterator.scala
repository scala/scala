/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection
package immutable

import HashMap.{ HashTrieMap, HashMapCollision1, HashMap1 }
import HashSet.{ HashTrieSet, HashSetCollision1, HashSet1 }
import scala.annotation.unchecked.{ uncheckedVariance => uV }
import scala.annotation.tailrec

/** Abandons any pretense of type safety for speed.  You can't say I
 *  didn't try: see r23934.
 */
private[collection] abstract class TrieIterator[+T](elems: Array[Iterable[T]]) extends AbstractIterator[T] {
  outer =>

  private[immutable] def getElem(x: AnyRef): T

  def initDepth                                     = 0
  def initArrayStack: Array[Array[Iterable[T @uV]]] = new Array[Array[Iterable[T]]](6)
  def initPosStack                                  = new Array[Int](6)
  def initArrayD: Array[Iterable[T @uV]]            = elems
  def initPosD                                      = 0
  def initSubIter: Iterator[T]                      = null // to traverse collision nodes

  private[this] var depth                                     = initDepth
  private[this] var arrayStack: Array[Array[Iterable[T @uV]]] = initArrayStack
  private[this] var posStack                                  = initPosStack
  private[this] var arrayD: Array[Iterable[T @uV]]            = initArrayD
  private[this] var posD                                      = initPosD
  private[this] var subIter                                   = initSubIter

  private[this] def getElems(x: Iterable[T]): Array[Iterable[T]] = (x match {
    case x: HashTrieMap[_, _] => x.elems
    case x: HashTrieSet[_]    => x.elems
  }).asInstanceOf[Array[Iterable[T]]]

  private[this] def collisionToArray(x: Iterable[T]): Array[Iterable[T]] = (x match {
    case x: HashMapCollision1[_, _] => x.kvs.map(x => HashMap(x)).toArray
    case x: HashSetCollision1[_]    => x.ks.map(x => HashSet(x)).toArray
  }).asInstanceOf[Array[Iterable[T]]]

  private[this] type SplitIterators = ((Iterator[T], Int), Iterator[T])

  private def isTrie(x: AnyRef) = x match {
    case _: HashTrieMap[_,_] | _: HashTrieSet[_] => true
    case _                                       => false
  }
  private def isContainer(x: AnyRef) = x match {
    case _: HashMap1[_, _] | _: HashSet1[_] => true
    case _                                  => false
  }

  final class DupIterator(xs: Array[Iterable[T]]) extends {
    override val initDepth                                     = outer.depth
    override val initArrayStack: Array[Array[Iterable[T @uV]]] = outer.arrayStack
    override val initPosStack                                  = outer.posStack
    override val initArrayD: Array[Iterable[T @uV]]            = outer.arrayD
    override val initPosD                                      = outer.posD
    override val initSubIter                                   = outer.subIter
  } with TrieIterator[T](xs) {
    final override def getElem(x: AnyRef): T = outer.getElem(x)
  }

  def dupIterator: TrieIterator[T] = new DupIterator(elems)

  private[this] def newIterator(xs: Array[Iterable[T]]) = new TrieIterator(xs) {
    final override def getElem(x: AnyRef): T = outer.getElem(x)
  }

  private[this] def iteratorWithSize(arr: Array[Iterable[T]]): (Iterator[T], Int) =
    (newIterator(arr), arr.map(_.size).sum)

  private[this] def arrayToIterators(arr: Array[Iterable[T]]): SplitIterators = {
    val (fst, snd) = arr.splitAt(arr.length / 2)

    (iteratorWithSize(snd), newIterator(fst))
  }
  private[this] def splitArray(ad: Array[Iterable[T]]): SplitIterators =
    if (ad.length > 1) arrayToIterators(ad)
    else ad(0) match {
      case _: HashMapCollision1[_, _] | _: HashSetCollision1[_] =>
        arrayToIterators(collisionToArray(ad(0)))
      case _ =>
        splitArray(getElems(ad(0)))
    }

  def hasNext = (subIter ne null) || depth >= 0
  def next(): T = {
    if (subIter ne null) {
      val el = subIter.next()
      if (!subIter.hasNext)
        subIter = null
      el
    } else
      next0(arrayD, posD)
  }

  @tailrec private[this] def next0(elems: Array[Iterable[T]], i: Int): T = {
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

    // Note: this block is over twice as fast written this way as it is
    // as a pattern match.  Haven't started looking into why that is, but
    // it's pretty sad the pattern matcher is that much slower.
    if (isContainer(m))
      getElem(m) // push current pos onto stack and descend
    else if (isTrie(m)) {
      if (depth >= 0) {
        arrayStack(depth) = arrayD
        posStack(depth) = posD
      }
      depth += 1
      arrayD = getElems(m)
      posD = 0
      next0(getElems(m), 0)
    }
    else {
      subIter = m.iterator
      next()
    }
    // The much slower version:
    //
    // m match {
    //   case _: HashMap1[_, _] | _: HashSet1[_] =>
    //     getElem(m) // push current pos onto stack and descend
    //   case _: HashTrieMap[_,_] | _: HashTrieSet[_] =>
    //     if (depth >= 0) {
    //       arrayStack(depth) = arrayD
    //       posStack(depth) = posD
    //     }
    //     depth += 1
    //     arrayD = getElems(m)
    //     posD = 0
    //     next0(getElems(m), 0)
    //   case _ =>
    //     subIter = m.iterator
    //     next
    // }
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
        if (posStack(0) == arrayStack(0).length - 1) {
          // 2a) only a single entry left on top
          // this means we have to modify this iterator - pop topmost
          val snd = Array[Iterable[T]](arrayStack(0).last)
          val szsnd = snd(0).size
          // modify this - pop
          depth -= 1
          1 until arrayStack.length foreach (i => arrayStack(i - 1) = arrayStack(i))
          arrayStack(arrayStack.length - 1) = Array[Iterable[T]](null)
          posStack = posStack.tail ++ Array[Int](0)
          // we know that `this` is not empty, since it had something on the arrayStack and arrayStack elements are always non-empty
          ((newIterator(snd), szsnd), this)
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
          arrayToIterators(
            if (isTrie(m)) getElems(m)
            else collisionToArray(m)
          )
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
