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

import scala.annotation.unchecked.uncheckedVariance
import scala.annotation.tailrec
import mutable.{Builder, ListBuffer}
import scala.collection.generic.{CommonErrors, DefaultSerializable}
import scala.runtime.Statics.releaseFence

/** A class for immutable linked lists representing ordered collections
  *  of elements of type `A`.
  *
  *  This class comes with two implementing case classes `scala.Nil`
  *  and `scala.::` that implement the abstract members `isEmpty`,
  *  `head` and `tail`.
  *
  *  This class is optimal for last-in-first-out (LIFO), stack-like access patterns. If you need another access
  *  pattern, for example, random access or FIFO, consider using a collection more suited to this than `List`.
  *
  *  ==Performance==
  *  '''Time:''' `List` has `O(1)` prepend and head/tail access. Most other operations are `O(n)` on the number of elements in the list.
  *  This includes the index-based lookup of elements, `length`, `append` and `reverse`.
  *
  *  '''Space:''' `List` implements '''structural sharing''' of the tail list. This means that many operations are either
  *  zero- or constant-memory cost.
  *  {{{
  *  val mainList = List(3, 2, 1)
  *  val with4 =    4 :: mainList  // re-uses mainList, costs one :: instance
  *  val with42 =   42 :: mainList // also re-uses mainList, cost one :: instance
  *  val shorter =  mainList.tail  // costs nothing as it uses the same 2::1::Nil instances as mainList
  *  }}}
  *
  *  @example {{{
  *  // Make a list via the companion object factory
  *  val days = List("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  *
  *  // Make a list element-by-element
  *  val when = "AM" :: "PM" :: Nil
  *
  *  // Pattern match
  *  days match {
  *    case firstDay :: otherDays =>
  *      println("The first day of the week is: " + firstDay)
  *    case Nil =>
  *      println("There don't seem to be any week days.")
  *  }
  *  }}}
  *
  *  @note The functional list is characterized by persistence and structural sharing, thus offering considerable
  *        performance and space consumption benefits in some scenarios if used correctly.
  *        However, note that objects having multiple references into the same functional list (that is,
  *        objects that rely on structural sharing), will be serialized and deserialized with multiple lists, one for
  *        each reference to it. I.e. structural sharing is lost after serialization/deserialization.
  *
  *  @see  [[https://docs.scala-lang.org/overviews/collections-2.13/concrete-immutable-collection-classes.html#lists "Scala's Collection Library overview"]]
  *  section on `Lists` for more information.
  *
  *  @define coll list
  *  @define Coll `List`
  *  @define orderDependent
  *  @define orderDependentFold
  *  @define mayNotTerminateInf
  *  @define willNotTerminateInf
  */
@SerialVersionUID(3L)
sealed abstract class List[+A]
  extends AbstractSeq[A]
    with LinearSeq[A]
    with LinearSeqOps[A, List, List[A]]
    with StrictOptimizedLinearSeqOps[A, List, List[A]]
    with StrictOptimizedSeqOps[A, List, List[A]]
    with IterableFactoryDefaults[A, List]
    with DefaultSerializable {

  override def iterableFactory: SeqFactory[List] = List

  /** Adds an element at the beginning of this list.
    *  @param elem the element to prepend.
    *  @return  a list which contains `x` as first element and
    *           which continues with this list.
    *  Example:
    *  {{{1 :: List(2, 3) = List(2, 3).::(1) = List(1, 2, 3)}}}
    */
  def :: [B >: A](elem: B): List[B] =  new ::(elem, this)

  /** Adds the elements of a given list in front of this list.
    *
    * Example:
    * {{{List(1, 2) ::: List(3, 4) = List(3, 4).:::(List(1, 2)) = List(1, 2, 3, 4)}}}
    *
    *  @param prefix  The list elements to prepend.
    *  @return a list resulting from the concatenation of the given
    *    list `prefix` and this list.
    */
  def ::: [B >: A](prefix: List[B]): List[B] =
    if (isEmpty) prefix
    else if (prefix.isEmpty) this
    else {
      val result = new ::[B](prefix.head, this)
      var curr = result
      var that = prefix.tail
      while (!that.isEmpty) {
        val temp = new ::[B](that.head, this)
        curr.next = temp
        curr = temp
        that = that.tail
      }
      releaseFence()
      result
    }

  /** Adds the elements of a given list in reverse order in front of this list.
    *  `xs reverse_::: ys` is equivalent to
    *  `xs.reverse ::: ys` but is more efficient.
    *
    *  @param prefix the prefix to reverse and then prepend
    *  @return       the concatenation of the reversed prefix and the current list.
    */
  def reverse_:::[B >: A](prefix: List[B]): List[B] = {
    var these: List[B] = this
    var pres = prefix
    while (!pres.isEmpty) {
      these = pres.head :: these
      pres = pres.tail
    }
    these
  }

  override final def isEmpty: Boolean = this eq Nil

  override def prepended[B >: A](elem: B): List[B] = elem :: this

  override def prependedAll[B >: A](prefix: collection.IterableOnce[B]): List[B] = prefix match {
    case xs: List[B] => xs ::: this
    case _ if prefix.knownSize == 0 => this
    case b: ListBuffer[B] if this.isEmpty => b.toList
    case _ =>
      val iter = prefix.iterator
      if (iter.hasNext) {
        val result = new ::[B](iter.next(), this)
        var curr = result
        while (iter.hasNext) {
          val temp = new ::[B](iter.next(), this)
          curr.next = temp
          curr = temp
        }
        releaseFence()
        result
      } else {
        this
      }
  }

  // When calling appendAll with another list `suffix`, avoid copying `suffix`
  override def appendedAll[B >: A](suffix: collection.IterableOnce[B]): List[B] = suffix match {
    case xs: List[B] => this ::: xs
    case _ => super.appendedAll(suffix)
  }

  override def take(n: Int): List[A] = if (isEmpty || n <= 0) Nil else {
    val h = new ::(head, Nil)
    var t = h
    var rest = tail
    var i = 1
    while ({if (rest.isEmpty) return this; i < n}) {
      i += 1
      val nx = new ::(rest.head, Nil)
      t.next = nx
      t = nx
      rest = rest.tail
    }
    releaseFence()
    h
  }

  /** @inheritdoc
    *
    *  @example {{{
    *  // Given a list
    *  val letters = List('a','b','c','d','e')
    *
    *  // `slice` returns all elements beginning at index `from` and afterwards,
    *  // up until index `until` (excluding index `until`.)
    *  letters.slice(1,3) // Returns List('b','c')
    *  }}}
    */
  override def slice(from: Int, until: Int): List[A] = {
    val lo = scala.math.max(from, 0)
    if (until <= lo || isEmpty) Nil
    else this drop lo take (until - lo)
  }

  override def takeRight(n: Int): List[A] = {
    @tailrec
    def loop(lead: List[A], lag: List[A]): List[A] = lead match {
      case Nil => lag
      case _ :: tail => loop(tail, lag.tail)
    }
    loop(drop(n), this)
  }

  // dropRight is inherited from LinearSeq

  override def splitAt(n: Int): (List[A], List[A]) = {
    val b = new ListBuffer[A]
    var i = 0
    var these = this
    while (!these.isEmpty && i < n) {
      i += 1
      b += these.head
      these = these.tail
    }
    (b.toList, these)
  }

  override def updated[B >: A](index: Int, elem: B): List[B] = {
    var i = 0
    var current = this
    val prefix = ListBuffer.empty[B]
    while (i < index && current.nonEmpty) {
      i += 1
      prefix += current.head
      current = current.tail
    }
    if (i == index && current.nonEmpty) {
      prefix.prependToList(elem :: current.tail)
    } else {
      throw CommonErrors.indexOutOfBounds(index = index, max = length - 1)
    }
  }

  final override def map[B](f: A => B): List[B] = {
    if (this eq Nil) Nil else {
      val h = new ::[B](f(head), Nil)
      var t: ::[B] = h
      var rest = tail
      while (rest ne Nil) {
        val nx = new ::(f(rest.head), Nil)
        t.next = nx
        t = nx
        rest = rest.tail
      }
      releaseFence()
      h
    }
  }

  final override def collect[B](pf: PartialFunction[A, B]): List[B] = {
    if (this eq Nil) Nil else {
      var rest = this
      var h: ::[B] = null
      var x: Any = null
      // Special case for first element
      while (h eq null) {
        x = pf.applyOrElse(rest.head, List.partialNotApplied)
        if (x.asInstanceOf[AnyRef] ne List.partialNotApplied) h = new ::(x.asInstanceOf[B], Nil)
        rest = rest.tail
        if (rest eq Nil) return if (h eq null) Nil else h
      }
      var t = h
      // Remaining elements
      while (rest ne Nil) {
        x = pf.applyOrElse(rest.head, List.partialNotApplied)
        if (x.asInstanceOf[AnyRef] ne List.partialNotApplied) {
          val nx = new ::(x.asInstanceOf[B], Nil)
          t.next = nx
          t = nx
        }
        rest = rest.tail
      }
      releaseFence()
      h
    }
  }

  final override def flatMap[B](f: A => IterableOnce[B]): List[B] = {
    var rest = this
    var h: ::[B] = null
    var t: ::[B] = null
    while (rest ne Nil) {
      val it = f(rest.head).iterator
      while (it.hasNext) {
        val nx = new ::(it.next(), Nil)
        if (t eq null) {
          h = nx
        } else {
          t.next = nx
        }
        t = nx
      }
      rest = rest.tail
    }
    if (h eq null) Nil else {releaseFence(); h}
  }

  @inline final override def takeWhile(p: A => Boolean): List[A] = {
    val b = new ListBuffer[A]
    var these = this
    while (!these.isEmpty && p(these.head)) {
      b += these.head
      these = these.tail
    }
    b.toList
  }

  @inline final override def span(p: A => Boolean): (List[A], List[A]) = {
    val b = new ListBuffer[A]
    var these = this
    while (!these.isEmpty && p(these.head)) {
      b += these.head
      these = these.tail
    }
    (b.toList, these)
  }

  // Overridden with an implementation identical to the inherited one (at this time)
  // solely so it can be finalized and thus inlinable.
  @inline final override def foreach[U](f: A => U): Unit = {
    var these = this
    while (!these.isEmpty) {
      f(these.head)
      these = these.tail
    }
  }

  final override def reverse: List[A] = {
    var result: List[A] = Nil
    var these = this
    while (!these.isEmpty) {
      result = these.head :: result
      these = these.tail
    }
    result
  }

  final override def foldRight[B](z: B)(op: (A, B) => B): B = {
    var acc = z
    var these: List[A] = reverse
    while (!these.isEmpty) {
      acc = op(these.head, acc)
      these = these.tail
    }
    acc
  }

  // Copy/Paste overrides to avoid interface calls inside loops.

  override final def length: Int = {
    var these = this
    var len = 0
    while (!these.isEmpty) {
      len += 1
      these = these.tail
    }
    len
  }

  override final def lengthCompare(len: Int): Int = {
    @tailrec def loop(i: Int, xs: List[A]): Int = {
      if (i == len)
        if (xs.isEmpty) 0 else 1
      else if (xs.isEmpty)
        -1
      else
        loop(i + 1, xs.tail)
    }
    if (len < 0) 1
    else loop(0, coll)
  }

  override final def forall(p: A => Boolean): Boolean = {
    var these: List[A] = this
    while (!these.isEmpty) {
      if (!p(these.head)) return false
      these = these.tail
    }
    true
  }

  override final def exists(p: A => Boolean): Boolean = {
    var these: List[A] = this
    while (!these.isEmpty) {
      if (p(these.head)) return true
      these = these.tail
    }
    false
  }

  override final def contains[A1 >: A](elem: A1): Boolean = {
    var these: List[A] = this
    while (!these.isEmpty) {
      if (these.head == elem) return true
      these = these.tail
    }
    false
  }

  override final def find(p: A => Boolean): Option[A] = {
    var these: List[A] = this
    while (!these.isEmpty) {
      if (p(these.head)) return Some(these.head)
      these = these.tail
    }
    None
  }

  override def last: A = {
    if (isEmpty) throw new NoSuchElementException("List.last")
    else {
      var these = this
      var scout = tail
      while (!scout.isEmpty) {
        these = scout
        scout = scout.tail
      }
      these.head
    }
  }

  override def corresponds[B](that: collection.Seq[B])(p: (A, B) => Boolean): Boolean = that match {
    case that: LinearSeq[B] =>
      var i = this
      var j = that
      while (!(i.isEmpty || j.isEmpty)) {
        if (!p(i.head, j.head))
          return false
        i = i.tail
        j = j.tail
      }
      i.isEmpty && j.isEmpty
    case _ =>
      super.corresponds(that)(p)
  }

  override protected[this] def className = "List"

  /** Builds a new list by applying a function to all elements of this list.
    *  Like `xs map f`, but returns `xs` unchanged if function
    *  `f` maps all elements to themselves (as determined by `eq`).
    *
    *  @param f      the function to apply to each element.
    *  @tparam B     the element type of the returned collection.
    *  @return       a list resulting from applying the given function
    *                `f` to each element of this list and collecting the results.
    */
  @`inline` final def mapConserve[B >: A <: AnyRef](f: A => B): List[B] = {
    // Note to developers: there exists a duplication between this function and `reflect.internal.util.Collections#map2Conserve`.
    // If any successful optimization attempts or other changes are made, please rehash them there too.
    @tailrec
    def loop(mappedHead: List[B], mappedLast: ::[B], unchanged: List[A], pending: List[A]): List[B] = {
      if (pending.isEmpty) {
        if (mappedHead eq null) unchanged
        else {
          mappedLast.next = (unchanged: List[B])
          mappedHead
        }
      }
      else {
        val head0 = pending.head
        val head1 = f(head0)

        if (head1 eq head0.asInstanceOf[AnyRef])
          loop(mappedHead, mappedLast, unchanged, pending.tail)
        else {
          var xc = unchanged
          var mappedHead1: List[B] = mappedHead
          var mappedLast1: ::[B] = mappedLast
          while (xc ne pending) {
            val next = new ::[B](xc.head, Nil)
            if (mappedHead1 eq null) mappedHead1 = next
            if (mappedLast1 ne null) mappedLast1.next = next
            mappedLast1 = next
            xc = xc.tail
          }
          val next = new ::(head1, Nil)
          if (mappedHead1 eq null) mappedHead1 = next
          if (mappedLast1 ne null) mappedLast1.next = next
          mappedLast1 = next
          val tail0 = pending.tail
          loop(mappedHead1, mappedLast1, tail0, tail0)

        }
      }
    }
    val result = loop(null, null, this, this)
    releaseFence()
    result
  }

  override def filter(p: A => Boolean): List[A] = filterCommon(p, isFlipped = false)

  override def filterNot(p: A => Boolean): List[A] = filterCommon(p, isFlipped = true)

  private[this] def filterCommon(p: A => Boolean, isFlipped: Boolean): List[A] = {

    // everything seen so far so far is not included
    @tailrec def noneIn(l: List[A]): List[A] = {
      if (l.isEmpty)
        Nil
      else {
        val h = l.head
        val t = l.tail
        if (p(h) != isFlipped)
          allIn(l, t)
        else
          noneIn(t)
      }
    }

    // everything from 'start' is included, if everything from this point is in we can return the origin
    // start otherwise if we discover an element that is out we must create a new partial list.
    @tailrec def allIn(start: List[A], remaining: List[A]): List[A] = {
      if (remaining.isEmpty)
        start
      else {
        val x = remaining.head
        if (p(x) != isFlipped)
          allIn(start, remaining.tail)
        else
          partialFill(start, remaining)
      }
    }

    // we have seen elements that should be included then one that should be excluded, start building
    def partialFill(origStart: List[A], firstMiss: List[A]): List[A] = {
      val newHead = new ::(origStart.head, Nil)
      var toProcess = origStart.tail
      var currentLast = newHead

      // we know that all elements are :: until at least firstMiss.tail
      while (!(toProcess eq firstMiss)) {
        val newElem = new ::(toProcess.head, Nil)
        currentLast.next = newElem
        currentLast = newElem
        toProcess = toProcess.tail
      }

      // at this point newHead points to a list which is a duplicate of all the 'in' elements up to the first miss.
      // currentLast is the last element in that list.

      // now we are going to try and share as much of the tail as we can, only moving elements across when we have to.
      var next = firstMiss.tail
      var nextToCopy = next // the next element we would need to copy to our list if we cant share.
      while (!next.isEmpty) {
        // generally recommended is next.isNonEmpty but this incurs an extra method call.
        val head: A = next.head
        if (p(head) != isFlipped) {
          next = next.tail
        } else {
          // its not a match - do we have outstanding elements?
          while (!(nextToCopy eq next)) {
            val newElem = new ::(nextToCopy.head, Nil)
            currentLast.next = newElem
            currentLast = newElem
            nextToCopy = nextToCopy.tail
          }
          nextToCopy = next.tail
          next = next.tail
        }
      }

      // we have remaining elements - they are unchanged attach them to the end
      if (!nextToCopy.isEmpty)
        currentLast.next = nextToCopy

      newHead
    }

    val result = noneIn(this)
    releaseFence()
    result
  }

  override def partition(p: A => Boolean): (List[A], List[A]) = {
    if (isEmpty) List.TupleOfNil
    else super.partition(p) match {
      case (Nil, xs) => (Nil, this)
      case (xs, Nil) => (this, Nil)
      case pair => pair
    }
  }

  final override def toList: List[A] = this

  // Override for performance
  override def equals(o: scala.Any): Boolean = {
    @tailrec def listEq(a: List[_], b: List[_]): Boolean =
      (a eq b) || {
        val aEmpty = a.isEmpty
        val bEmpty = b.isEmpty
        if (!(aEmpty || bEmpty) && a.head == b.head) {
          listEq(a.tail, b.tail)
        }
        else {
          aEmpty && bEmpty
        }
      }

    o match {
      case that: List[_] => listEq(this, that)
      case _ => super.equals(o)
    }
  }

  // TODO: uncomment once bincompat allows (reference: scala/scala#9365)
  /*
  // Override for performance: traverse only as much as needed
  // and share tail when nothing needs to be filtered out anymore
  override def diff[B >: A](that: collection.Seq[B]): AnyRef = {
    if (that.isEmpty || this.isEmpty) this
    else if (tail.isEmpty) if (that.contains(head)) Nil else this
    else {
      val occ = occCounts(that)
      val b = new ListBuffer[A]()
      @tailrec
      def rec(remainder: List[A]): List[A] = {
        if(occ.isEmpty) b.prependToList(remainder)
        else remainder match {
          case Nil => b.result()
          case head :: next => {
            occ.updateWith(head){
              case None => {
                b.append(head)
                None
              }
              case Some(1) => None
              case Some(n) => Some(n - 1)
            }
            rec(next)
          }
        }
      }
      rec(this)
    }
  }
  */

}

// Internal code that mutates `next` _must_ call `Statics.releaseFence()` if either immediately, or
// before a newly-allocated, thread-local :: instance is aliased (e.g. in ListBuffer.toList)
final case class :: [+A](override val head: A, private[scala] var next: List[A @uncheckedVariance]) // sound because `next` is used only locally
  extends List[A] {
  releaseFence()
  override def headOption: Some[A] = Some(head)
  override def tail: List[A] = next
}

case object Nil extends List[Nothing] {
  override def head: Nothing = throw new NoSuchElementException("head of empty list")
  override def headOption: None.type = None
  override def tail: Nothing = throw new UnsupportedOperationException("tail of empty list")
  override def last: Nothing = throw new NoSuchElementException("last of empty list")
  override def init: Nothing = throw new UnsupportedOperationException("init of empty list")
  override def knownSize: Int = 0
  override def iterator: Iterator[Nothing] = Iterator.empty
  override def unzip[A1, A2](implicit asPair: Nothing => (A1, A2)): (List[A1], List[A2]) = EmptyUnzip

  @transient
  private[this] val EmptyUnzip = (Nil, Nil)
}

/**
  * $factoryInfo
  * @define coll list
  * @define Coll `List`
  */
@SerialVersionUID(3L)
object List extends StrictOptimizedSeqFactory[List] {
  private val TupleOfNil = (Nil, Nil)

  def from[B](coll: collection.IterableOnce[B]): List[B] = Nil.prependedAll(coll)

  def newBuilder[A]: Builder[A, List[A]] = new ListBuffer()

  def empty[A]: List[A] = Nil

  @transient
  private[collection] val partialNotApplied = new Function1[Any, Any] { def apply(x: Any): Any = this }
}
