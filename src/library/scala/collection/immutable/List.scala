package scala
package collection
package immutable

import java.io.{ObjectInputStream, ObjectOutputStream}

import scala.annotation.unchecked.uncheckedVariance
import scala.annotation.tailrec
import mutable.{Builder, ListBuffer, ReusableBuilder}

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
  *  $usesMutableState
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
  *  @author  Martin Odersky and others
  *  @version 2.8
  *  @since   1.0
  *  @see  [[http://docs.scala-lang.org/overviews/collections/concrete-immutable-collection-classes.html#lists "Scala's Collection Library overview"]]
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
    with StrictOptimizedSeqOps[A, List, List[A]]
    with Serializable {

  override def iterableFactory: SeqFactory[List] = List

  /** Adds an element at the beginning of this list.
    *  @param elem the element to prepend.
    *  @return  a list which contains `x` as first element and
    *           which continues with this list.
    *
    *  @usecase def ::(elem: A): List[A]
    *    @inheritdoc
    *
    *    Example:
    *    {{{1 :: List(2, 3) = List(2, 3).::(1) = List(1, 2, 3)}}}
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
    else (new ListBuffer[B] ++= prefix).prependToList(this)

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

  override def prepended[B >: A](elem: B): List[B] = elem :: this

  // When calling prependAll with another list `prefix`, avoid copying `this`
  override def prependedAll[B >: A](prefix: collection.Iterable[B]): List[B] = prefix match {
    case xs: List[B] => xs ::: this
    case _ => super.prependedAll(prefix)
  }

  // When calling appendAll with another list `suffix`, avoid copying `suffix`
  override def appendedAll[B >: A](suffix: collection.Iterable[B]): List[B] = suffix match {
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
    h
  }

  /**
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
    var prefix = ListBuffer.empty[B]
    while (i < index && current.nonEmpty) {
      i += 1
      prefix += current.head
      current = current.tail
    }
    if (i == index && current.nonEmpty) {
      prefix.prependToList(elem :: current.tail)
    } else {
      throw new IndexOutOfBoundsException(index.toString)
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
      h
    }
  }

  final override def collect[B](pf: PartialFunction[A, B]): List[B] = {
    if (this eq Nil) Nil else {
      var rest = this
      var h: ::[B] = null
      // Special case for first element
      do {
        val x: Any = pf.applyOrElse(rest.head, List.partialNotApplied)
        if (x.asInstanceOf[AnyRef] ne List.partialNotApplied) h = new ::(x.asInstanceOf[B], Nil)
        rest = rest.tail
        if (rest eq Nil) return if (h eq null) Nil else h
      } while (h eq null)
      var t = h
      // Remaining elements
      do {
        val x: Any = pf.applyOrElse(rest.head, List.partialNotApplied)
        if (x.asInstanceOf[AnyRef] ne List.partialNotApplied) {
          val nx = new ::(x.asInstanceOf[B], Nil)
          t.next = nx
          t = nx
        }
        rest = rest.tail
      } while (rest ne Nil)
      h
    }
  }

  final override def flatMap[B](f: A => IterableOnce[B]): List[B] = {
    if (this eq Nil) Nil else {
      var rest = this
      var found = false
      var h: ::[B] = null
      var t: ::[B] = null
      while (rest ne Nil) {
        f(rest.head).iterator().foreach { b =>
          if (!found) {
            h = new ::(b, Nil)
            t = h
            found = true
          }
          else {
            val nx = new ::(b, Nil)
            t.next = nx
            t = nx
          }
        }
        rest = rest.tail
      }
      if (!found) Nil else h
    }
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

  @inline final override def dropWhile(p: A => Boolean): List[A] = {
    @tailrec
    def loop(xs: List[A]): List[A] =
      if (xs.isEmpty || !p(xs.head)) xs
      else loop(xs.tail)

    loop(this)
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

  // Create a proxy for Java serialization that allows us to avoid mutation
  // during deserialization.  This is the Serialization Proxy Pattern.
  protected final def writeReplace(): AnyRef = new List.SerializationProxy(this)

  override def className = "List"

  /** Builds a new list by applying a function to all elements of this list.
    *  Like `xs map f`, but returns `xs` unchanged if function
    *  `f` maps all elements to themselves (as determined by `eq`).
    *
    *  @param f      the function to apply to each element.
    *  @tparam B     the element type of the returned collection.
    *  @return       a list resulting from applying the given function
    *                `f` to each element of this list and collecting the results.
    *
    *  @usecase def mapConserve(f: A => A): List[A]
    *    @inheritdoc
    */
  @`inline` final def mapConserve[B >: A <: AnyRef](f: A => B): List[B] = {
    // Note to developers: there exists a duplication between this function and `reflect.internal.util.Collections#map2Conserve`.
    // If any successful optimization attempts or other changes are made, please rehash them there too.
    //@tailrec
    def loop(mappedHead: List[B] = Nil, mappedLast: ::[B], unchanged: List[A], pending: List[A]): List[B] = {
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
    loop(null, null, this, this)
  }

  final override def toList: List[A] = this

  // Override for performance
  override def equals(o: scala.Any): Boolean = {
    @tailrec def listEq(a: List[_], b: List[_]): Boolean =
      (a eq b) || {
        if (a.nonEmpty && b.nonEmpty && a.head == b.head) {
          listEq(a.tail, b.tail)
        }
        else {
          a.isEmpty && b.isEmpty
        }
      }

    o match {
      case that: List[_] => listEq(this, that)
      case _ => super.equals(o)
    }
  }

}

@SerialVersionUID(4L)
case class :: [+A](override val head: A, private[scala] var next: List[A @uncheckedVariance]) // sound because `next` is used only locally
  extends List[A] {
  override def isEmpty: Boolean = false
  override def headOption: Some[A] = Some(head)
  override def tail: List[A] = next
}

@SerialVersionUID(3L)
case object Nil extends List[Nothing] {
  override def isEmpty: Boolean = true
  override def head: Nothing = throw new NoSuchElementException("head of empty list")
  override def headOption: None.type = None
  override def tail: Nothing = throw new UnsupportedOperationException("tail of empty list")
  override def last: Nothing = throw new NoSuchElementException("last of empty list")
  override def init: Nothing = throw new UnsupportedOperationException("init of empty list")
  override def knownSize: Int = 0
}

/**
  * $factoryInfo
  * @define coll list
  * @define Coll `List`
  */
object List extends StrictOptimizedSeqFactory[List] {

  // override for now so we can compile in stdlib without https://github.com/scala/scala/pull/6229
  override def apply[A](elems: A*): List[A] = super.apply[A](elems: _*)

  def from[B](coll: collection.IterableOnce[B]): List[B] = coll match {
    case coll: List[B] => coll
    case _ => ListBuffer.from(coll).toList
  }

  def newBuilder[A](): Builder[A, List[A]] = new ListBuffer()

  def empty[A]: List[A] = Nil

  private[collection] val partialNotApplied = new Function1[Any, Any] { def apply(x: Any): Any = this }

  @SerialVersionUID(3L)
  private class SerializationProxy[A](@transient private var orig: List[A]) extends Serializable {

    private def writeObject(out: ObjectOutputStream): Unit = {
      out.defaultWriteObject()
      var xs: List[A] = orig
      while (!xs.isEmpty) {
        out.writeObject(xs.head)
        xs = xs.tail
      }
      out.writeObject(ListSerializeEnd)
    }

    // Java serialization calls this before readResolve during deserialization.
    // Read the whole list and store it in `orig`.
    private def readObject(in: ObjectInputStream): Unit = {
      in.defaultReadObject()
      val builder = List.newBuilder[A]()
      while (true) in.readObject match {
        case ListSerializeEnd =>
          orig = builder.result()
          return
        case a =>
          builder += a.asInstanceOf[A]
      }
    }

    // Provide the result stored in `orig` for Java serialization
    private def readResolve(): AnyRef = orig
  }
}


/** Only used for list serialization */
@SerialVersionUID(3L)
private[scala] case object ListSerializeEnd
