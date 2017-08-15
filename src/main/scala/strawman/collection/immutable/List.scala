package strawman
package collection
package immutable

import java.io.{ObjectInputStream, ObjectOutputStream}

import scala.annotation.unchecked.uncheckedVariance
import scala.annotation.tailrec
import mutable.{Builder, ListBuffer}

import scala.{Any, AnyRef, Boolean, Function1, `inline`, Int, NoSuchElementException, Nothing, PartialFunction, SerialVersionUID, Serializable, transient, Unit, UnsupportedOperationException}


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
@SerialVersionUID(-6084104484083858598L) // value computed by serialver for 2.11.2, annotation added in 2.11.4
sealed trait List[+A]
  extends LinearSeq[A]
    with LinearSeqOps[A, List, List[A]]
    with StrictOptimizedSeqOps[A, List, List[A]]
    with Serializable {

  def iterableFactory: SeqFactory[List] = List

  protected[this] def fromSpecificIterable(coll: collection.Iterable[A]): List[A] = fromIterable(coll)

  protected[this] def newSpecificBuilder() = List.newBuilder[A]()

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

  override def prepend[B >: A](elem: B): List[B] = elem :: this

  // When calling prependAll with another list `prefix`, avoid copying `this`
  override def prependAll[B >: A](prefix: collection.Iterable[B]): List[B] = prefix match {
    case xs: List[B] => xs ::: this
    case _ => super.prependAll(prefix)
  }

  // When calling appendAll with another list `suffix`, avoid copying `suffix`
  override def appendAll[B >: A](suffix: collection.Iterable[B]): List[B] = suffix match {
    case xs: List[B] => this ::: xs
    case _ => super.appendAll(suffix)
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

  override def drop(n: Int): List[A] = {
    var these = this
    var count = n
    while (!these.isEmpty && count > 0) {
      these = these.tail
      count -= 1
    }
    these
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

  override def reverse: List[A] = {
    var result: List[A] = Nil
    var these = this
    while (!these.isEmpty) {
      result = these.head :: result
      these = these.tail
    }
    result
  }

  override def foldRight[B](z: B)(op: (A, B) => B): B =
    reverse.foldLeft(z)((right, left) => op(left, right))

  // Create a proxy for Java serialization that allows us to avoid mutation
  // during deserialization.  This is the Serialization Proxy Pattern.
  protected final def writeReplace(): AnyRef = new List.SerializationProxy(this)

  override def className = "List"
}

case class :: [+A](x: A, private[collection] var next: List[A @uncheckedVariance]) // sound because `next` is used only locally
  extends List[A] {
  override def isEmpty: Boolean = false
  override def nonEmpty: Boolean = true
  override def head: A = x
  override def tail: List[A] = next
}

case object Nil extends List[Nothing] {
  override def isEmpty: Boolean = true
  override def nonEmpty: Boolean = false
  override def head: Nothing = throw new NoSuchElementException("head of empty list")
  override def tail: Nothing = throw new UnsupportedOperationException("tail of empty list")
}

object List extends SeqFactory[List] {

  def fromIterable[B](coll: collection.Iterable[B]): List[B] = coll match {
    case coll: List[B] => coll
    case _ => ListBuffer.fromIterable(coll).toList
  }

  override def newBuilder[A](): Builder[A, List[A]] = ListBuffer.newBuilder[A]().mapResult(_.toList)

  def empty[A]: List[A] = Nil

  private[collection] val partialNotApplied = new Function1[Any, Any] { def apply(x: Any): Any = this }

  @SerialVersionUID(1L)
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
@SerialVersionUID(0L - 8476791151975527571L)
private[strawman] case object ListSerializeEnd
