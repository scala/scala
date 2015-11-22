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

import generic._
import mutable.{Builder, ListBuffer}
import scala.annotation.tailrec
import java.io._

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
 *  val when = "AM" :: "PM" :: List()
 *
 *  // Pattern match
 *  days match {
 *    case firstDay :: otherDays =>
 *      println("The first day of the week is: " + firstDay)
 *    case List() =>
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
 *  @define thatinfo the class of the returned collection. In the standard library configuration,
 *    `That` is always `List[B]` because an implicit of type `CanBuildFrom[List, B, That]`
 *    is defined in object `List`.
 *  @define bfinfo an implicit value of class `CanBuildFrom` which determines the
 *    result class `That` from the current representation type `Repr`
 *    and the new element type `B`. This is usually the `canBuildFrom` value
 *    defined in object `List`.
 *  @define orderDependent
 *  @define orderDependentFold
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
@SerialVersionUID(-6084104484083858598L) // value computed by serialver for 2.11.2, annotation added in 2.11.4
sealed abstract class List[+A] extends AbstractSeq[A]
                                  with LinearSeq[A]
                                  with Product
                                  with GenericTraversableTemplate[A, List]
                                  with LinearSeqOptimized[A, List[A]]
                                  with Serializable {
  override def companion: GenericCompanion[List] = List

  import scala.collection.{Iterable, Traversable, Seq, IndexedSeq}

  def isEmpty: Boolean
  def head: A
  def tail: List[A]

  // New methods in List

  /** Adds an element at the beginning of this list.
   *  @param x the element to prepend.
   *  @return  a list which contains `x` as first element and
   *           which continues with this list.
   *
   *  @usecase def ::(x: A): List[A]
   *    @inheritdoc
   *
   *    Example:
   *    {{{1 :: List(2, 3) = List(2, 3).::(1) = List(1, 2, 3)}}}
   */
  def ::[B >: A] (x: B): List[B] =
    new scala.collection.immutable.::(x, this)

  /** Adds the elements of a given list in front of this list.
   *  @param prefix  The list elements to prepend.
   *  @return a list resulting from the concatenation of the given
   *    list `prefix` and this list.
   *
   *  @usecase def :::(prefix: List[A]): List[A]
   *    @inheritdoc
   *
   *    Example:
   *    {{{List(1, 2) ::: List(3, 4) = List(3, 4).:::(List(1, 2)) = List(1, 2, 3, 4)}}}
   */
  def :::[B >: A](prefix: List[B]): List[B] =
    if (isEmpty) prefix
    else if (prefix.isEmpty) this
    else (new ListBuffer[B] ++= prefix).prependToList(this)

  /** Adds the elements of a given list in reverse order in front of this list.
   *  `xs reverse_::: ys` is equivalent to
   *  `xs.reverse ::: ys` but is more efficient.
   *
   *  @param prefix the prefix to reverse and then prepend
   *  @return       the concatenation of the reversed prefix and the current list.
   *
   *  @usecase def reverse_:::(prefix: List[A]): List[A]
   *    @inheritdoc
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
  @inline final def mapConserve[B >: A <: AnyRef](f: A => B): List[B] = {
    // Note to developers: there exists a duplication between this function and `reflect.internal.util.Collections#map2Conserve`.
    // If any successful optimization attempts or other changes are made, please rehash them there too.
    @tailrec
    def loop(mapped: ListBuffer[B], unchanged: List[A], pending: List[A]): List[B] =
      if (pending.isEmpty) {
        if (mapped eq null) unchanged
        else mapped.prependToList(unchanged)
      }
      else {
        val head0 = pending.head
        val head1 = f(head0)

        if (head1 eq head0.asInstanceOf[AnyRef])
          loop(mapped, unchanged, pending.tail)
        else {
          val b = if (mapped eq null) new ListBuffer[B] else mapped
          var xc = unchanged
          while (xc ne pending) {
            b += xc.head
            xc = xc.tail
          }
          b += head1
          val tail0 = pending.tail
          loop(b, tail0, tail0)
        }
      }
    loop(null, this, this)
  }

  // Overridden methods from IterableLike and SeqLike or overloaded variants of such methods

  override def ++[B >: A, That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[List[A], B, That]): That =
    if (bf eq List.ReusableCBF) (this ::: that.seq.toList).asInstanceOf[That]
    else super.++(that)

  override def +:[B >: A, That](elem: B)(implicit bf: CanBuildFrom[List[A], B, That]): That = bf match {
    case _: List.GenericCanBuildFrom[_] => (elem :: this).asInstanceOf[That]
    case _ => super.+:(elem)(bf)
  }

  override def toList: List[A] = this

  override def take(n: Int): List[A] = if (isEmpty || n <= 0) Nil else {
    val h = new ::(head, Nil)
    var t = h
    var rest = tail
    var i = 1
    while ({if (rest.isEmpty) return this; i < n}) {
      i += 1
      val nx = new ::(rest.head, Nil)
      t.tl = nx
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
  
  @noinline // TODO - fix optimizer bug that requires noinline (see SI-8334)
  final override def map[B, That](f: A => B)(implicit bf: CanBuildFrom[List[A], B, That]): That = {
    if (bf eq List.ReusableCBF) {
      if (this eq Nil) Nil.asInstanceOf[That] else {
        val h = new ::[B](f(head), Nil)
        var t: ::[B] = h
        var rest = tail
        while (rest ne Nil) {
          val nx = new ::(f(rest.head), Nil)
          t.tl = nx
          t = nx
          rest = rest.tail
        }
        h.asInstanceOf[That]
      }
    }
    else super.map(f)
  }
  
  @noinline // TODO - fix optimizer bug that requires noinline for map; applied here to be safe (see SI-8334)
  final override def collect[B, That](pf: PartialFunction[A, B])(implicit bf: CanBuildFrom[List[A], B, That]): That = {
    if (bf eq List.ReusableCBF) {
      if (this eq Nil) Nil.asInstanceOf[That] else {
        var rest = this
        var h: ::[B] = null
        // Special case for first element
        do {
          val x: Any = pf.applyOrElse(rest.head, List.partialNotApplied)
          if (x.asInstanceOf[AnyRef] ne List.partialNotApplied) h = new ::(x.asInstanceOf[B], Nil)
          rest = rest.tail
          if (rest eq Nil) return (if (h eq null ) Nil else h).asInstanceOf[That]
        } while (h eq null)
        var t = h
        // Remaining elements
        do {
          val x: Any = pf.applyOrElse(rest.head, List.partialNotApplied)
          if (x.asInstanceOf[AnyRef] ne List.partialNotApplied) {
            val nx = new ::(x.asInstanceOf[B], Nil)
            t.tl = nx
            t = nx
          }
          rest = rest.tail
        } while (rest ne Nil)
        h.asInstanceOf[That]
      }
    }
    else super.collect(pf)
  }
  
  @noinline // TODO - fix optimizer bug that requires noinline for map; applied here to be safe (see SI-8334)
  final override def flatMap[B, That](f: A => GenTraversableOnce[B])(implicit bf: CanBuildFrom[List[A], B, That]): That = {
    if (bf eq List.ReusableCBF) {
      if (this eq Nil) Nil.asInstanceOf[That] else {
        var rest = this
        var found = false
        var h: ::[B] = null
        var t: ::[B] = null
        while (rest ne Nil) {
          f(rest.head).seq.foreach{ b =>
            if (!found) {
              h = new ::(b, Nil)
              t = h
              found = true
            }
            else {
              val nx = new ::(b, Nil)
              t.tl = nx
              t = nx
            }
          }
          rest = rest.tail
        }
        (if (!found) Nil else h).asInstanceOf[That]
      }
    }
    else super.flatMap(f)
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
  @inline final override def foreach[U](f: A => U) {
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

  override def stringPrefix = "List"

  override def toStream : Stream[A] =
    if (isEmpty) Stream.Empty
    else new Stream.Cons(head, tail.toStream)

  // Create a proxy for Java serialization that allows us to avoid mutation
  // during de-serialization.  This is the Serialization Proxy Pattern.
  protected final def writeReplace(): AnyRef = new List.SerializationProxy(this)
}

/** The empty list.
 *
 *  @author  Martin Odersky
 *  @version 1.0, 15/07/2003
 *  @since   2.8
 */
@SerialVersionUID(0 - 8256821097970055419L)
case object Nil extends List[Nothing] {
  override def isEmpty = true
  override def head: Nothing =
    throw new NoSuchElementException("head of empty list")
  override def tail: List[Nothing] =
    throw new UnsupportedOperationException("tail of empty list")
  // Removal of equals method here might lead to an infinite recursion similar to IntMap.equals.
  override def equals(that: Any) = that match {
    case that1: scala.collection.GenSeq[_] => that1.isEmpty
    case _ => false
  }
}

/** A non empty list characterized by a head and a tail.
 *  @param head the first element of the list
 *  @param tl   the list containing the remaining elements of this list after the first one.
 *  @tparam B   the type of the list elements.
 *  @author  Martin Odersky
 *  @version 1.0, 15/07/2003
 *  @since   2.8
 */
@SerialVersionUID(509929039250432923L) // value computed by serialver for 2.11.2, annotation added in 2.11.4
final case class ::[B](override val head: B, private[scala] var tl: List[B]) extends List[B] {
  override def tail : List[B] = tl
  override def isEmpty: Boolean = false
}

/** $factoryInfo
 *  @define coll list
 *  @define Coll `List`
 */
object List extends SeqFactory[List] {
  /** $genericCanBuildFromInfo */
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, List[A]] =
    ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]

  def newBuilder[A]: Builder[A, List[A]] = new ListBuffer[A]

  override def empty[A]: List[A] = Nil

  override def apply[A](xs: A*): List[A] = xs.toList
  
  private[collection] val partialNotApplied = new Function1[Any, Any] { def apply(x: Any): Any = this }

  @SerialVersionUID(1L)
  private class SerializationProxy[A](@transient private var orig: List[A]) extends Serializable {

    private def writeObject(out: ObjectOutputStream) {
      out.defaultWriteObject()
      var xs: List[A] = orig
      while (!xs.isEmpty) {
        out.writeObject(xs.head)
        xs = xs.tail
      }
      out.writeObject(ListSerializeEnd)
    }

    // Java serialization calls this before readResolve during de-serialization.
    // Read the whole list and store it in `orig`.
    private def readObject(in: ObjectInputStream) {
      in.defaultReadObject()
      val builder = List.newBuilder[A]
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
private[scala] case object ListSerializeEnd
