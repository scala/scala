/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.collection
package immutable

import generic._
import mutable.{Builder, ListBuffer}
import annotation.tailrec

/** A class for immutable linked lists representing ordered collections
 *  of elements of type.
 *
 *  This class comes with two implementing case classes `scala.Nil`
 *  and `scala.::` that implement the abstract members `isEmpty`,
 *  `head` and `tail`.
 *
 *  This class is optimal for last-in-first-out (LIFO), stack-like access patterns. If you need another access
 *  pattern, for example, random access or FIFO, consider using a collection more suited to this than `List`.
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
 *  @author  Martin Odersky and others
 *  @version 2.8
 *  @since   1.0
 *  @see  [["http://docs.scala-lang.org/overviews/collections/concrete-immutable-collection-classes.html#lists" "Scala's Collection Library overview"]]
 *  section on `Lists` for more information.
 *
 *  @define coll list
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
sealed abstract class List[+A] extends LinearSeq[A]
                                  with Product
                                  with GenericTraversableTemplate[A, List]
                                  with LinearSeqOptimized[A, List[A]] {
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
   *  @example `1 :: List(2, 3) = List(2, 3).::(1) = List(1, 2, 3)`
   *  @usecase def ::(x: A): List[A]
   */
  def ::[B >: A] (x: B): List[B] =
    new scala.collection.immutable.::(x, this)

  /** Adds the elements of a given list in front of this list.
   *  @param prefix  The list elements to prepend.
   *  @return a list resulting from the concatenation of the given
   *    list `prefix` and this list.
   *  @example `List(1, 2) ::: List(3, 4) = List(3, 4).:::(List(1, 2)) = List(1, 2, 3, 4)`
   *  @usecase def :::(prefix: List[A]): List[A]
   */
  def :::[B >: A](prefix: List[B]): List[B] =
    if (isEmpty) prefix
    else (new ListBuffer[B] ++= prefix).prependToList(this)

  /** Adds the elements of a given list in reverse order in front of this list.
   *  `xs reverse_::: ys` is equivalent to
   *  `xs.reverse ::: ys` but is more efficient.
   *
   *  @param prefix the prefix to reverse and then prepend
   *  @return       the concatenation of the reversed prefix and the current list.
   *  @usecase def reverse_:::(prefix: List[A]): List[A]
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
   *  `f` maps all elements to themselves (wrt eq).
   *
   *  @param f      the function to apply to each element.
   *  @tparam B     the element type of the returned collection.
   *  @return       a list resulting from applying the given function
   *                `f` to each element of this list and collecting the results.
   *  @usecase def mapConserve(f: A => A): List[A]
   */
  def mapConserve[B >: A <: AnyRef](f: A => B): List[B] = {
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

  override def ++[B >: A, That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[List[A], B, That]): That = {
    val b = bf(this)
    if (b.isInstanceOf[ListBuffer[_]]) (this ::: that.seq.toList).asInstanceOf[That]
    else super.++(that)
  }

  override def +:[B >: A, That](elem: B)(implicit bf: CanBuildFrom[List[A], B, That]): That = bf match {
    case _: List.GenericCanBuildFrom[_] => (elem :: this).asInstanceOf[That]
    case _ => super.+:(elem)(bf)
  }

  override def toList: List[A] = this

  override def take(n: Int): List[A] = {
    val b = new ListBuffer[A]
    var i = 0
    var these = this
    while (!these.isEmpty && i < n) {
      i += 1
      b += these.head
      these = these.tail
    }
    if (these.isEmpty) this
    else b.toList
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

  override def slice(from: Int, until: Int): List[A] = {
    val lo = math.max(from, 0)
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

  override def takeWhile(p: A => Boolean): List[A] = {
    val b = new ListBuffer[A]
    var these = this
    while (!these.isEmpty && p(these.head)) {
      b += these.head
      these = these.tail
    }
    b.toList
  }

  override def dropWhile(p: A => Boolean): List[A] = {
    @tailrec
    def loop(xs: List[A]): List[A] =
      if (xs.isEmpty || !p(xs.head)) xs
      else loop(xs.tail)

    loop(this)
  }

  override def span(p: A => Boolean): (List[A], List[A]) = {
    val b = new ListBuffer[A]
    var these = this
    while (!these.isEmpty && p(these.head)) {
      b += these.head
      these = these.tail
    }
    (b.toList, these)
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

  override def stringPrefix = "List"

  override def toStream : Stream[A] =
    if (isEmpty) Stream.Empty
    else new Stream.Cons(head, tail.toStream)

  /** Like <code>span</code> but with the predicate inverted.
   */
  @deprecated("use `span { x => !p(x) }` instead", "2.8.0")
  def break(p: A => Boolean): (List[A], List[A]) = span { x => !p(x) }

  @deprecated("use `filterNot' instead", "2.8.0")
  def remove(p: A => Boolean): List[A] = filterNot(p)

  /** Computes the difference between this list and the given list
   *  `that`.
   *
   *  @param that the list of elements to remove from this list.
   *  @return     this list without the elements of the given list
   *              `that`.
   */
  @deprecated("use `list1 filterNot (list2 contains)` instead", "2.8.0")
  def -- [B >: A](that: List[B]): List[B] = {
    val b = new ListBuffer[B]
    var these = this
    while (!these.isEmpty) {
      if (!that.contains(these.head)) b += these.head
      these = these.tail
    }
    b.toList
  }

  /** Computes the difference between this list and the given object
   *  `x`.
   *
   *  @param x    the object to remove from this list.
   *  @return     this list without occurrences of the given object
   *              `x`.
   */
  @deprecated("use `filterNot (_ == x)` instead", "2.8.0")
  def - [B >: A](x: B): List[B] = {
    val b = new ListBuffer[B]
    var these = this
    while (!these.isEmpty) {
      if (these.head != x) b += these.head
      these = these.tail
    }
    b.toList
  }

  @deprecated("use `distinct' instead", "2.8.0")
  def removeDuplicates: List[A] = distinct

  @deprecated("use `sortWith' instead", "2.8.0")
  def sort(lt : (A,A) => Boolean): List[A] = {
    /** Merge two already-sorted lists */
    def merge(l1: List[A], l2: List[A]): List[A] = {
      val res = new ListBuffer[A]
      var left1 = l1
      var left2 = l2

      while (!left1.isEmpty && !left2.isEmpty) {
        if(lt(left1.head, left2.head)) {
          res += left1.head
          left1 = left1.tail
        } else {
          res += left2.head
          left2 = left2.tail
        }
      }

      res ++= left1
      res ++= left2

      res.toList
    }

    /** Split a list into two lists of about the same size */
    def split(lst: List[A]) = {
      val res1 = new ListBuffer[A]
      val res2 = new ListBuffer[A]
      var left = lst

      while (!left.isEmpty) {
        res1 += left.head
        left = left.tail
        if (!left.isEmpty) {
          res2 += left.head
          left = left.tail
        }
      }

      (res1.toList, res2.toList)
    }


    /** Merge-sort the specified list */
    def ms(lst: List[A]): List[A] =
      lst match {
        case Nil => lst
        case x :: Nil => lst
        case x :: y :: Nil =>
          if (lt(x,y))
            lst
          else
            y :: x :: Nil

        case lst =>
          val (l1, l2) = split(lst)
          val l1s = ms(l1)
          val l2s = ms(l2)
          merge(l1s, l2s)
      }

    ms(this)
  }

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
    case that1: collection.Seq[_] => that1.isEmpty
    case _ => false
  }
}

/** A non empty list characterized by a head and a tail.
 *  @param hd   the first element of the list
 *  @param tl   the list containing the remaining elements of this list after the first one.
 *  @tparam B   the type of the list elements.
 *  @author  Martin Odersky
 *  @version 1.0, 15/07/2003
 *  @since   2.8
 */
@SerialVersionUID(0L - 8476791151983527571L)
final case class ::[B](private var hd: B, private[scala] var tl: List[B]) extends List[B] {
  override def head : B = hd
  override def tail : List[B] = tl
  override def isEmpty: Boolean = false

  import java.io._

  private def writeObject(out: ObjectOutputStream) {
    var xs: List[B] = this
    while (!xs.isEmpty) { out.writeObject(xs.head); xs = xs.tail }
    out.writeObject(ListSerializeEnd)
  }

  private def readObject(in: ObjectInputStream) {
    hd = in.readObject.asInstanceOf[B]
    assert(hd != ListSerializeEnd)
    var current: ::[B] = this
    while (true) in.readObject match {
      case ListSerializeEnd =>
        current.tl = Nil
        return
      case a : Any =>
        val list : ::[B] = new ::(a.asInstanceOf[B], Nil)
        current.tl = list
        current = list
    }
  }
}

/** $factoryInfo
 *  @define coll list
 *  @define Coll List
 */
object List extends SeqFactory[List] {

  import scala.collection.{Iterable, Seq, IndexedSeq}

  /** $genericCanBuildFromInfo */
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, List[A]] = new GenericCanBuildFrom[A]

  def newBuilder[A]: Builder[A, List[A]] = new ListBuffer[A]

  override def empty[A]: List[A] = Nil

  override def apply[A](xs: A*): List[A] = xs.toList

  /** Create a sorted list with element values
   * `v<sub>n+1</sub> = step(v<sub>n</sub>)`
   * where `v<sub>0</sub> = start`
   * and elements are in the range between `start` (inclusive)
   * and `end` (exclusive)
   *
   *  @param start the start value of the list
   *  @param end  the end value of the list
   *  @param step the increment function of the list, which given `v<sub>n</sub>`,
   *              computes `v<sub>n+1</sub>`. Must be monotonically increasing
   *              or decreasing.
   *  @return     the sorted list of all integers in range [start;end).
   */
  @deprecated("use `iterate' instead", "2.8.0")
  def range(start: Int, end: Int, step: Int => Int): List[Int] = {
    val up = step(start) > start
    val down = step(start) < start
    val b = new ListBuffer[Int]
    var i = start
    while ((!up || i < end) && (!down || i > end)) {
      b += i
      val next = step(i)
      if (i == next)
        throw new IllegalArgumentException("the step function did not make any progress on "+ i)
      i = next
    }
    b.toList
  }

  /** Create a list containing several copies of an element.
   *
   *  @param n    the length of the resulting list
   *  @param elem the element composing the resulting list
   *  @return     a list composed of n elements all equal to elem
   */
  @deprecated("use `fill' instead", "2.8.0")
  def make[A](n: Int, elem: A): List[A] = {
    val b = new ListBuffer[A]
    var i = 0
    while (i < n) {
      b += elem
      i += 1
    }
    b.toList
  }

  /** Concatenate all the elements of a given list of lists.
   *
   *  @param xss the list of lists that are to be concatenated
   *  @return    the concatenation of all the lists
   */
  @deprecated("use `xss.flatten' instead of `List.flatten(xss)'", "2.8.0")
  def flatten[A](xss: List[List[A]]): List[A] = {
    val b = new ListBuffer[A]
    for (xs <- xss) {
      var xc = xs
      while (!xc.isEmpty) {
        b += xc.head
        xc = xc.tail
      }
    }
    b.toList
  }

  /** Transforms a list of pairs into a pair of lists.
   *
   *  @param xs the list of pairs to unzip
   *  @return a pair of lists.
   */
  @deprecated("use `xs.unzip' instead of `List.unzip(xs)'", "2.8.0")
  def unzip[A,B](xs: List[(A,B)]): (List[A], List[B]) = {
    val b1 = new ListBuffer[A]
    val b2 = new ListBuffer[B]
    var xc = xs
    while (!xc.isEmpty) {
      b1 += xc.head._1
      b2 += xc.head._2
      xc = xc.tail
    }
    (b1.toList, b2.toList)
  }

  /** Transforms an iterable of pairs into a pair of lists.
   *
   *  @param xs the iterable of pairs to unzip
   *  @return a pair of lists.
   */
  @deprecated("use `xs.unzip' instead of `List.unzip(xs)'", "2.8.0")
  def unzip[A,B](xs: Iterable[(A,B)]): (List[A], List[B]) =
      xs.foldRight[(List[A], List[B])]((Nil, Nil)) {
        case ((x, y), (xs, ys)) => (x :: xs, y :: ys)
      }

  /**
   * Returns the `Left` values in the given `Iterable`
   * of `Either`s.
   */
  @deprecated("use `xs collect { case Left(x: A) => x }' instead of `List.lefts(xs)'", "2.8.0")
  def lefts[A, B](es: Iterable[Either[A, B]]) =
    es.foldRight[List[A]](Nil)((e, as) => e match {
      case Left(a) => a :: as
      case Right(_) => as
    })

  /**
   * Returns the `Right` values in the given`Iterable` of  `Either`s.
   */
  @deprecated("use `xs collect { case Right(x: B) => x }' instead of `List.rights(xs)'", "2.8.0")
  def rights[A, B](es: Iterable[Either[A, B]]) =
    es.foldRight[List[B]](Nil)((e, bs) => e match {
      case Left(_) => bs
      case Right(b) => b :: bs
    })

  /** Transforms an Iterable of Eithers into a pair of lists.
   *
   *  @param xs the iterable of Eithers to separate
   *  @return a pair of lists.
   */
  @deprecated("use `(for (Left(x) <- es) yield x, for (Right(x) <- es) yield x)` instead", "2.8.0")
  def separate[A,B](es: Iterable[Either[A, B]]): (List[A], List[B]) =
    es.foldRight[(List[A], List[B])]((Nil, Nil)) {
      case (Left(a), (lefts, rights)) => (a :: lefts, rights)
      case (Right(b), (lefts, rights)) => (lefts, b :: rights)
    }

  /** Converts an iterator to a list.
   *
   *  @param it the iterator to convert
   *  @return   a list that contains the elements returned by successive
   *            calls to `it.next`
   */
  @deprecated("use `it.toList' instead of `List.toList(it)'", "2.8.0")
  def fromIterator[A](it: Iterator[A]): List[A] = it.toList

  /** Converts an array into a list.
   *
   *  @param arr the array to convert
   *  @return    a list that contains the same elements than `arr`
   *             in the same order
   */
  @deprecated("use `array.toList' instead of `List.fromArray(array)'", "2.8.0")
  def fromArray[A](arr: Array[A]): List[A] = fromArray(arr, 0, arr.length)

  /** Converts a range of an array into a list.
   *
   *  @param arr   the array to convert
   *  @param start the first index to consider
   *  @param len   the length of the range to convert
   *  @return      a list that contains the same elements than `arr`
   *               in the same order
   */
  @deprecated("use `array.view(start, end).toList' instead of `List.fromArray(array, start, end)'", "2.8.0")
  def fromArray[A](arr: Array[A], start: Int, len: Int): List[A] = {
    var res: List[A] = Nil
    var i = start + len
    while (i > start) {
      i -= 1
      res = arr(i) :: res
    }
    res
  }

  /** Parses a string which contains substrings separated by a
   *  separator character and returns a list of all substrings.
   *
   *  @param str       the string to parse
   *  @param separator the separator character
   *  @return          the list of substrings
   */
  @deprecated("use `str.split(separator).toList' instead of `List.fromString(str, separator)'", "2.8.0")
  def fromString(str: String, separator: Char): List[String] = {
    var words: List[String] = Nil
    var pos = str.length()
    while (pos > 0) {
      val pos1 = str.lastIndexOf(separator, pos - 1)
      if (pos1 + 1 < pos)
        words = str.substring(pos1 + 1, pos) :: words
      pos = pos1
    }
    words
  }

  /** Returns the given list of characters as a string.
   *
   *  @param xs the list to convert.
   *  @return   the list in form of a string.
   */
  @deprecated("use `xs.mkString' instead of `List.toString(xs)'", "2.8.0")
  def toString(xs: List[Char]): String = {
    val sb = new StringBuilder()
    var xc = xs
    while (!xc.isEmpty) {
      sb.append(xc.head)
      xc = xc.tail
    }
    sb.toString()
  }

  /** Like xs map f, but returns `xs` unchanged if function
   *  `f` maps all elements to themselves.
   */
  @deprecated("use `xs.mapConserve(f)' instead of `List.mapConserve(xs, f)'", "2.8.0")
  def mapConserve[A <: AnyRef](xs: List[A])(f: A => A): List[A] = {
    def loop(ys: List[A]): List[A] =
      if (ys.isEmpty) xs
      else {
        val head0 = ys.head
        val head1 = f(head0)
        if (head1 eq head0) {
          loop(ys.tail)
        } else {
          val ys1 = head1 :: mapConserve(ys.tail)(f)
          if (xs eq ys) ys1
          else {
            val b = new ListBuffer[A]
            var xc = xs
            while (xc ne ys) {
              b += xc.head
              xc = xc.tail
            }
            b.prependToList(ys1)
          }
        }
      }
    loop(xs)
  }

  /** Returns the list resulting from applying the given function `f`
   *  to corresponding elements of the argument lists.
   *
   *  @param f function to apply to each pair of elements.
   *  @return `[f(a0,b0), ..., f(an,bn)]` if the lists are
   *          `[a0, ..., ak]`, `[b0, ..., bl]` and
   *          `n = min(k,l)`
   */
  @deprecated("use `(xs, ys).zipped.map(f)' instead of `List.map2(xs, ys)(f)'", "2.8.0")
  def map2[A,B,C](xs: List[A], ys: List[B])(f: (A, B) => C): List[C] = {
    val b = new ListBuffer[C]
    var xc = xs
    var yc = ys
    while (!xc.isEmpty && !yc.isEmpty) {
      b += f(xc.head, yc.head)
      xc = xc.tail
      yc = yc.tail
    }
    b.toList
  }

  /** Returns the list resulting from applying the given function
   *  `f` to corresponding elements of the argument lists.
   *
   *  @param f function to apply to each pair of elements.
   *  @return  `[f(a<sub>0</sub>,b<sub>0</sub>,c<sub>0</sub>),
   *           ..., f(a<sub>n</sub>,b<sub>n</sub>,c<sub>n</sub>)]`
   *           if the lists are `[a<sub>0</sub>, ..., a<sub>k</sub>]`,
   *           `[b<sub>0</sub>, ..., b<sub>l</sub>]`,
   *           `[c<sub>0</sub>, ..., c<sub>m</sub>]` and
   *           `n = min(k,l,m)`
   */
  @deprecated("use `(xs, ys, zs).zipped.map(f)' instead of `List.map3(xs, ys, zs)(f)'", "2.8.0")
  def map3[A,B,C,D](xs: List[A], ys: List[B], zs: List[C])(f: (A, B, C) => D): List[D] = {
    val b = new ListBuffer[D]
    var xc = xs
    var yc = ys
    var zc = zs
    while (!xc.isEmpty && !yc.isEmpty && !zc.isEmpty) {
      b += f(xc.head, yc.head, zc.head)
      xc = xc.tail
      yc = yc.tail
      zc = zc.tail
    }
    b.toList
  }

  /** Tests whether the given predicate `p` holds
   *  for all corresponding elements of the argument lists.
   *
   *  @param p function to apply to each pair of elements.
   *  @return  `(p(a<sub>0</sub>,b<sub>0</sub>) &amp;&amp;
   *           ... &amp;&amp; p(a<sub>n</sub>,b<sub>n</sub>))]`
   *           if the lists are `[a<sub>0</sub>, ..., a<sub>k</sub>]`;
   *           `[b<sub>0</sub>, ..., b<sub>l</sub>]`
   *           and `n = min(k,l)`
   */
  @deprecated("use `(xs, ys).zipped.forall(f)' instead of `List.forall2(xs, ys)(f)'", "2.8.0")
  def forall2[A,B](xs: List[A], ys: List[B])(f: (A, B) => Boolean): Boolean = {
    var xc = xs
    var yc = ys
    while (!xc.isEmpty && !yc.isEmpty) {
      if (!f(xc.head, yc.head)) return false
      xc = xc.tail
      yc = yc.tail
    }
    true
  }

  /** Tests whether the given predicate `p` holds
   *  for some corresponding elements of the argument lists.
   *
   *  @param p function to apply to each pair of elements.
   *  @return  `n != 0 &amp;&amp; (p(a<sub>0</sub>,b<sub>0</sub>) ||
   *           ... || p(a<sub>n</sub>,b<sub>n</sub>))]` if the lists are
   *           `[a<sub>0</sub>, ..., a<sub>k</sub>]`,
   *           `[b<sub>0</sub>, ..., b<sub>l</sub>]` and
   *           `n = min(k,l)`
   */
  @deprecated("use `(xs, ys).zipped.exists(f)' instead of `List.exists2(xs, ys)(f)'", "2.8.0")
  def exists2[A,B](xs: List[A], ys: List[B])(f: (A, B) => Boolean): Boolean = {
    var xc = xs
    var yc = ys
    while (!xc.isEmpty && !yc.isEmpty) {
      if (f(xc.head, yc.head)) return true
      xc = xc.tail
      yc = yc.tail
    }
    false
  }

  /** Transposes a list of lists.
   *  pre: All element lists have the same length.
   *
   *  @param xss the list of lists
   *  @return    the transposed list of lists
   */
  @deprecated("use `xss.transpose' instead of `List.transpose(xss)'", "2.8.0")
  def transpose[A](xss: List[List[A]]): List[List[A]] = {
    val buf = new ListBuffer[List[A]]
    var yss = xss
    while (!yss.head.isEmpty) {
      buf += (yss map (_.head))
      yss = (yss map (_.tail))
    }
    buf.toList
  }
}

/** Only used for list serialization */
@SerialVersionUID(0L - 8476791151975527571L)
private[scala] case object ListSerializeEnd

