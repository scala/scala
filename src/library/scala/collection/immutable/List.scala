/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package immutable

import generic._
import mutable.{Builder, ListBuffer}
import annotation.tailrec

/** A class for immutable linked lists representing ordered collections
 *  of elements of type.
 *
 * This class comes with two implementing case classes `scala.Nil`
 * and `scala.::` that implement the abstract members `isEmpty`,
 * `head` and `tail`.
 *
 *  @author  Martin Odersky and others
 *  @version 2.8
 *  @since   1.0
 *
 *  @tparam  A    the type of the list's elements
 *
 *  @define Coll List
 *  @define coll list
 *  @define thatinfo the class of the returned collection. In the standard library configuration,
 *    `That` is always `List[B]` because an implicit of type `CanBuildFrom[List, B, That]`
 *    is defined in object `List`.
 *  @define $bfinfo an implicit value of class `CanBuildFrom` which determines the
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
                                  with LinearSeqLike[A, List[A]] {
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
   *  @ex `1 :: List(2, 3) = List(2, 3).::(1) = List(1, 2, 3)`
   *  @usecase def ::(x: A): List[A]
   */
  def ::[B >: A] (x: B): List[B] =
    new scala.collection.immutable.::(x, this)

  /** Adds the elements of a given list in front of this list.
   *  @param prefix  The list elements to prepend.
   *  @return a list resulting from the concatenation of the given
   *    list `prefix` and this list.
   *  @ex `List(1, 2) ::: List(3, 4) = List(3, 4).:::(List(1, 2)) = List(1, 2, 3, 4)`
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
   *  `f` maps all elements to themselves (wrt ==).
   *
   *  Note: Unlike `map`, `mapConserve` is not tail-recursive.
   *
   *  @param f      the function to apply to each element.
   *  @tparam B     the element type of the returned collection.
   *  @return       a list resulting from applying the given function
   *                `f` to each element of this list and collecting the results.
   *  @usecase def mapConserve[B](f: A => B): List[A]
   */
  def mapConserve[B >: A] (f: A => B): List[B] = {
    def loop(ys: List[A]): List[B] =
      if (ys.isEmpty) this
      else {
        val head0 = ys.head
        val head1 = f(head0)
        if (head1 == head0) {
          loop(ys.tail)
        } else {
          val ys1 = head1 :: ys.tail.mapConserve(f)
          if (this eq ys) ys1
          else {
            val b = new ListBuffer[B]
            var xc = this
            while (xc ne ys) {
              b += xc.head
              xc = xc.tail
            }
            b.prependToList(ys1)
          }
        }
      }
    loop(this)
  }

  // Overridden methods from IterableLike or overloaded variants of such methods

  override def ++[B >: A, That](that: Traversable[B])(implicit bf: CanBuildFrom[List[A], B, That]): That = {
    val b = bf(this)
    if (b.isInstanceOf[ListBuffer[_]]) (this ::: that.toList).asInstanceOf[That]
    else super.++(that)
  }

  override def ++[B >: A, That](that: Iterator[B])(implicit bf: CanBuildFrom[List[A], B, That]): That =
    this ++ that.toList

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

  override def slice(start: Int, end: Int): List[A] = {
    var len = end
    if (start > 0) len -= start
    drop(start) take len
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
  @deprecated("use `span { x => !p(x) }` instead")
  def break(p: A => Boolean): (List[A], List[A]) = span { x => !p(x) }

  @deprecated("use `filterNot' instead")
  def remove(p: A => Boolean): List[A] = filterNot(p)

  /** Computes the difference between this list and the given list
   *  `that`.
   *
   *  @param that the list of elements to remove from this list.
   *  @return     this list without the elements of the given list
   *              `that`.
   */
  @deprecated("use `list1 filterNot (list2 contains)` instead")
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
  @deprecated("use `filterNot (_ == x)` instead")
  def - [B >: A](x: B): List[B] = {
    val b = new ListBuffer[B]
    var these = this
    while (!these.isEmpty) {
      if (these.head != x) b += these.head
      these = these.tail
    }
    b.toList
  }

  /** <p>
   *    Sort the list according to the comparison function
   *    `lt(e1: a, e2: a) =&gt; Boolean`,
   *    which should be true iff `e1` precedes
   *    `e2` in the desired ordering.
   *  !!! todo: move sorting to IterableLike
   *  </p>
   *
   *  @param lt the comparison function
   *  @return   a list sorted according to the comparison function
   *            `lt(e1: a, e2: a) =&gt; Boolean`.
   *  @ex <pre>
   *    List("Steve", "Tom", "John", "Bob")
   *      .sort((e1, e2) => (e1 compareTo e2) &lt; 0) =
   *    List("Bob", "John", "Steve", "Tom")</pre>
   */
  @deprecated("use `sortWith' instead")
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
  // Removal of equals method here might lead to an infinite recusion similar to IntMap.equals.
  override def equals(that: Any) = that match {
    case that1: Seq[_] => that1.isEmpty
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

/** $factoryInfo */
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
  @deprecated("use `iterate' instead")
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
  @deprecated("use `fill' instead")
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
  @deprecated("use `xss.flatten' instead")
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
  @deprecated("use `xs.unzip' instead")
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
  @deprecated("use `xs.unzip' instead")
  def unzip[A,B](xs: Iterable[(A,B)]): (List[A], List[B]) =
      xs.foldRight[(List[A], List[B])]((Nil, Nil)) {
        case ((x, y), (xs, ys)) => (x :: xs, y :: ys)
      }

  /**
   * Returns the `Left` values in the given `Iterable`
   * of `Either`s.
   */
  @deprecated("use `xs partialMap { case Left(x: A) => x }' instead")
  def lefts[A, B](es: Iterable[Either[A, B]]) =
    es.foldRight[List[A]](Nil)((e, as) => e match {
      case Left(a) => a :: as
      case Right(_) => as
    })

  /**
   * Returns the `Right` values in the given`Iterable` of  `Either`s.
   */
  @deprecated("use `xs partialMap { case Right(x: B) => x }' instead")
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
  @deprecated("use `Either.separate' instead")
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
  @deprecated("use `it.toList' instead")
  def fromIterator[A](it: Iterator[A]): List[A] = it.toList

  /** Converts an array into a list.
   *
   *  @param arr the array to convert
   *  @return    a list that contains the same elements than `arr`
   *             in the same order
   */
  @deprecated("use `array.toList' instead")
  def fromArray[A](arr: Array[A]): List[A] = fromArray(arr, 0, arr.length)

  /** Converts a range of an array into a list.
   *
   *  @param arr   the array to convert
   *  @param start the first index to consider
   *  @param len   the lenght of the range to convert
   *  @return      a list that contains the same elements than `arr`
   *               in the same order
   */
  @deprecated("use `array.view(start, end).toList' instead")
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
  @deprecated("use `str.split(separator).toList' instead")
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
  @deprecated("use `xs.mkString' instead")
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
  @deprecated("use `xs.mapConserve(f)' instead")
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
  @deprecated("use `(xs, ys).zipped.map(f)' instead")
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
  @deprecated("use `(xs, ys, zs).zipped.map(f)' instead")
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
  @deprecated("use `(xs, ys).zipped.forall(f)' instead")
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
  @deprecated("use `(xs, ys).zipped.exists(f)' instead")
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
  @deprecated("use `xss.transpose' instead")
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

