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

/** A class representing an ordered collection of elements of type
 *  <code>a</code>. This class comes with two implementing case
 *  classes <code>scala.Nil</code> and <code>scala.::</code> that
 *  implement the abstract members <code>isEmpty</code>,
 *  <code>head</code> and <code>tail</code>.
 *
 *  @author  Martin Odersky and others
 *  @version 2.8
 */
sealed trait List[@specialized +A] extends LinearSeq[A] 
                                  with Product 
                                  with GenericTraversableTemplate[A, List]
                                  with LinearSeqOptimized[A, List[A]] {
  override def companion: GenericCompanion[List] = List

  import scala.collection.{Iterable, Traversable, Seq}

  /** Returns true if the list does not contain any elements.
   *  @return <code>true</code>, iff the list is empty.
   */
  def isEmpty: Boolean

  /** Returns this first element of the list.
   *
   *  @return the first element of this list.
   *  @throws Predef.NoSuchElementException if the list is empty.
   */
  def head: A

  /** Returns this list without its first element.
   *
   *  @return this list without its first element.
   *  @throws Predef.NoSuchElementException if the list is empty.
   */
  def tail: List[A]

  // New methods in List

  /** <p>
   *    Add an element <code>x</code> at the beginning of this list.
   *  </p>
   *
   *  @param x the element to prepend.
   *  @return  the list with <code>x</code> added at the beginning.
   *  @ex <code>1 :: List(2, 3) = List(2, 3).::(1) = List(1, 2, 3)</code>
   */
  def ::[@specialized B >: A] (x: B): List[B] =
    new scala.collection.immutable.::(x, this)

  /** <p>
   *    Returns a list resulting from the concatenation of the given
   *    list <code>prefix</code> and this list. 
   *  </p>
   *
   *  @param prefix the list to concatenate at the beginning of this list.
   *  @return the concatenation of the two lists.
   *  @ex <code>List(1, 2) ::: List(3, 4) = List(3, 4).:::(List(1, 2)) = List(1, 2, 3, 4)</code>
   */
  def :::[B >: A](prefix: List[B]): List[B] =
    if (isEmpty) prefix
    else (new ListBuffer[B] ++= prefix).prependToList(this)

  /** Reverse the given prefix and append the current list to that.
   *  This function is equivalent to an application of <code>reverse</code>
   *  on the prefix followed by a call to <code>:::</code>, but is more
   *  efficient.
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

  /** Apply a function to all the elements of the list, and return the
   *  reversed list of results. This is equivalent to a call to <code>map</code>
   *  followed by a call to <code>reverse</code>, but more efficient.
   *  !!! should we deprecate this? Why have reverseMap, but not filterMap or reverseFilter, say?
   *  @param f the function to apply to each elements.
   *  @return  the reversed list of results.
   */
  def reverseMap[B](f: A => B): List[B] = {
    @tailrec
    def loop(l: List[A], res: List[B]): List[B] = l match {
      case Nil => res
      case head :: tail => loop(tail, f(head) :: res)
    }
    loop(this, Nil)
  }

  /** Like xs map f, but returns <code>xs</code> unchanged if function
   *  <code>f</code> maps all elements to themselves (wrt ==).
   *  @note Unlike `map`, `mapConserve` is not tail-recursive.
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
  
  /** Create a new list which contains all elements of this list
   *  followed by all elements of Traversable `that'
   */
  override def ++[B >: A, That](xs: GenTraversableOnce[B])(implicit bf: CanBuildFrom[List[A], B, That]): That = {
    val b = bf(this)
    if (b.isInstanceOf[ListBuffer[_]]) (this ::: xs.toList).asInstanceOf[That]
    else super.++(xs)
  }

  /** Overrides the method in Iterable for efficiency.
   *
   *  @return  the list itself
   */
  override def toList: List[A] = this

  /** Returns the <code>n</code> first elements of this list, or else the whole 
   *  list, if it has less than <code>n</code> elements.

   *  @param n the number of elements to take.
   *  @return the <code>n</code> first elements of this list.
   */
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

  /** Returns the list without its <code>n</code> first elements.
   *  If this list has less than <code>n</code> elements, the empty list is returned.
   *
   *  @param n the number of elements to drop.
   *  @return the list without its <code>n</code> first elements.
   */
  override def drop(n: Int): List[A] = {
    var these = this
    var count = n
    while (!these.isEmpty && count > 0) {
      these = these.tail
      count -= 1
    }
    these
  }

  /** Returns the list with elements belonging to the given index range.
   *
   *  @param start the start position of the list slice.
   *  @param end   the end position (exclusive) of the list slice.
   *  @return the list with elements belonging to the given index range.
   */
  override def slice(start: Int, end: Int): List[A] = {
    var len = end
    if (start > 0) len -= start
    drop(start) take len
  }

  /** Returns the rightmost <code>n</code> elements from this list.
   *
   *  @param n the number of elements to take
   *  @return the suffix of length <code>n</code> of the list
   */
  override def takeRight(n: Int): List[A] = {
    @tailrec
    def loop(lead: List[A], lag: List[A]): List[A] = lead match {
      case Nil => lag
      case _ :: tail => loop(tail, lag.tail)
    }
    loop(drop(n), this)
  }
  
  // dropRight is inherited from Stream

  /** Split the list at a given point and return the two parts thus
   *  created.
   *
   *  @param n the position at which to split
   *  @return  a pair of lists composed of the first <code>n</code>
   *           elements, and the other elements.
   */
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

  /** Returns the longest prefix of this list whose elements satisfy
   *  the predicate <code>p</code>.
   *
   *  @param p the test predicate.
   *  @return  the longest prefix of this list whose elements satisfy
   *           the predicate <code>p</code>.
   */
  override def takeWhile(p: A => Boolean): List[A] = {
    val b = new ListBuffer[A]
    var these = this
    while (!these.isEmpty && p(these.head)) {
      b += these.head
      these = these.tail
    }
    b.toList
  }

  /** Returns the longest suffix of this list whose first element
   *  does not satisfy the predicate <code>p</code>.
   *
   *  @param p the test predicate.
   *  @return  the longest suffix of the list whose first element
   *           does not satisfy the predicate <code>p</code>.
   */
  override def dropWhile(p: A => Boolean): List[A] = {
    @tailrec
    def loop(xs: List[A]): List[A] =
      if (xs.isEmpty || !p(xs.head)) xs
      else loop(xs.tail)

    loop(this)
  }

  /** Returns the longest prefix of the list whose elements all satisfy
   *  the given predicate, and the rest of the list.
   *
   *  @param p the test predicate
   *  @return  a pair consisting of the longest prefix of the list whose
   *           elements all satisfy <code>p</code>, and the rest of the list.
   */
  override def span(p: A => Boolean): (List[A], List[A]) = {
    val b = new ListBuffer[A]
    var these = this
    while (!these.isEmpty && p(these.head)) {
      b += these.head
      these = these.tail
    }
    (b.toList, these)
  }

  /** A list consisting of all elements of this list in reverse order.
   */
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

  // !!! todo: work in patch

  /** Computes the difference between this list and the given list
   *  <code>that</code>.
   *
   *  @param that the list of elements to remove from this list.
   *  @return     this list without the elements of the given list
   *              <code>that</code>.
   */
  @deprecated("use `diff' instead")
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
   *  <code>x</code>.
   *
   *  @param x    the object to remove from this list.
   *  @return     this list without occurrences of the given object
   *              <code>x</code>.
   */
  @deprecated("use `diff' instead")
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
   *    <code>&lt;(e1: a, e2: a) =&gt; Boolean</code>,
   *    which should be true iff <code>e1</code> is smaller than
   *    <code>e2</code>.
   *  !!! todo: move sorting to IterableLike
   *  </p>
   *
   *  @param lt the comparison function
   *  @return   a list sorted according to the comparison function
   *            <code>&lt;(e1: a, e2: a) =&gt; Boolean</code>.
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
 */
@SerialVersionUID(0 - 8256821097970055419L)
case object Nil extends List[Nothing] {
  override def isEmpty = true
  override def head: Nothing =
    throw new NoSuchElementException("head of empty list")
  override def tail: List[Nothing] =
    throw new NoSuchElementException("tail of empty list")
  // Removal of equals method here might lead to an infinite recusion similar to IntMap.equals.
  override def equals(that: Any) = that match {
    case that1: Seq[_] => that1.isEmpty
    case _ => false
  }
}

/** A non empty list characterized by a head and a tail.
 *
 *  @author  Martin Odersky
 *  @version 1.0, 15/07/2003
 */
@SerialVersionUID(0L - 8476791151983527571L)
final case class ::[@specialized B](private var hd: B, private[scala] var tl: List[B]) extends List[B] {
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

/** This object provides methods for creating specialized lists, and for
 *  transforming special kinds of lists (e.g. lists of lists).
 *
 *  @author  Martin Odersky
 *  @version 2.8
 */
object List extends SeqFactory[List] {
  
  import collection.{Iterable, Seq}

  implicit def builderFactory[A]: CanBuildFrom[Coll, A, List[A]] = 
    new GenericCanBuildFrom[A] {
      override def apply() = newBuilder[A]
    }
  def newBuilder[A]: Builder[A, List[A]] = new ListBuffer[A]

  override def empty[A]: List[A] = Nil

  override def apply[A](xs: A*): List[A] = xs.toList

  /** Create a sorted list with element values
   * <code>v<sub>n+1</sub> = step(v<sub>n</sub>)</code>
   * where <code>v<sub>0</sub> = start</code>
   * and elements are in the range between <code>start</code> (inclusive)
   * and <code>end</code> (exclusive)
   *
   *  @param start the start value of the list
   *  @param end  the end value of the list
   *  @param step the increment function of the list, which given <code>v<sub>n</sub></code>,
   *              computes <code>v<sub>n+1</sub></code>. Must be monotonically increasing
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
   * Returns the <code>Left</code> values in the given <code>Iterable</code>
   * of <code>Either</code>s.
   */
  @deprecated("use `Either.lefts' instead")
  def lefts[A, B](es: Iterable[Either[A, B]]) = 
    es.foldRight[List[A]](Nil)((e, as) => e match {
      case Left(a) => a :: as
      case Right(_) => as
    })     
 
  /**
   * Returns the <code>Right</code> values in the given<code>Iterable</code> of  <code>Either</code>s.
   */
  @deprecated("use `Either.rights' instead")
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
  def separate[A,B](es: Iterable[Either[A,B]]): (List[A], List[B]) =
      es.foldRight[(List[A], List[B])]((Nil, Nil)) {
      case (Left(a), (lefts, rights)) => (a :: lefts, rights)
      case (Right(b), (lefts, rights)) => (lefts, b :: rights)
    }

  /** Converts an iterator to a list.
   *
   *  @param it the iterator to convert
   *  @return   a list that contains the elements returned by successive
   *            calls to <code>it.next</code>
   */
  @deprecated("use `it.toList' instead")
  def fromIterator[A](it: Iterator[A]): List[A] = it.toList

  /** Converts an array into a list.
   *
   *  @param arr the array to convert
   *  @return    a list that contains the same elements than <code>arr</code>
   *             in the same order
   */
  @deprecated("use `array.toList' instead")
  def fromArray[A](arr: Array[A]): List[A] = fromArray(arr, 0, arr.length)

  /** Converts a range of an array into a list.
   *
   *  @param arr   the array to convert
   *  @param start the first index to consider
   *  @param len   the length of the range to convert
   *  @return      a list that contains the same elements than <code>arr</code>
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

  /** Returns the given string as a list of characters.
   *
   *  @param str the string to convert.
   *  @return    the string as a list of characters.
   */
  @deprecated("use `str.toList' instead")
  def fromString(str: String): List[Char] = str.toList

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

  /** Like xs map f, but returns <code>xs</code> unchanged if function
   *  <code>f</code> maps all elements to themselves.
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

  /** Returns the list resulting from applying the given function <code>f</code>
   *  to corresponding elements of the argument lists.
   *  @param f function to apply to each pair of elements.
   *  @return <code>[f(a0,b0), ..., f(an,bn)]</code> if the lists are 
   *          <code>[a0, ..., ak]</code>, <code>[b0, ..., bl]</code> and
   *          <code>n = min(k,l)</code>
   */
  @deprecated("use `(xs, ys).map(f)' instead")
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
   *  <code>f</code> to corresponding elements of the argument lists.
   *
   *  @param f function to apply to each pair of elements.
   *  @return  <code>[f(a<sub>0</sub>,b<sub>0</sub>,c<sub>0</sub>),
   *           ..., f(a<sub>n</sub>,b<sub>n</sub>,c<sub>n</sub>)]</code>
   *           if the lists are <code>[a<sub>0</sub>, ..., a<sub>k</sub>]</code>,
   *           <code>[b<sub>0</sub>, ..., b<sub>l</sub>]</code>,
   *           <code>[c<sub>0</sub>, ..., c<sub>m</sub>]</code> and
   *           <code>n = min(k,l,m)</code>
   */
  @deprecated("use `(xs, ys, zs).map(f)' instead")
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

  /** Tests whether the given predicate <code>p</code> holds 
   *  for all corresponding elements of the argument lists.
   *
   *  @param p function to apply to each pair of elements.
   *  @return  <code>(p(a<sub>0</sub>,b<sub>0</sub>) &amp;&amp;
   *           ... &amp;&amp; p(a<sub>n</sub>,b<sub>n</sub>))]</code>
   *           if the lists are <code>[a<sub>0</sub>, ..., a<sub>k</sub>]</code>;
   *           <code>[b<sub>0</sub>, ..., b<sub>l</sub>]</code>
   *           and <code>n = min(k,l)</code>
   */
  @deprecated("use `(xs, ys).forall(f)' instead")
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

  /** Tests whether the given predicate <code>p</code> holds
   *  for some corresponding elements of the argument lists.
   *
   *  @param p function to apply to each pair of elements.
   *  @return  <code>n != 0 &amp;&amp; (p(a<sub>0</sub>,b<sub>0</sub>) ||
   *           ... || p(a<sub>n</sub>,b<sub>n</sub>))]</code> if the lists are
   *           <code>[a<sub>0</sub>, ..., a<sub>k</sub>]</code>,
   *           <code>[b<sub>0</sub>, ..., b<sub>l</sub>]</code> and
   *           <code>n = min(k,l)</code>
   */
  @deprecated("use `(xs, ys).exists(f)' instead")
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
  @deprecated("use p`xss.transpose' instead")
  def transpose[A](xss: List[List[A]]): List[List[A]] = {
    val buf = new ListBuffer[List[A]]
    var yss = xss
    while (!yss.head.isEmpty) {
      buf += (yss map (_.head))
      yss = (yss map (_.tail))
    }
    buf.toList
  }

  /** Lists with ordered elements are ordered
  implicit def list2ordered[a <% Ordered[a]](x: List[a]): Ordered[List[a]] = new Ordered[List[a]] {
    def compare [b >: List[a] <% Ordered[b]](y: b): Int = y match {
      case y1: List[a] => compareLists(x, y1);
      case _ => -(y compare x)
    }
    private def compareLists(xs: List[a], ys: List[a]): Int = {
      if (xs.isEmpty && ys.isEmpty) 0
      else if (xs.isEmpty) -1
      else if (ys.isEmpty) 1
      else {
        val s = xs.head compare ys.head;
        if (s != 0) s
        else compareLists(xs.tail, ys.tail)
      }
    }
  }
   */
}

/** Only used for list serialization */
@SerialVersionUID(0L - 8476791151975527571L)
private[scala] case object ListSerializeEnd

