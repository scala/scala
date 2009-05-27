/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

import scala.collection.mutable.ListBuffer
import Predef._

/** This object provides methods for creating specialized lists, and for
 *  transforming special kinds of lists (e.g. lists of lists).
 *
 *  @author  Martin Odersky and others
 *  @version 1.0, 15/07/2003
 */
object List {

  /** Create a list with given elements.
   *
   *  @param xs the elements to put in the list
   *  @return the list containing elements xs.
   */
  def apply[A](xs: A*): List[A] = xs.toList

  /** for unapply matching
   */
  def unapplySeq[A](x: List[A]): Some[List[A]] = Some(x)

  /** Create a sorted list of all integers in a range.
   *
   *  @param from the start value of the list
   *  @param end the end value of the list
   *  @return the sorted list of all integers in range [from;end).
   */
  def range(from: Int, end: Int): List[Int] =
    range(from, end, 1)

  /** Create a sorted list of all integers in a range.
   *
   *  @param from the start value of the list
   *  @param end  the end value of the list
   *  @param step the increment value of the list
   *  @return     the sorted list of all integers in range [from;end).
   */
  def range(from: Int, end: Int, step: Int): List[Int] = {
    val b = new ListBuffer[Int]
    var i = from
    while (i < end) {
      b += i
      i += step
    }
    b.toList
  }

  /** Create a sorted list of all integers in a range.
   *
   *  @param from the start value of the list
   *  @param end  the end value of the list
   *  @param step the increment function of the list
   *  @return     the sorted list of all integers in range [from;end).
   */
  def range(from: Int, end: Int, step: Int => Int): List[Int] = {
    val b = new ListBuffer[Int]
    var i = from
    while (i < end) {
      b += i
      i += step(i)
    }
    b.toList
  }

  /** Create a list containing several copies of an element.
   *
   *  @param n    the length of the resulting list
   *  @param elem the element composing the resulting list
   *  @return     a list composed of n elements all equal to elem
   */
  def make[A](n: Int, elem: A): List[A] = {
    val b = new ListBuffer[A]
    var i = 0
    while (i < n) {
      b += elem
      i += 1
    }
    b.toList
  }

  /** Create a list by applying a function to successive integers.
   *
   *  @param n     the length of the resulting list
   *  @param maker the procedure which, given an integer <code>n</code>,
   *               returns the nth element of the resulting list, where
   *               <code>n</code> is in interval <code>[0;n)</code>.
   *  @return      the list obtained by applying the maker function to
   *               successive integers from 0 to n (exclusive).
   */
  def tabulate[A](n: Int, maker: Int => A): List[A] = {
    val b = new ListBuffer[A]
    var i = 0
    while (i < n) {
      b += maker(i)
      i += 1
    }
    b.toList
  }

  /** Concatenate all the elements of a given list of lists.
   *
   *  @param xss the list of lists that are to be concatenated
   *  @return    the concatenation of all the lists
   */
  def flatten[A](xss: List[List[A]]): List[A] = concat(xss: _*)

  /** Concatenate all the argument lists into a single list.
   *
   *  @param xss the lists that are to be concatenated
   *  @return the concatenation of all the lists
   */
  def concat[A](xss: List[A]*): List[A] = {
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

  /** Transforms a list of pair into a pair of lists.
   *
   *  @param xs the list of pairs to unzip
   *  @return a pair of lists: the first list in the pair contains the list
   */
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

  /** Converts an iterator to a list.
   *
   *  @param it the iterator to convert
   *  @return   a list that contains the elements returned by successive
   *            calls to <code>it.next</code>
   */
  def fromIterator[A](it: Iterator[A]): List[A] = it.toList

  /** Converts an array into a list.
   *
   *  @param arr the array to convert
   *  @return    a list that contains the same elements than <code>arr</code>
   *             in the same order
   */
  def fromArray[A](arr: Array[A]): List[A] = fromArray(arr, 0, arr.length)

  /** Converts a range of an array into a list.
   *
   *  @param arr   the array to convert
   *  @param start the first index to consider
   *  @param len   the lenght of the range to convert
   *  @return      a list that contains the same elements than <code>arr</code>
   *               in the same order
   */
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
   *  @deprecated use <code>str.toList</code> instead
   */
  @deprecated def fromString(str: String): List[Char] =
    str.toList

  /** Returns the given list of characters as a string.
   *
   *  @param xs the list to convert.
   *  @return   the list in form of a string.
   */
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
   *
   *  @param xs ...
   *  @param f  ...
   *  @return   ...
   */
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
   *
   *  @param f function to apply to each pair of elements.
   *  @return <code>[f(a0,b0), ..., f(an,bn)]</code> if the lists are
   *          <code>[a0, ..., ak]</code>, <code>[b0, ..., bl]</code> and
   *          <code>n = min(k,l)</code>
   */
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
  def transpose[A](xss: List[List[A]]): List[List[A]] =
    if (xss.head.isEmpty) List()
    else (xss map (xs => xs.head)) :: transpose(xss map (xs => xs.tail))

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

/** A class representing an ordered collection of elements of type
 *  <code>a</code>. This class comes with two implementing case
 *  classes <code>scala.Nil</code> and <code>scala.::</code> that
 *  implement the abstract members <code>isEmpty</code>,
 *  <code>head</code> and <code>tail</code>.
 *
 *  @author  Martin Odersky and others
 *  @version 1.0, 16/07/2003
 */
sealed abstract class List[+A] extends Seq[A] {

  /** Returns true if the list does not contain any elements.
   *  @return <code>true</code>, iff the list is empty.
   */
  override def isEmpty: Boolean

  /** Returns this first element of the list.
   *
   *  @return the first element of this list.
   *  @throws Predef.NoSuchElementException if the list is empty.
   */
  def head: A

  /** returns length - l, without calling length
   */
  override def lengthCompare(l: Int) = {
    if (isEmpty) 0 - l
    else if (l <= 0) 1
    else tail.lengthCompare(l - 1)
  }

  /** Returns this list without its first element.
   *
   *  @return this list without its first element.
   *  @throws Predef.NoSuchElementException if the list is empty.
   */
  def tail: List[A]

  /** <p>
   *    Add an element <code>x</code> at the beginning of this list.
   *  </p>
   *
   *  @param x the element to append.
   *  @return  the list with <code>x</code> appended at the beginning.
   *  @ex <code>1 :: List(2, 3) = List(2, 3).::(1) = List(1, 2, 3)</code>
   */
  def ::[B >: A] (x: B): List[B] =
    new scala.::(x, this)

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
    else {
      val b = new ListBuffer[B]
      var those = prefix
      while (!those.isEmpty) {
        b += those.head
        those = those.tail
      }
      b.prependToList(this)
    }

  /** Reverse the given prefix and append the current list to that.
   *  This function is equivalent to an application of <code>reverse</code>
   *  on the prefix followed by a call to <code>:::</code>, but more
   *  efficient (and tail recursive).
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

  /** Returns the number of elements in the list.
   *
   *  @return the number of elements in the list.
   */
  def length: Int = {
    var these = this
    var len = 0
    while (!these.isEmpty) {
      len += 1
      these = these.tail
    }
    len
  }

  /** Creates a list with all indices in the list. This is
   *  equivalent to a call to <code>List.range(0, xs.length)</code>.
   *
   *  @return a list of all indices in the list.
   */
  def indices: List[Int] = {
    val b = new ListBuffer[Int]
    var i = 0
    var these = this
    while (!these.isEmpty) {
      b += i
      i += 1
      these = these.tail
    }
    b.toList
  }

  /** Returns the elements in the list as an iterator
   *
   *  @return an iterator on the list elements.
   */
  override def iterator: Iterator[A] = new Iterator[A] {
    var these = List.this
    def hasNext: Boolean = !these.isEmpty
    def next: A =
      if (!hasNext)
        throw new NoSuchElementException("next on empty Iterator")
      else {
        val result = these.head; these = these.tail; result
      }
    override def toList: List[A] = these
  }

  /** Overrides the method in Iterable for efficiency.
   *
   *  @return  the list itself
   */
  override def toList: List[A] = this

  /** Returns the list without its last element.
   *
   *  @return the list without its last element.
   *  @throws Predef.UnsupportedOperationException if the list is empty.
   */
  def init: List[A] =
    if (isEmpty) throw new UnsupportedOperationException("Nil.init")
    else {
      val b = new ListBuffer[A]
      var elem = head
      var next = tail
      while (!next.isEmpty) {
        b += elem
        elem = next.head
        next = next.tail
      }
      b.toList
    }

  /** Returns the last element of this list.
   *
   *  @return the last element of the list.
   *  @throws Predef.UnsupportedOperationException if the list is empty.
   */
  override def last: A =
    if (isEmpty) throw new Predef.NoSuchElementException("Nil.last")
    else {
      var cur = this
      var next = this.tail
      while (!next.isEmpty) {
        cur = next
        next = next.tail
      }
      cur.head
    }

  /** Returns the <code>n</code> first elements of this list, or else the whole
   *  list, if it has less than <code>n</code> elements.
   *
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

  /** Returns the list with elements belonging to the given index range.
   *
   *  @param start the start position of the list slice.
   *  @param end   the end position (exclusive) of the list slice.
   *  @return the list with elements belonging to the given index range.
   */
  override def slice(start: Int, end: Int): List[A] = {
    val s = start max 0
    val e = end min this.length
    drop(s) take (e - s)
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

  /** Returns the rightmost <code>n</code> elements from this list.
   *
   *  @param n the number of elements to take
   *  @return the suffix of length <code>n</code> of the list
   */
  def takeRight(n: Int): List[A] = {
    def loop(lead: List[A], lag: List[A]): List[A] = lead match {
      case Nil => lag
      case _ :: tail => loop(tail, lag.tail)
    }
    loop(drop(n), this)
  }

  /** Returns the list wihout its rightmost <code>n</code> elements.
   *
   *  @param n the number of elements to take
   *  @return the suffix of length <code>n</code> of the list
   */
  def dropRight(n: Int): List[A] = {
    def loop(lead: List[A], lag: List[A]): List[A] = lead match {
      case Nil => Nil
      case _ :: tail => lag.head :: loop(tail, lag.tail)
    }
    loop(drop(n), this)
  }

  /** Split the list at a given point and return the two parts thus
   *  created.
   *
   *  @param n the position at which to split
   *  @return  a pair of lists composed of the first <code>n</code>
   *           elements, and the other elements.
   */
  def splitAt(n: Int): (List[A], List[A]) = {
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
  override def dropWhile(p: A => Boolean): List[A] =
    if (isEmpty || !p(head)) this
    else tail dropWhile p

  /** Returns the longest prefix of the list whose elements all satisfy
   *  the given predicate, and the rest of the list.
   *
   *  @param p the test predicate
   *  @return  a pair consisting of the longest prefix of the list whose
   *           elements all satisfy <code>p</code>, and the rest of the list.
   */
  def span(p: A => Boolean): (List[A], List[A]) = {
    val b = new ListBuffer[A]
    var these = this
    while (!these.isEmpty && p(these.head)) {
      b += these.head
      these = these.tail
    }
    (b.toList, these)
  }

  /** Like <code>span</code> but with the predicate inverted.
   */
  def break(p: A => Boolean): (List[A], List[A]) = span { x => !p(x) }

  /** Returns the <code>n</code>-th element of this list. The first element
   *  (head of the list) is at position 0.
   *
   *  @param n index of the element to return
   *  @return  the element at position <code>n</code> in this list.
   *  @throws Predef.NoSuchElementException if the list is too short.
   */
  def apply(n: Int): A = drop(n).head

  /** Returns the list resulting from applying the given function <code>f</code> to each
   *  element of this list.
   *
   *  @param f function to apply to each element.
   *  @return <code>[f(a0), ..., f(an)]</code> if this list is <code>[a0, ..., an]</code>.
   */
  final override def map[B](f: A => B): List[B] = {
    val b = new ListBuffer[B]
    var these = this
    while (!these.isEmpty) {
      b += f(these.head)
      these = these.tail
    }
    b.toList
  }

  /** Apply a function to all the elements of the list, and return the
   *  reversed list of results. This is equivalent to a call to <code>map</code>
   *  followed by a call to <code>reverse</code>, but more efficient.
   *
   *  @param f the function to apply to each elements.
   *  @return  the reversed list of results.
   */
  def reverseMap[B](f: A => B): List[B] = {
    def loop(l: List[A], res: List[B]): List[B] = l match {
      case Nil => res
      case head :: tail => loop(tail, f(head) :: res)
    }
    loop(this, Nil)
  }

  /** Apply the given function <code>f</code> to each element of this list
   *  (while respecting the order of the elements).
   *
   *  @param f the treatment to apply to each element.
   */
  final override def foreach(f: A => Unit) {
    var these = this
    while (!these.isEmpty) {
      f(these.head)
      these = these.tail
    }
  }

  /** Returns all the elements of this list that satisfy the
   *  predicate <code>p</code>. The order of the elements is preserved.
   *  It is guarenteed that the receiver list itself is returned iff all its
   *  elements satisfy the predicate `p'. Hence the following equality is valid:
   *
   *  (xs filter p) eq xs  ==  xs forall p
   *
   *  @param p the predicate used to filter the list.
   *  @return the elements of this list satisfying <code>p</code>.
   */
  final override def filter(p: A => Boolean): List[A] = {
    // return same list if all elements satisfy p
    var these = this
    while (!these.isEmpty && p(these.head)) {
      these = these.tail
    }
    if (these.isEmpty) this
    else {
      val b = new ListBuffer[A]
      var these1 = this
      while (these1 ne these) {
        b += these1.head
        these1 = these1.tail
      }

      these = these.tail // prevent the second evaluation of the predicate
                         // on the element on which it first failed
      while (!these.isEmpty) {
        if (p(these.head)) b += these.head
        these = these.tail
      }
      b.toList
    }
  }

//  final def filterMap[B](f: PartialFunction[A, B]): List[B] =
//    this filter f.isDefinedAt map f

  /** Removes all elements of the list which satisfy the predicate
   *  <code>p</code>. This is like <code>filter</code> with the
   *  predicate inversed.
   *
   *  @param p the predicate to use to test elements
   *  @return  the list without all elements which satisfy <code>p</code>
   */
  def remove(p: A => Boolean): List[A] = filter (x => !p(x))

  /** Partition the list in two sub-lists according to a predicate.
   *
   *  @param p the predicate on which to partition
   *  @return  a pair of lists: the list of all elements which satisfy
   *           <code>p</code> and the list of all elements which do not.
   *           The relative order of the elements in the sub-lists is the
   *           same as in the original list.
   */
  override def partition(p: A => Boolean): (List[A], List[A]) = {
    val btrue = new ListBuffer[A]
    val bfalse = new ListBuffer[A]
    var these = this
    while (!these.isEmpty) {
      (if (p(these.head)) btrue else bfalse) += these.head
      these = these.tail
    }
    (btrue.toList, bfalse.toList)
  }

  /** <p>
   *    Sort the list according to the comparison function
   *    <code>&lt;(e1: a, e2: a) =&gt; Boolean</code>,
   *    which should be true iff <code>e1</code> is smaller than
   *    <code>e2</code>.
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


  /** Count the number of elements in the list which satisfy a predicate.
   *
   *  @param p the predicate for which to count
   *  @return  the number of elements satisfying the predicate <code>p</code>.
   */
  def count(p: A => Boolean): Int = {
    var cnt = 0
    var these = this
    while (!these.isEmpty) {
      if (p(these.head)) cnt += 1
      these = these.tail
    }
    cnt
  }

  /** Tests if the predicate <code>p</code> is satisfied by all elements
   *  in this list.
   *
   *  @param p the test predicate.
   *  @return  <code>true</code> iff all elements of this list satisfy the
   *           predicate <code>p</code>.
   */
  override def forall(p: A => Boolean): Boolean = {
    var these = this
    while (!these.isEmpty) {
      if (!p(these.head)) return false
      these = these.tail
    }
    true
  }

  /** Tests the existence in this list of an element that satisfies the
   *  predicate <code>p</code>.
   *
   *  @param p the test predicate.
   *  @return  <code>true</code> iff there exists an element in this list that
   *           satisfies the predicate <code>p</code>.
   */
  override def exists(p: A => Boolean): Boolean = {
    var these = this
    while (!these.isEmpty) {
      if (p(these.head)) return true
      these = these.tail
    }
    false
  }

  /** Find and return the first element of the list satisfying a
   *  predicate, if any.
   *
   *  @param p the predicate
   *  @return the first element in the list satisfying <code>p</code>,
   *  or <code>None</code> if none exists.
   */
  override def find(p: A => Boolean): Option[A] = {
    var these = this
    while (!these.isEmpty) {
      if (p(these.head)) return Some(these.head)
      these = these.tail
    }
    None
  }

  /** Combines the elements of this list together using the binary
   *  function <code>f</code>, from left to right, and starting with
   *  the value <code>z</code>.
   *
   *  @return <code>f(... (f(f(z, a<sub>0</sub>), a<sub>1</sub>) ...),
   *          a<sub>n</sub>)</code> if the list is
   *          <code>[a<sub>0</sub>, a<sub>1</sub>, ..., a<sub>n</sub>]</code>.
   */
  override def foldLeft[B](z: B)(f: (B, A) => B): B = {
    var acc = z
    var these = this
    while (!these.isEmpty) {
      acc = f(acc, these.head)
      these = these.tail
    }
    acc
  }

  /** Combines the elements of this list together using the binary
   *  function <code>f</code>, from right to left, and starting with
   *  the value <code>z</code>.
   *
   *  @return <code>f(a<sub>0</sub>, f(a<sub>1</sub>, f(..., f(a<sub>n</sub>, z)...)))</code>
   *          if the list is <code>[a<sub>0</sub>, a1, ..., a<sub>n</sub>]</code>.
   */
  override def foldRight[B](z: B)(f: (A, B) => B): B = this match {
    case Nil => z
    case x :: xs => f(x, xs.foldRight(z)(f))
  }

  /** Combines the elements of this list together using the binary
   *  operator <code>op</code>, from left to right
   *  @param op  The operator to apply
   *  @return <code>op(... op(a<sub>0</sub>,a<sub>1</sub>), ..., a<sub>n</sub>)</code>
      if the list has elements
   *          <code>a<sub>0</sub>, a<sub>1</sub>, ..., a<sub>n</sub></code>.
   *  @throws Predef.UnsupportedOperationException if the list is empty.
   */
  override def reduceLeft[B >: A](f: (B, B) => B): B = this match {
    case Nil => throw new UnsupportedOperationException("Nil.reduceLeft")
    case x :: xs => ((xs: List[B]) foldLeft (x: B))(f)
  }

  /** Combines the elements of this list together using the binary
   *  operator <code>op</code>, from right to left
   *  @param op  The operator to apply
   *
   *  @return <code>a<sub>0</sub> op (... op (a<sub>n-1</sub> op a<sub>n</sub>)...)</code>
   *          if the list has elements <code>a<sub>0</sub>, a<sub>1</sub>, ...,
   *          a<sub>n</sub></code>.
   *
   *  @throws Predef.UnsupportedOperationException if the list is empty.
   */
  override def reduceRight[B >: A](f: (B, B) => B): B = this match {
    case Nil => throw new UnsupportedOperationException("Nil.reduceRight")
    case x :: Nil => x: B
    case x :: xs => f(x, xs reduceRight f)
  }

  /** Applies the given function <code>f</code> to each element of
   *  this list, then concatenates the results.
   *
   *  @param f the function to apply on each element.
   *  @return  <code>f(a<sub>0</sub>) ::: ... ::: f(a<sub>n</sub>)</code> if
   *           this list is <code>[a<sub>0</sub>, ..., a<sub>n</sub>]</code>.
   */
  final override def flatMap[B](f: A => Iterable[B]): List[B] = {
    val b = new ListBuffer[B]
    var these = this
    while (!these.isEmpty) {
      var those = f(these.head).iterator
      while (those.hasNext) {
        b += those.next
      }
      these = these.tail
    }
    b.toList
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

  /** Returns a list formed from this list and the specified list
   *  <code>that</code> by associating each element of the former with
   *  the element at the same position in the latter.
   *  If one of the two lists is longer than the other, its remaining elements are ignored.
   *
   *  @return     <code>List((a<sub>0</sub>,b<sub>0</sub>), ...,
   *              (a<sub>min(m,n)</sub>,b<sub>min(m,n)</sub>))</code> when
   *              <code>List(a<sub>0</sub>, ..., a<sub>m</sub>)
   *              zip List(b<sub>0</sub>, ..., b<sub>n</sub>)</code> is invoked.
   */
  def zip[B](that: List[B]): List[(A, B)] = {
    val b = new ListBuffer[(A, B)]
    var these = this
    var those = that
    while (!these.isEmpty && !those.isEmpty) {
      b += (these.head, those.head)
      these = these.tail
      those = those.tail
    }
    b.toList
  }

  /** Returns a list that pairs each element of this list
   *  with its index, counting from 0.
   *
   *  @return      the list <code>List((a<sub>0</sub>,0), (a<sub>1</sub>,1), ...)</code>
   *               where <code>a<sub>i</sub></code> are the elements of this list.
   */
  def zipWithIndex: List[(A, Int)] = {
    val b = new ListBuffer[(A, Int)]
    var these = this
    var idx = 0

    while(!these.isEmpty) {
      b += (these.head, idx)
      these = these.tail
      idx += 1
    }

    b.toList
  }

  /** Returns a list formed from this list and the specified list
   *  <code>that</code> by associating each element of the former with
   *  the element at the same position in the latter.
   *
   *  @param that     list <code>that</code> may have a different length
   *                  as the self list.
   *  @param thisElem element <code>thisElem</code> is used to fill up the
   *                  resulting list if the self list is shorter than
   *                  <code>that</code>
   *  @param thatElem element <code>thatElem</code> is used to fill up the
   *                  resulting list if <code>that</code> is shorter than
   *                  the self list
   *  @return         <code>List((a<sub>0</sub>,b<sub>0</sub>), ...,
   *                  (a<sub>n</sub>,b<sub>n</sub>), (elem,b<sub>n+1</sub>),
   *                  ..., {elem,b<sub>m</sub>})</code>
   *                  when <code>[a<sub>0</sub>, ..., a<sub>n</sub>] zip
   *                  [b<sub>0</sub>, ..., b<sub>m</sub>]</code> is
   *                  invoked where <code>m &gt; n</code>.
   */
  def zipAll[B, C >: A, D >: B](that: List[B], thisElem: C, thatElem: D): List[(C, D)] = {
    val b = new ListBuffer[(C, D)]
    var these = this
    var those = that
    while (!these.isEmpty && !those.isEmpty) {
      b += (these.head, those.head)
      these = these.tail
      those = those.tail
    }
    while (!these.isEmpty) {
      b += (these.head, thatElem)
      these = these.tail
    }
    while (!those.isEmpty) {
      b += (thisElem, those.head)
      those = those.tail
    }
    b.toList
  }

  /** Computes the union of this list and the given list
   *  <code>that</code>.
   *
   *  @param that the list of elements to add to the list.
   *  @return     a list without doubles containing the elements of this
   *              list and those of the given list <code>that</code>.
   */
  def union[B >: A](that: List[B]): List[B] = {
    val b = new ListBuffer[B]
    var these = this
    while (!these.isEmpty) {
      if (!that.contains(these.head)) b += these.head
      these = these.tail
    }
    b.prependToList(that)
  }

  /** Computes the difference between this list and the given list
   *  <code>that</code>.
   *
   *  @param that the list of elements to remove from this list.
   *  @return     this list without the elements of the given list
   *              <code>that</code>.
   */
  def diff[B >: A](that: List[B]): List[B] = {
    val b = new ListBuffer[B]
    var these = this
    while (!these.isEmpty) {
      if (!that.contains(these.head)) b += these.head
      these = these.tail
    }
    b.toList
  }

  def flatten[B](implicit f : A => Iterable[B]) : List[B] = {
    val buf = new ListBuffer[B]
    foreach(f(_).foreach(buf += _))
    buf.toList
  }


  /** Computes the intersection between this list and the given list
   *  <code>that</code>.
   *
   *  @param that the list to intersect.
   *  @return     the list of elements contained both in this list and
   *              in the given list <code>that</code>.
   */
  def intersect[B >: A](that: List[B]): List[B] = filter(x => that contains x)

  /** Removes redundant elements from the list. Uses the method <code>==</code>
   *  to decide if two elements are identical.
   *
   *  @return the list without doubles.
   */
  def removeDuplicates: List[A] = {
    val b = new ListBuffer[A]
    var these = this
    while (!these.isEmpty) {
      if (!these.tail.contains(these.head)) b += these.head
      these = these.tail
    }
    b.toList
  }

  override protected def stringPrefix = "List"
  override def projection = toStream
  override def toStream : Stream[A] = new Stream.Definite[A] {
    override def force : List[A] = List.this
    override def isEmpty = List.this.isEmpty
    override def head = List.this.head
    override def tail = List.this.tail.toStream
    protected def addDefinedElems(buf: StringBuilder, prefix: String): StringBuilder = if (!isEmpty) {
      var prefix0 = prefix
      var buf1 = buf.append(prefix0).append(head)
      prefix0 = ", "
      var tail0 = tail
      while (!tail0.isEmpty) {
        buf1 = buf.append(prefix0).append(tail0.head)
        tail0 = tail0.tail
      }
      buf1
    } else buf
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
  def head: Nothing =
    throw new NoSuchElementException("head of empty list")
  def tail: List[Nothing] =
    throw new NoSuchElementException("tail of empty list")
}

/** A non empty list characterized by a head and a tail.
 *
 *  @author  Martin Odersky
 *  @version 1.0, 15/07/2003
 */
@SerialVersionUID(0L - 8476791151983527571L)
final case class ::[B](private var hd: B, private[scala] var tl: List[B]) extends List[B] {
  def head : B = hd
  def tail : List[B] = tl
  override def isEmpty: Boolean = false
}

