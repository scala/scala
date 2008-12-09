/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Iterable.scala 15188 2008-05-24 15:01:02Z stepancheg $


package scalax.collection.generic.covartest

import scalax.collection.mutable.{Buffer, ArrayBuffer, ListBuffer}
import scalax.collection.immutable.{List, Nil, ::, Stream}
import util.control.Break._
import Iterable._

/** Collection classes mixing in this class provide a method
 *  <code>elements</code> which returns an iterator over all the
 *  elements contained in the collection.
 *
 *  @note If a collection has a known <code>size</code>, it should also sub-type <code>Collection</code>.
 *        Only potentially unbounded collections should directly sub-class <code>Iterable</code>.
 *  @author Matthias Zenger
 *  @author Martin Odersky
 *  @version 2.8
 */
trait IterableTemplate[+CC[+B] <: IterableTemplate[CC, B] with Iterable[B], +A] { self/*: CC[A]*/ =>

  /** The template itself seen as an instance of `CC[A]`.
   * @note: It would be logical to have a self type `CC[A]` instead, then we would not need
   * this method. Unfortunately, tyis runs afoul some pecularities in Scala's member resolution
   * algorithm: If the self type is a CC, then Iterable is one of its supertypes. Iterable
   * defines a number of concrete methods such as newBuilder which are abstract here.
   * The newBuilder method in Iterable[A] has type Builder[Iterable, A]. Because Scala
   * prefers concrete over abstract members, it is this newBuilder which is chosen, instead of
   * the abstract newBuilder in class IterableTemplate of type Builder[CC, A].
   * Even for concrete methods we have a problem because the last mixin in the parents of CC is
   * Iterable, not IterableTemplate. So resolution picks the version in Iterable, which returns
   * again an Iterable, not a CC, as would be required.
   * These problems would be avoided if Scala computed the type of a member as the glb of the types
   * all members in the class and its superclasses types.
   * I think overall this would be a better design.
   */
  protected[this] def thisCC: CC[A] = this.asInstanceOf[CC[A]]

  /** Creates a new iterator over all elements contained in this
   *  object.
   *
   *  @return the new iterator
   */
  def elements: Iterator[A]

  /** Create a new builder for this IterableType
   */
  def newBuilder[B]: Builder[CC, B]

  /** Is this collection empty? */
  def isEmpty: Boolean = !elements.hasNext

  /** returns true iff this collection has a bound size.
   *  Many APIs in this trait will not work on collections of
   *  unbound sizes.
   */
  def hasDefiniteSize = true

  /** Create a new sequence of type CC which contains all elements of this sequence
   *  followed by all elements of Iterable `that'
   */
  def ++[B >: A](that: Iterable[B]): CC[B] = {
    val b: Builder[CC, B] = (this: IterableTemplate[CC, A]).newBuilder[B]
    b ++= thisCC
    b ++= that
    b.result
  }

  /** Create a new sequence of type IterableType which contains all elements of this sequence
   *  followed by all elements of Iterator `that'
   */
  def ++[B >: A](that: Iterator[B]): CC[B] = {
    val b = newBuilder[B]
    b ++= thisCC
    b ++= that
    b.result
  }

  /** Returns the sequence resulting from applying the given function
   *  <code>f</code> to each element of this sequence.
   *
   *  @param f function to apply to each element.
   *  @return  <code>f(a<sub>0</sub>), ..., f(a<sub>n</sub>)</code> if this
   *           sequence is <code>a<sub>0</sub>, ..., a<sub>n</sub></code>.
   */
  def map[B](f: A => B): CC[B] = {
    val b = newBuilder[B]
    for (x <- this) b += f(x)
    b.result
  }

  /** Applies the given function <code>f</code> to each element of
   *  this sequence, then concatenates the results.
   *
   *  @param f the function to apply on each element.
   *  @return  <code>f(a<sub>0</sub>) ::: ... ::: f(a<sub>n</sub>)</code> if
   *           this sequence is <code>a<sub>0</sub>, ..., a<sub>n</sub></code>.
   */
  def flatMap[B](f: A => Iterable[B]): CC[B] = {
    val b = newBuilder[B]
    for (x <- this) b ++= f(x)
    b.result
  }

  /** Returns all the elements of this sequence that satisfy the
   *  predicate <code>p</code>. The order of the elements is preserved.
   *  @param p the predicate used to filter the list.
   *  @return the elements of this list satisfying <code>p</code>.
   */
  def filter(p: A => Boolean): CC[A] = {
    val b = newBuilder[A]
    for (x <- this)
      if (p(x)) b += x
    b.result
  }

  /** Removes all elements of the iterable which satisfy the predicate
   *  <code>p</code>. This is like <code>filter</code> with the
   *  predicate inversed.
   *
   *  @param p the predicate to use to test elements
   *  @return  the list without all elements which satisfy <code>p</code>
   */
  def remove(p: A => Boolean): CC[A] = filter(!p(_))

  /** Partitions this iterable in two iterables according to a predicate.
   *
   *  @param p the predicate on which to partition
   *  @return  a pair of iterables: the iterable that satisfies the predicate
   *           <code>p</code> and the iterable that does not.
   *           The relative order of the elements in the resulting iterables
   *           is the same as in the original iterable.
   */
  def partition(p: A => Boolean): (CC[A], CC[A]) = {
    val l, r = newBuilder[A]
    for (x <- this) (if (p(x)) l else r) += x
    (l.result, r.result)
  }

  /** Apply a function <code>f</code> to all elements of this
   *  iterable object.
   *
   *  @note Will not terminate for infinite-sized collections.
   *  @param  f   a function that is applied to every element.
   *  Note this function underlies the implementation of most other bulk operations.
   *  It should be overridden in concrete collectionc classes with efficient implementations.
   */
  def foreach(f: A => Unit): Unit = elements.foreach(f)

  /** Return true iff the given predicate `p` yields true for all elements
   *  of this iterable.
   *
   *  @note May not terminate for infinite-sized collections.
   *  @param   p     the predicate
   */
  def forall(p: A => Boolean): Boolean = {
    var result = true
    breakable {
      for (x <- this)
        if (!p(x)) { result = false; break }
    }
    result
  }

  /** Return true iff there is an element in this iterable for which the
   *  given predicate `p` yields true.
   *
   *  @note May not terminate for infinite-sized collections.
   *  @param   p     the predicate
   */
  def exists(p: A => Boolean): Boolean = {
    var result = false
    breakable {
      for (x <- this)
        if (p(x)) { result = true; break }
    }
    result
  }

  /** Count the number of elements in the iterable which satisfy a predicate.
   *
   *  @param p the predicate for which to count
   *  @return  the number of elements satisfying the predicate <code>p</code>.
   */
  def count(p: A => Boolean): Int = {
    var cnt = 0
    for (x <- this) {
      if (p(x)) cnt += 1
    }
    cnt
  }

  /** Find and return the first element of the iterable object satisfying a
   *  predicate, if any.
   *
   *  @note may not terminate for infinite-sized collections.
   *  @param p the predicate
   *  @return an option containing the first element in the iterable object
   *  satisfying <code>p</code>, or <code>None</code> if none exists.
   */
  def find(p: A => Boolean): Option[A] = {
    var result: Option[A] = None
    breakable {
      for (x <- this)
        if (p(x)) { result = Some(x); break }
    }
    result
  }

  /** Combines the elements of this iterable object together using the binary
   *  function <code>f</code>, from left to right, and starting with
   *  the value <code>z</code>.
   *
   *  @note Will not terminate for infinite-sized collections.
   *  @return <code>f(... (f(f(z, a<sub>0</sub>), a<sub>1</sub>) ...),
   *          a<sub>n</sub>)</code> if the list is
   *          <code>[a<sub>0</sub>, a<sub>1</sub>, ..., a<sub>n</sub>]</code>.
   */
  def foldLeft[B](z: B)(op: (B, A) => B): B = {
    var result = z
    for (x <- this)
      result = op(result, x)
    result
  }

  /** Combines the elements of this list together using the binary
   *  function <code>f</code>, from right to left, and starting with
   *  the value <code>z</code>.
   *
   *  @note Will not terminate for infinite-sized collections.
   *  @return <code>f(a<sub>0</sub>, f(a<sub>1</sub>, f(..., f(a<sub>n</sub>, z)...)))</code>
   *          if the list is <code>[a<sub>0</sub>, a1, ..., a<sub>n</sub>]</code>.
   */
  def foldRight[B](z: B)(op: (A, B) => B): B = elements.foldRight(z)(op)

  /** Similar to <code>foldLeft</code> but can be used as
   *  an operator with the order of list and zero arguments reversed.
   *  That is, <code>z /: xs</code> is the same as <code>xs foldLeft z</code>
   *  @note Will not terminate for infinite-sized collections.
   */
  def /: [B](z: B)(op: (B, A) => B): B = foldLeft(z)(op)

  /** An alias for <code>foldRight</code>.
   *  That is, <code>xs :\ z</code> is the same as <code>xs foldRight z</code>
   *  @note Will not terminate for infinite-sized collections.
   */
  def :\ [B](z: B)(op: (A, B) => B): B = foldRight(z)(op)

  /** Combines the elements of this iterable object together using the binary
   *  operator <code>op</code>, from left to right
   *  @note Will not terminate for infinite-sized collections.
   *  @param op  The operator to apply
   *  @return <code>op(... op(a<sub>0</sub>,a<sub>1</sub>), ..., a<sub>n</sub>)</code>
      if the iterable object has elements
   *          <code>a<sub>0</sub>, a<sub>1</sub>, ..., a<sub>n</sub></code>.
   *  @throws Predef.UnsupportedOperationException if the iterable object is empty.
   */
  def reduceLeft[B >: A](op: (B, A) => B): B = {
    if (isEmpty) throw new UnsupportedOperationException("empty.reduceLeft")
    var result: B = elements.next
    var first = true
    for (x <- this)
      if (first) first = false
      else result = op(result, x)
    result
  }

  /** Combines the elements of this iterable object together using the binary
   *  operator <code>op</code>, from right to left
   *  @note Will not terminate for infinite-sized collections.
   *  @param op  The operator to apply
   *
   *  @return <code>a<sub>0</sub> op (... op (a<sub>n-1</sub> op a<sub>n</sub>)...)</code>
   *          if the iterable object has elements <code>a<sub>0</sub>, a<sub>1</sub>, ...,
   *          a<sub>n</sub></code>.
   *
   *  @throws Predef.UnsupportedOperationException if the iterator is empty.
   */
  def reduceRight[B >: A](op: (A, B) => B): B =
    elements.reduceRight(op)

  /** Returns an iterable formed from this iterable and the specified list
   *  `other` by associating each element of the former with
   *  the element at the same position in the latter.
   *  If one of the two iterables is longer than the other, its remaining elements are ignored.
   */
  def zip[B](that: Iterable[B]): CC[(A, B)] = {
    val these = this.elements
    val those = that.elements
    val b = this.newBuilder[(A, B)]
    while (these.hasNext && those.hasNext)
      b += ((these.next, those.next))
    b.result
  }

  /** Returns a iterable formed from this iterable and the specified iterable
   *  <code>that</code> by associating each element of the former with
   *  the element at the same position in the latter.
   *
   *  @param that     iterable <code>that</code> may have a different length
   *                  as the self iterable.
   *  @param thisElem element <code>thisElem</code> is used to fill up the
   *                  resulting iterable if the self iterable is shorter than
   *                  <code>that</code>
b   *  @param thatElem element <code>thatElem</code> is used to fill up the
   *                  resulting iterable if <code>that</code> is shorter than
   *                  the self iterable
   *  @return         <code>Iterable((a<sub>0</sub>,b<sub>0</sub>), ...,
   *                  (a<sub>n</sub>,b<sub>n</sub>), (elem,b<sub>n+1</sub>),
   *                  ..., {elem,b<sub>m</sub>})</code>
   *                  when <code>[a<sub>0</sub>, ..., a<sub>n</sub>] zip
   *                  [b<sub>0</sub>, ..., b<sub>m</sub>]</code> is
   *                  invoked where <code>m &gt; n</code>.
   */
  def zipAll[B, A1 >: A, B1 >: B](that: Iterable[B], thisElem: A1, thatElem: B1): CC[(A1, B1)] = {
    val these = this.elements
    val those = that.elements
    val b = newBuilder[(A1, B1)]
    while (these.hasNext && those.hasNext)
      b += ((these.next, those.next))
    while (these.hasNext)
      b += ((these.next, thatElem))
    while (those.hasNext)
      b += ((thisElem, those.next))
    b.result
  }

  /** Zips this iterable with its indices. `s.zipWithIndex` is equivalent to
   *  `s zip s.indices`, but is usually more efficient.
   */
  def zipWithIndex: CC[(A, Int)] = {
    val b = newBuilder[(A, Int)]
    var i = 0
    for (x <- this) {
      b += ((x, i))
      i +=1
    }
    b.result
  }

  /** Copy all elements to a given buffer
   *  @note Will not terminate for infinite-sized collections.
   *  @param  dest   The buffer to which elements are copied
   */
  def copyToBuffer[B >: A](dest: Buffer[B]) {
    for (x <- this) dest += x
  }

  /** Fills the given array <code>xs</code> with at most `len` elements of
   *  this sequence starting at position `start`.
   *  Copying will stop oce either the end of the current iterable is reached or
   *  `len` elements have been copied.
   *
   *  @note Will not terminate for infinite-sized collections.
   *  @param  xs the array to fill.
   *  @param  start starting index.
   *  @param  len number of elements to copy
   *  @pre    the array must be large enough to hold all elements.
   */
  def copyToArray[B >: A](xs: Array[B], start: Int, len: Int) {
    var i = start
    val end = (start + len) min xs.length
    for (x <- this) {
      if (i < end) {
        xs(i) = x
        i += 1
      }
    }
  }

  /** Fills the given array <code>xs</code> with the elements of
   *  this sequence starting at position <code>start</code>
   *  until either the end of the current iterable or the end of array `xs` is reached.
   *
   *  @note Will not terminate for infinite-sized collections.
   *  @param  xs the array to fill.
   *  @param  start starting index.
   *  @pre    the array must be large enough to hold all elements.
   */
  def copyToArray[B >: A](xs: Array[B], start: Int) {
    copyToArray(xs, start, xs.length - start)
  }

  /** Converts this collection to a fresh Array elements.
   *  @note  Will not terminate for infinite-sized collections.
   */
  def toArray[B >: A]: Array[B] = {
    var size = 0
    for (x <- this) size += 1
    val result = new Array[B](size)
    copyToArray(result, 0)
    result
  }

  /**
   *  Create a fresh list with all the elements of this iterable object.
   *  @note Will not terminate for infinite-sized collections.
   */
  def toList: List[A] = (new ListBuffer[A] ++ thisCC).toList

  /**
   *  Returns a sequence containing all of the elements in this iterable object.
   *  @note Will not terminate for infinite-sized collections.
   */
  def toSequence: Sequence[A] = toList

  /** @deprecated use toSequence instead
   */
  @deprecated def toSeq: Sequence[A] = toSequence

  /**
   *  Create a stream which contains all the elements of this iterable object.
   *  @note consider using <code>projection</code> for lazy behavior.
   */
  def toStream: Stream[A] = elements.toStream

  /** Sort the iterable according to the comparison function
   *  <code>&lt;(e1: a, e2: a) =&gt; Boolean</code>,
   *  which should be true iff <code>e1</code> is smaller than
   *  <code>e2</code>.
   *  The sort is stable. That is elements that are equal wrt `lt` appear in the
   *  same order in the sorted iterable as in the original.
   *
   *  @param lt the comparison function
   *  @return   a list sorted according to the comparison function
   *            <code>&lt;(e1: a, e2: a) =&gt; Boolean</code>.
   *  @ex <pre>
   *    List("Steve", "Tom", "John", "Bob")
   *      .sort((e1, e2) => (e1 compareTo e2) &lt; 0) =
   *    List("Bob", "John", "Steve", "Tom")</pre>
   *  !!!
  def sortWith(lt : (A,A) => Boolean): CC[A] = {
    val arr = toArray
    Array.sortWith(arr, lt)
    val b = newBuilder[A]
    for (x <- arr) b += x
    b.result
  }
  */

  /** Returns a string representation of this iterable object. The resulting string
   *  begins with the string <code>start</code> and is finished by the string
   *  <code>end</code>. Inside, the string representations of elements (w.r.t.
   *  the method <code>toString()</code>) are separated by the string
   *  <code>sep</code>.
   *
   *  @ex  <code>List(1, 2, 3).mkString("(", "; ", ")") = "(1; 2; 3)"</code>
   *  @note Will not terminate for infinite-sized collections.
   *  @param start starting string.
   *  @param sep separator string.
   *  @param end ending string.
   *  @return a string representation of this iterable object.
   */
  def mkString(start: String, sep: String, end: String): String =
    addString(new StringBuilder(), start, sep, end).toString

  /** Returns a string representation of this iterable object. The string
   *  representations of elements (w.r.t. the method <code>toString()</code>)
   *  are separated by the string <code>sep</code>.
   *
   *  @note Will not terminate for infinite-sized collections.
   *  @param sep separator string.
   *  @return a string representation of this iterable object.
   */
  def mkString(sep: String): String =
    addString(new StringBuilder(), sep).toString

  /** Converts a collection into a flat <code>String</code> by each element's toString method.
   *  @note Will not terminate for infinite-sized collections.
   */
  def mkString =
    addString(new StringBuilder()).toString

  /** Write all elements of this iterable into given string builder.
   *  The written text begins with the string <code>start</code> and is finished by the string
   *  <code>end</code>. Inside, the string representations of elements (w.r.t.
   *  the method <code>toString()</code>) are separated by the string
   *  <code>sep</code>.
   *  @note Will not terminate for infinite-sized collections.
   */
  def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = {
    b append start
    var first = true
    for (x <- this) {
      if (first) first = false
      else b append sep
      b append x
    }
    b append end
  }

  /** Write all elements of this string into given string builder.
   *  The string representations of elements (w.r.t. the method <code>toString()</code>)
   *  are separated by the string <code>sep</code>.
   *  @note Will not terminate for infinite-sized collections.
   */
  def addString(b: StringBuilder, sep: String): StringBuilder = {
    var first = true
    for (x <- this) {
      if (first) first = false
      else b append sep
      b append x
    }
    b
  }

  /** Write all elements of this string into given string builder without using
   *  any separator between consecutive elements.
   *  @note Will not terminate for infinite-sized collections.
   */
  def addString(b: StringBuilder): StringBuilder = {
    for (x <- this) {
      b append x
    }
    b
  }

  /**
   * returns a projection that can be used to call non-strict <code>filter</code>,
   * <code>map</code>, and <code>flatMap</code> methods that build projections
   * of the collection.
  def projection : Iterable.Projection[A] = new Iterable.Projection[A] {
    def elements = Iterable.this.elements
    override def force = Iterable.this
  }
   */

  override def toString = mkString(stringPrefix + "(", ", ", ")")

  /** Defines the prefix of this object's <code>toString</code> representation.
   */
  def stringPrefix : String = {
    var string = this.getClass.getName
    val idx1 = string.lastIndexOf('.' : Int)
    if (idx1 != -1) string = string.substring(idx1 + 1)
    val idx2 = string.indexOf('$')
    if (idx2 != -1) string = string.substring(0, idx2)
    string
  }


  /** Creates a view of this iterable @see IterableView
   */
  def view: IterableView[CC, A] = new IterableView[CC, A] { // !!! Martin: We should maybe infer the type parameters here?
    val origin = thisCC
    val elements: Iterator[A] = self.elements
  }

// The following methods return non-deterministic results, unless this iterable is an OrderedIterable

  /** The first element of this sequence.
   *
   *  @note  Might return different results for different runs, unless this iterable is ordered
   *  @throws Predef.NoSuchAentException if the sequence is empty.
   */
  def head: A = if (isEmpty) throw new NoSuchElementException else elements.next

   /** @deprecated  use head instead */
  @deprecated def first: A = head

  /** Returns as an option the first element of this iterable
   *  or <code>None</code> if iterable is empty.
   *  @note  Might return different results for different runs, unless this iterable is ordered
   */
  def headOption: Option[A] = if (isEmpty) None else Some(head)

  /** @deprecated use headOption instead
   *  <code>None</code> if list is empty.
   */
  @deprecated def firstOption: Option[A] = headOption

  /** An iterable consisting of all elements of this iterable
   *  except the first one.
   *  @note  Might return different results for different runs, unless this iterable is ordered
   */
  def tail: CC[A] = drop(1)

  /** Return an iterable consisting only of the first <code>n</code>
   *  elements of this iterable, or else the whole iterable, if it has less
   *  than <code>n</code> elements.
   *
   *  @param n the number of elements to take
   *  @return a possibly projected sequence
   *  @note  Might return different results for different runs, unless this iterable is ordered
   */
  def take(n: Int): CC[A] = {
    val b = newBuilder[A]
    var i = 0
    breakable {
      for (x <- this) {
        b += x
        i += 1
        if (i == n) break
      }
    }
    b.result
  }

  /** Returns this iterable without its <code>n</code> first elements
   *  If this iterable has less than <code>n</code> elements, the empty
   *  iterable is returned.
   *
   *  @param n the number of elements to drop
   *  @return  the new iterable
   *  @note  Might return different results for different runs, unless this iterable is ordered
   */
  def drop(n: Int): CC[A] = {
    val b = newBuilder[A]
    var i = 0
    for (x <- this) {
      if (i >= n) b += x
      i += 1
    }
    b.result
  }

  /** A sub-iterable starting at index `from`
   *  and extending up to (but not including) index `until`.
   *
   *  @note c.slice(from, to)  is equivalent to (but possibly more efficient than)
   *  c.drop(from).take(to - from)
   *
   *  @param from   The index of the first element of the returned subsequence
   *  @param until  The index of the element following the returned subsequence
   *  @throws IndexOutOfBoundsException if <code>from &lt; 0</code>
   *          or <code>length &lt; from + len<code>
   *  @note  Might return different results for different runs, unless this iterable is ordered
   */
  def slice(from: Int, until: Int): CC[A] = {
    val b = newBuilder[A]
    var i = 0
    breakable {
      for (x <- this) {
        if (i >= from) b += x
        i += 1
        if (i == until) break
      }
    }
    b.result
  }

  /** The last element of this iterable.
   *
   *  @throws Predef.NoSuchElementException if the sequence is empty.
   *  @note  Might return different results for different runs, unless this iterable is ordered
   */
  def last: A = {
    var lst = head
    for (x <- this)
      lst = x
    lst
  }

  /** Returns as an option the last element of this iterable or
   *  <code>None</code> if iterable is empty.
   *
   *  @return the last element as an option.
   *  @note  Might return different results for different runs, unless this iterable is ordered
   */
  def lastOption: Option[A] = if (isEmpty) None else Some(last)

  /** An iterable consisting of all elements of this iterable except the last one.
   *  @note  Might return different results for different runs, unless this iterable is ordered
   */
  def init: CC[A] = {
    var lst = head
    val b = newBuilder[A]
    for (x <- this) {
      b += lst
      lst = x
    }
    b.result
  }

  /** Returns the rightmost <code>n</code> elements from this iterable.
   *
   *  @param n the number of elements to take
   *  @note  Might return different results for different runs, unless this iterable is ordered
   */
  def takeRight(n: Int): CC[A] = {
    val b = newBuilder[A]
    val lead = elements drop n
    var go = false
    for (x <- this) {
      if (go) b += x
      else if (lead.hasNext) lead.next
      else go = true
    }
    b.result
  }

  /** Returns the iterable wihtout its rightmost <code>n</code> elements.
   *
   *  @param n the number of elements to take
   *  @note  Might return different results for different runs, unless this iterable is ordered
   */
  def dropRight(n: Int): CC[A] = {
    val b = newBuilder[A]
    val lead = elements drop n
    breakable {
      for (x <- this) {
        if (!lead.hasNext) break
        lead.next
        b += x
      }
    }
    b.result
  }

  /** Split the iterable at a given point and return the two parts thus
   *  created.
   *
   *  @param n the position at which to split
   *  @return  a pair of iterables composed of the first <code>n</code>
   *           elements, and the other elements.
   *  @note  Might return different results for different runs, unless this iterable is ordered
   */
  def splitAt(n: Int): (CC[A], CC[A]) = {
    val l, r = newBuilder[A]
    var i = 0
    for (x <- this)
      (if (i < n) l else r) += x
    (l.result, r.result)
  }

  /** Returns the longest prefix of this sequence whose elements satisfy
   *  the predicate <code>p</code>.
   *
   *  @param p the test predicate.
   *  @return  the longest prefix of this sequence whose elements satisfy
   *           the predicate <code>p</code>.
   *  @note  Might return different results for different runs, unless this iterable is ordered
   */
  def takeWhile(p: A => Boolean): CC[A] = {
    val b = newBuilder[A]
    breakable {
      for (x <- this) {
        if (!p(x)) break
        b += x
      }
    }
    b.result
  }

  /** Returns the longest suffix of this sequence whose first element
   *  does not satisfy the predicate <code>p</code>.
   *
   *  @param p the test predicate.
   *  @return  the longest suffix of the sequence whose first element
   *           does not satisfy the predicate <code>p</code>.
   *  @note  Might return different results for different runs, unless this iterable is ordered
   */
  def dropWhile(p: A => Boolean): CC[A] = {
    val b = newBuilder[A]
    var go = false
    for (x <- this) {
      if (go) b += x
      else if (!p(x)) { go = true; b += x }
    }
    b.result
  }

 /** Returns a pair consisting of the longest prefix of the list whose
   *  elements all satisfy the given predicate, and the rest of the list.
   *
   *  @param p the test predicate
   *  @return  a pair consisting of the longest prefix of the list whose
   *           elements all satisfy <code>p</code>, and the rest of the list.
   *  @note  Might return different results for different runs, unless this iterable is ordered
   */
  def span(p: A => Boolean): (CC[A], CC[A]) = {
    val l, r = newBuilder[A]
    var toLeft = true
    for (x <- this) {
      toLeft = toLeft && p(x)
      (if (toLeft) l else r) += x
    }
    (l.result, r.result)
  }

  /** Checks if the other iterable object contains the same elements as this one.
   *
   *  @note will not terminate for infinite-sized iterables.
   *  @param that  the other iterable
   *  @return true, iff both iterables contain the same elements.
   *  @note  Might return different results for different runs, unless this iterable is ordered
   */
  def sameElements[B >: A](that: OrderedIterable[B]): Boolean = {
    val these = this.elements
    val those = that.elements
    while (these.hasNext && those.hasNext && these.next() == those.next()) {}
    !these.hasNext && !those.hasNext
  }

  /** A sub-sequence view  starting at index `from`
   *  and extending up to (but not including) index `until`.
   *
   *  @param from   The index of the first element of the slice
   *  @param until  The index of the element following the slice
   *  @note  The difference between `view` and `slice` is that `view` produces
   *         a view of the current sequence, whereas `slice` produces a new sequence.
   *
   *  @note  Might return different results for different runs, unless this iterable is ordered
   *  @note view(from, to)  is equivalent to view.slice(from, to)
   */
  def view(from: Int, until: Int): IterableView[CC, A] = view.slice(from, until)
}
