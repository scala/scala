/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Traversable.scala 15188 2008-05-24 15:01:02Z stepancheg $
package scala.collection.generic

// import immutable.{List, Stream, Nil} //!!!
import mutable.{Buffer, ArrayBuffer, ListBuffer}

/** A template trait for traversable collections.
 *  This is a base trait of all kinds of Scala collections. It implements the
 *  behavior common to all collections, in terms of a method `foreach` with signature:
 *
 *   def foreach[U](f: Elem => U): Unit
 *
 *  Collection classes mixing in this trait provide a concrete
 *  <code>foreach</code> method which traverses all the
 *  elements contained in the collection, applying a given function to each.
 *  They also need to provide a method `newBuilder`
 *  which creates a builder for collections of the same kind.
 *
 *  A traversable class might or might not have two properties: strictness and orderedness.
 *  Neither is represented as a type.
 *
 *  The instances of a strict collection class have all their elements computed before
 *  they can be used as values. By contrast, instances of a non-strict collection class
 *  may defer computation of some of their elements until after the instance is available as a value.
 *  A typical example of a non-strict collection class is a scala.immutable.Stream.
 *  A more general class of examples are TraversableViews.
 *
 *  If a collection is an instance of an ordered collection class, traversing its elements
 *  with `foreach` will always visit elements in the same order, even for different
 *  runs of the program. If the class is not ordered, `foreach` can visit elements
 *  in different orders for different runs (but it will keep the same order in the same run).
 *  A typical example of a collection class which is not ordered is a `HashMap` of objects.
 *  The traversal order for hash maps will depend on the hash codes of its elements, and
 *  these hash codes might differ from one run to the next. By contrast, a `LinkedHashMap`
 *  is odered because it's `foreach` method visits elements in the order they were inserted
 *  into the `HashMap`.
 *
 *  @author Martin Odersky
 *  @version 2.8
 */
trait TraversableTemplate[+A, +This <: TraversableTemplate[A, This] with Traversable[A]] {
self =>

  import Traversable.breaks._

  /** The proper way to do this would be to make `self` of type `This`. But unfortunately this
   *  makes `this` to be of type `Traversable[A]`. Since `Traversable` is a subtype of
   *  `TraversableTemplate`, all methods of `this` are taken from `Traversable`. In particular
   *  the `newBuilder` method is taken from `Traversable`, which means it yields a `Traversable[A]`
   *  instyead of a `This`.
   *
   *  The right way out of this is to change Scala's member selection rules, so that
   *  always the most specific type will be selected, no matter whether a member is abstract
   *  or concrete. I tried to fake this by having a method `thisTemplate` which
   *  returns this at the Template type. But unfortunately that does not work, because
   *  we need to call `newBuilder` on this at the Template type (so that we get back a `This`)
   *  and `newBuilder` has to be a proctected[this] because of variance.
   *  The less appealing alternative is implemented now: Forget the self type and
   *  introduce a `thisCollection` which is this seen as an instance of `This`.
   *  We should go back to this once we have ameliorated Scala's member selection rules.
   */
  protected def thisCollection: This = this.asInstanceOf[This]

  /** Create a new builder for this collection type.
   */
  protected[this] def newBuilder: Builder[A, This]

  /** Apply a function <code>f</code> to all elements of this
   *  traversable object.
   *
   *  @param  f   A function that is applied for its side-effect to every element.
   *              The result (of arbitrary type U) of function `f` is discarded.
   *
   *  @note This method underlies the implementation of most other bulk operations.
   *        It's important to implement this method in an efficient way.
   */
  def foreach[U](f: A => U): Unit

  /** Does this collection contain no elements?
   */
  def isEmpty: Boolean = {
    var result = true
    breakable {
      for (x <- this) {
        result = false
        break
      }
    }
    result
  }

  /** Does this collection contain some elements?
   */
  def nonEmpty: Boolean = !isEmpty

  /** The number of elements in this collection
   */
  def size: Int = {
    var result = 0
    for (x <- this) result += 1
    result
  }

  /** Returns true if this collection is known to have finite size.
   *  This is the case if the collection type is strict, or if the
   *  collection type is non-strict (e.g. it's a Stream), but all
   *  collection elements have been computed.
   *  Many methods in this trait will not work on collections of
   *  infinite sizes.
   */
  def hasDefiniteSize = true

  /** Creates a new traversable of type `That` which contains all elements of this traversable
   *  followed by all elements of another traversable.
   *
   *  @param that   The traversable to append
   */
  def ++[B >: A, That](that: Traversable[B])(implicit bf: BuilderFactory[B, That, This]): That = {
    val b = bf(thisCollection)
    b ++= thisCollection
    b ++= that
    b.result
  }

  /** Creates a new traversable of type `That` which contains all elements of this traversable
   *  followed by all elements of an iterator.
   *
   *  @param that  The iterator to append
   */
  def ++[B >: A, That](that: Iterator[B])(implicit bf: BuilderFactory[B, That, This]): That = {
    val b = bf(thisCollection)
    b ++= thisCollection
    b ++= that
    b.result
  }

  /** Returns the traversable that results from applying the given function
   *  <code>f</code> to each element of this traversable and collecting the results
   *  in an traversable of type `That`.
   *
   *  @param f function to apply to each element.
   */
  def map[B, That](f: A => B)(implicit bf: BuilderFactory[B, That, This]): That = {
    val b = bf(thisCollection)
    for (x <- this) b += f(x)
    b.result
  }

  /** Applies the given function <code>f</code> to each element of
   *  this traversable, then concatenates the results in an traversable of type That.
   *
   *  @param f the function to apply on each element.
   */
  def flatMap[B, That](f: A => Traversable[B])(implicit bf: BuilderFactory[B, That, This]): That = {
    val b = bf(thisCollection)
    for (x <- this) b ++= f(x)
    b.result
  }

  /** Returns all the elements of this traversable that satisfy the
   *  predicate <code>p</code>. The order of the elements is preserved.
   *  @param p the predicate used to filter the traversable.
   *  @return the elements of this traversable satisfying <code>p</code>.
   */
  def filter(p: A => Boolean): This = {
    val b = newBuilder
    for (x <- this)
      if (p(x)) b += x
    b.result
  }

  /** Returns a traversable with all elements of this traversable which do not satisfy the predicate
   *  <code>p</code>.
   *
   *  @param p the predicate used to test elements
   *  @return  the traversable without all elements that satisfy <code>p</code>
   */
  def filterNot(p: A => Boolean): This = filter(!p(_))

  /** Returns a traversable with all elements of this traversable which do not satisfy the predicate
   *  <code>p</code>.
   *
   *  @param p the predicate used to test elements
   *  @return  the traversable without all elements that satisfy <code>p</code>
   *  @deprecated use `filterNot` instead
   */
  @deprecated def remove(p: A => Boolean): This = filterNot(p)

  /** Partitions this traversable in two traversables according to a predicate.
   *
   *  @param p the predicate on which to partition
   *  @return  a pair of traversables: the traversable that satisfies the predicate
   *           <code>p</code> and the traversable that does not.
   *           The relative order of the elements in the resulting traversables
   *           is the same as in the original traversable.
   */
  def partition(p: A => Boolean): (This, This) = {
    val l, r = newBuilder
    for (x <- this) (if (p(x)) l else r) += x
    (l.result, r.result)
  }

  /** Partition this traversable into a map of traversables
   *  according to some discriminator function.
   *  @invariant   (xs partition f)(k) = xs filter (x => f(x) == k)
   *
   *  @note This method is not re-implemented by views. This means
   *        when applied to a view it will always force the view and
   *        return a new collection.
   */
  def groupBy[K](f: A => K): Map[K, This] = {
    var m = Map[K, Builder[A, This]]()
    for (elem <- this) {
      val key = f(elem)
      val bldr = m get key match {
        case None => val b = newBuilder; m = m updated (key, b); b
        case Some(b) => b
      }
      bldr += elem
    }
    m mapValues (_.result)
  }

  /** Return true iff the given predicate `p` yields true for all elements
   *  of this traversable.
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

  /** Return true iff there is an element in this traversable for which the
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

  /** Count the number of elements in the traversable which satisfy a predicate.
   *
   *  @note Will not terminate for infinite-sized collections.
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

  /** Find and return the first element of the traversable object satisfying a
   *  predicate, if any.
   *
   *  @note may not terminate for infinite-sized collections.
   *  @note Might return different results for different runs, unless this traversable is ordered.
   *  @param p the predicate
   *  @return an option containing the first element in the traversable object
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

  /** Combines the elements of this traversable object together using the binary
   *  function <code>f</code>, from left to right, and starting with
   *  the value <code>z</code>.
   *
   *  @note Will not terminate for infinite-sized collections.
   *  @note Might return different results for different runs, unless this traversable is ordered, or
   *        the operator is associative and commutative.
   *  @return <code>f(... (f(f(z, a<sub>0</sub>), a<sub>1</sub>) ...),
   *          a<sub>n</sub>)</code> if the traversable is
   *          <code>[a<sub>0</sub>, a<sub>1</sub>, ..., a<sub>n</sub>]</code>.
   */
  def foldLeft[B](z: B)(op: (B, A) => B): B = {
    var result = z
    for (x <- this)
      result = op(result, x)
    result
  }

  /** Similar to <code>foldLeft</code> but can be used as
   *  an operator with the order of traversable and zero arguments reversed.
   *  That is, <code>z /: xs</code> is the same as <code>xs foldLeft z</code>
   *  @note Will not terminate for infinite-sized collections.
   *  @note Might return different results for different runs, unless this traversable is ordered, or
   *        the operator is associative and commutative.
   */
  def /: [B](z: B)(op: (B, A) => B): B = foldLeft(z)(op)

  /** Combines the elements of this iterable together using the binary
   *  function <code>f</code>, from right to left, and starting with
   *  the value <code>z</code>.
   *
   *  @note Will not terminate for infinite-sized collections.
   *  @note Might return different results for different runs, unless this iterable is ordered, or
   *        the operator is associative and commutative.
   *  @return <code>f(a<sub>0</sub>, f(a<sub>1</sub>, f(..., f(a<sub>n</sub>, z)...)))</code>
   *          if the iterable is <code>[a<sub>0</sub>, a1, ..., a<sub>n</sub>]</code>.
   */
  def foldRight[B](z: B)(op: (A, B) => B): B = {
    var elems: List[A] = Nil
    for (x <- this) elems = x :: elems
    elems.foldLeft(z)((x, y) => op(y, x))
  }

  /** An alias for <code>foldRight</code>.
   *  That is, <code>xs :\ z</code> is the same as <code>xs foldRight z</code>
   *  @note Will not terminate for infinite-sized collections.
   *  @note Might return different results for different runs, unless this iterable is ordered, or
   *        the operator is associative and commutative.
   */
  def :\ [B](z: B)(op: (A, B) => B): B = foldRight(z)(op)

  /** Combines the elements of this traversable object together using the binary
   *  operator <code>op</code>, from left to right
   *  @note Will not terminate for infinite-sized collections.
   *  @note Might return different results for different runs, unless this traversable is ordered, or
   *        the operator is associative and commutative.
   *  @param op  The operator to apply
   *  @return <code>op(... op(a<sub>0</sub>,a<sub>1</sub>), ..., a<sub>n</sub>)</code>
      if the traversable object has elements
   *          <code>a<sub>0</sub>, a<sub>1</sub>, ..., a<sub>n</sub></code>.
   *  @throws Predef.UnsupportedOperationException if the traversable object is empty.
   */
  def reduceLeft[B >: A](op: (B, A) => B): B = {
    if (isEmpty) throw new UnsupportedOperationException("empty.reduceLeft")
    var result: B = head
    var first = true
    for (x <- this)
      if (first) first = false
      else result = op(result, x)
    result
  }

  /** Combines the elements of this traversable object together using the binary
   *  operator <code>op</code>, from left to right
   *  @note Will not terminate for infinite-sized collections.
   *  @note Might return different results for different runs, unless this traversable is ordered, or
   *        the operator is associative and commutative.
   *  @param op  The operator to apply
   *  @return  If the traversable is non-empty, the result of the operations as an Option, otherwise None.
   */
  def reduceLeftOption[B >: A](op: (B, A) => B): Option[B] = {
    if (isEmpty) None else Some(reduceLeft(op))
  }

  /** Combines the elements of this iterable object together using the binary
   *  operator <code>op</code>, from right to left
   *  @note Will not terminate for infinite-sized collections.
   *  @note Might return different results for different runs, unless this iterable is ordered, or
   *        the operator is associative and commutative.
   *  @param op  The operator to apply
   *
   *  @return <code>a<sub>0</sub> op (... op (a<sub>n-1</sub> op a<sub>n</sub>)...)</code>
   *          if the iterable object has elements <code>a<sub>0</sub>, a<sub>1</sub>, ...,
   *          a<sub>n</sub></code>.
   *
   *  @throws Predef.UnsupportedOperationException if the iterator is empty.
   */
  def reduceRight[B >: A](op: (A, B) => B): B = {
    if (isEmpty) throw new UnsupportedOperationException("empty.reduceRight")
    var elems: List[A] = Nil
    for (x <- this) elems = x :: elems
    elems.reduceLeft[B]((x, y) => op(y, x))
  }

 /** Combines the elements of this iterable object together using the binary
   *  operator <code>op</code>, from right to left.
   *  @note Will not terminate for infinite-sized collections.
   *  @note Might return different results for different runs, unless this iterable is ordered, or
   *        the operator is associative and commutative.
   *  @param op  The operator to apply
   *  @return  If the iterable is non-empty, the result of the operations as an Option, otherwise None.
   */
  def reduceRightOption[B >: A](op: (A, B) => B): Option[B] = {
    if (isEmpty) None else Some(reduceRight(op))
  }

  /** The first element of this traversable.
   *
   *  @note  Might return different results for different runs, unless this traversable is ordered
   *  @throws Predef.NoSuchElementException if the traversable is empty.
   */
  def head: A = {
    var result: () => A = () => throw new NoSuchElementException
    breakable {
      for (x <- this) {
        result = () => x
        break
      }
    }
    result()
  }

  /** Returns as an option the first element of this traversable
   *  or <code>None</code> if traversable is empty.
   *  @note  Might return different results for different runs, unless this traversable is ordered
   */
  def headOption: Option[A] = if (isEmpty) None else Some(head)

  /** An traversable consisting of all elements of this traversable
   *  except the first one.
   *  @note  Might return different results for different runs, unless this traversable is ordered
   */
  def tail: This = drop(1)

  /** The last element of this traversable.
   *
   *  @throws Predef.NoSuchElementException if the traversable is empty.
   *  @note  Might return different results for different runs, unless this traversable is ordered
   */
  def last: A = {
    var lst = head
    for (x <- this)
      lst = x
    lst
  }

  /** Returns as an option the last element of this traversable or
   *  <code>None</code> if traversable is empty.
   *
   *  @return the last element as an option.
   *  @note  Might return different results for different runs, unless this traversable is ordered
   */
  def lastOption: Option[A] = if (isEmpty) None else Some(last)

  /** An traversable consisting of all elements of this traversable except the last one.
   *  @throws Predef.UnsupportedOperationException if the stream is empty.
   *  @note  Might return different results for different runs, unless this traversable is ordered
   */
  def init: This = {
    if (isEmpty) throw new UnsupportedOperationException("empty.init")
    var lst = head
    var follow = false
    val b = newBuilder
    for (x <- this) {
      if (follow) b += lst
      else follow = true
      lst = x
    }
    b.result
  }

  /** Return an traversable consisting only of the first <code>n</code>
   *  elements of this traversable, or else the whole traversable, if it has less
   *  than <code>n</code> elements.
   *
   *  @param n the number of elements to take
   *  @note  Might return different results for different runs, unless this traversable is ordered
   */
  def take(n: Int): This = {
    val b = newBuilder
    var i = 0
    breakable {
      for (x <- this) {
        if (i >= n) break
        b += x
        i += 1
      }
    }
    b.result
  }

  /** Returns this traversable without its <code>n</code> first elements
   *  If this traversable has less than <code>n</code> elements, the empty
   *  traversable is returned.
   *
   *  @param n the number of elements to drop
   *  @return  the new traversable
   *  @note  Might return different results for different runs, unless this traversable is ordered
   */
  def drop(n: Int): This = {
    val b = newBuilder
    var i = 0
    for (x <- this) {
      if (i >= n) b += x
      i += 1
    }
    b.result
  }

  /** A sub-traversable starting at index `from`
   *  and extending up to (but not including) index `until`.
   *
   *  @note c.slice(from, to)  is equivalent to (but possibly more efficient than)
   *  c.drop(from).take(to - from)
   *
   *  @param from   The index of the first element of the returned subsequence
   *  @param until  The index of the element following the returned subsequence
   *  @note  Might return different results for different runs, unless this traversable is ordered
   */
  def slice(from: Int, until: Int): This = {
    val b = newBuilder
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

  /** Returns the longest prefix of this traversable whose elements satisfy
   *  the predicate <code>p</code>.
   *
   *  @param p the test predicate.
   *  @note  Might return different results for different runs, unless this traversable is ordered
   */
  def takeWhile(p: A => Boolean): This = {
    val b = newBuilder
    breakable {
      for (x <- this) {
        if (!p(x)) break
        b += x
      }
    }
    b.result
  }

  /** Returns the longest suffix of this traversable whose first element
   *  does not satisfy the predicate <code>p</code>.
   *
   *  @param p the test predicate.
   *  @note  Might return different results for different runs, unless this traversable is ordered
   */
  def dropWhile(p: A => Boolean): This = {
    val b = newBuilder
    var go = false
    for (x <- this) {
      if (!p(x)) go = true
      if (go) b += x
    }
    b.result
  }

 /** Returns a pair consisting of the longest prefix of the traversable whose
   *  elements all satisfy the given predicate, and the rest of the traversable.
   *
   *  @param p the test predicate
   *  @return  a pair consisting of the longest prefix of the traversable whose
   *           elements all satisfy <code>p</code>, and the rest of the traversable.
   *  @note  Might return different results for different runs, unless this traversable is ordered
   */
  def span(p: A => Boolean): (This, This) = {
    val l, r = newBuilder
    var toLeft = true
    for (x <- this) {
      toLeft = toLeft && p(x)
      (if (toLeft) l else r) += x
    }
    (l.result, r.result)
  }

  /** Split the traversable at a given point and return the two parts thus
   *  created.
   *
   *  @param n the position at which to split
   *  @return  a pair of traversables composed of the first <code>n</code>
   *           elements, and the other elements.
   *  @note  Might return different results for different runs, unless this traversable is ordered
   */
  def splitAt(n: Int): (This, This) = {
    val l, r = newBuilder
    var i = 0
    for (x <- this) {
      (if (i < n) l else r) += x
      i += 1
    }
    (l.result, r.result)
  }

  /** Copy all elements of this traversable to a given buffer
   *  @note Will not terminate for infinite-sized collections.
   *  @param  dest   The buffer to which elements are copied
   */
  def copyToBuffer[B >: A](dest: Buffer[B]) {
    for (x <- this) dest += x
  }

  /** Fills the given array <code>xs</code> with at most `len` elements of
   *  this traversable starting at position `start`.
   *  Copying will stop once either the end of the current traversable is reached or
   *  `len` elements have been copied or the end of the array is reached.
   *
   *  @note Will not terminate for infinite-sized collections.
   *  @param  xs the array to fill.
   *  @param  start starting index.
   *  @param  len number of elements to copy
   */
  def copyToArray[B >: A](xs: Array[B], start: Int, len: Int) {
    var i = start
    val end = (start + len) min xs.length
    breakable {
      for (x <- this) {
        if (i >= end) break
        xs(i) = x
        i += 1
      }
    }
  }

  /** Fills the given array <code>xs</code> with the elements of
   *  this traversable starting at position <code>start</code>
   *  until either the end of the current traversable or the end of array `xs` is reached.
   *
   *  @note Will not terminate for infinite-sized collections.
   *  @param  xs the array to fill.
   *  @param  start starting index.
   *  @pre    the array must be large enough to hold all elements.
   */
  def copyToArray[B >: A](xs: Array[B], start: Int) {
    copyToArray(xs, start, xs.length - start)
  }

  /** Converts this traversable to a fresh Array containing all elements.
   *  @note  Will not terminate for infinite-sized collections.
   */
  def toArray[B >: A]: Array[B] = {
    val result = new Array[B](size)
    copyToArray(result, 0)
    result
  }

  /** Returns a list with all elements of this traversable object.
   *  @note Will not terminate for infinite-sized collections.
   */
  def toList: List[A] = (new ListBuffer[A] ++= thisCollection).toList

  /** Returns an iterable with all elements in this traversable object.
   *  @note Will not terminate for infinite-sized collections.
   */
  def toIterable: Iterable[A] = toStream

  /** Returns a sequence with all elements in this traversable object.
   *  @note Will not terminate for infinite-sized collections.
   */
  def toSequence: Sequence[A] = toList

  /** Returns a stream with all elements in this traversable object.
   */
  def toStream: Stream[A] = toList.toStream

  /** Sort the traversable according to the comparison function
   *  <code>&lt;(e1: a, e2: a) =&gt; Boolean</code>,
   *  which should be true iff <code>e1</code> is smaller than
   *  <code>e2</code>.
   *  The sort is stable. That is elements that are equal wrt `lt` appear in the
   *  same order in the sorted traversable as in the original.
   *
   *  @param lt the comparison function
   *  @return   a traversable sorted according to the comparison function
   *            <code>&lt;(e1: a, e2: a) =&gt; Boolean</code>.
   *  @ex <pre>
   *    List("Steve", "Tom", "John", "Bob")
   *      .sort((e1, e2) => (e1 compareTo e2) &lt; 0) =
   *    List("Bob", "John", "Steve", "Tom")</pre>
   *  !!!
  def sortWith(lt : (A,A) => Boolean): This = {
    val arr = toArray
    Array.sortWith(arr, lt)
    val b = newBuilder[A]
    for (x <- arr) b += x
    b.result
  }
  */

  /** Returns a string representation of this traversable object. The resulting string
   *  begins with the string <code>start</code> and is finished by the string
   *  <code>end</code>. Inside, the string representations of elements (w.r.t.
   *  the method <code>toString()</code>) are separated by the string
   *  <code>sep</code>.
   *
   *  @ex  <code>List(1, 2, 3).mkString("(", "; ", ")") = "(1; 2; 3)"</code>
   *  @param start starting string.
   *  @param sep separator string.
   *  @param end ending string.
   *  @return a string representation of this traversable object.
   */
  def mkString(start: String, sep: String, end: String): String =
    addString(new StringBuilder(), start, sep, end).toString

  /** Returns a string representation of this traversable object. The string
   *  representations of elements (w.r.t. the method <code>toString()</code>)
   *  are separated by the string <code>sep</code>.
   *
   *  @param sep separator string.
   *  @return a string representation of this traversable object.
   */
  def mkString(sep: String): String =
    addString(new StringBuilder(), sep).toString

  /** Returns a string representation of this traversable object. The string
   *  representations of elements (w.r.t. the method <code>toString()</code>)
   *  follow each other without any separator string.
   */
  def mkString: String =
    addString(new StringBuilder()).toString

  /** Write all elements of this traversable into given string builder.
   *  The written text begins with the string <code>start</code> and is finished by the string
   *  <code>end</code>. Inside, the string representations of elements (w.r.t.
   *  the method <code>toString()</code>) are separated by the string
   *  <code>sep</code>.
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
   */
  def addString(b: StringBuilder, sep: String): StringBuilder = addString(b, "", sep, "")

  /** Write all elements of this string into given string builder without using
   *  any separator between consecutive elements.
   */
  def addString(b: StringBuilder): StringBuilder = addString(b, "")

  override def toString = mkString(stringPrefix + "(", ", ", ")")

  /** Defines the prefix of this object's <code>toString</code> representation.
   */
  def stringPrefix : String = {
    var string = thisCollection.getClass.getName
    val idx1 = string.lastIndexOf('.' : Int)
    if (idx1 != -1) string = string.substring(idx1 + 1)
    val idx2 = string.indexOf('$')
    if (idx2 != -1) string = string.substring(0, idx2)
    string
  }

  /** Creates a view of this traversable @see TraversableView
   */
  def view = new TraversableView[A, This] {
    protected lazy val underlying = self.thisCollection
    override def foreach[B](f: A => B) = self foreach f
  }

  /** A sub-traversable  starting at index `from`
   *  and extending up to (but not including) index `until`.
   *
   *  @param from   The index of the first element of the slice
   *  @param until  The index of the element following the slice
   *  @note  The difference between `view` and `slice` is that `view` produces
   *         a view of the current iterable, whereas `slice` produces a new iterable.
   *
   *  @note  Might return different results for different runs, unless this iterable is ordered
   *  @note view(from, to)  is equivalent to view.slice(from, to)
   */
  def view(from: Int, until: Int): TraversableView[A, This] = view.slice(from, until)
}
