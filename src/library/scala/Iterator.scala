/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala


import Predef._
import collection.mutable.{Buffer, ArrayBuffer}
import compat.StringBuilder

/** The <code>Iterator</code> object provides various functions for
 *  creating specialized iterators.
 *
 *  @author  Martin Odersky
 *  @author  Matthias Zenger
 *  @version 1.2, 10/02/2007
 */
object Iterator {

  val empty = new Iterator[Nothing] {
    def hasNext: Boolean = false
    def next(): Nothing = throw new NoSuchElementException("next on empty iterator")
  }

  /**
   *  @param x the element
   *  @return  the iterator with one single element
   */
  def single[a](x: a) = new Iterator[a] {
    private var hasnext = true
    def hasNext: Boolean = hasnext
    def next(): a =
      if (hasnext) { hasnext = false; x }
      else throw new NoSuchElementException("next on empty iterator")
  }

  def fromValues[a](xs: a*) = xs.elements

  /**
   *  @param xs the array of elements
   *  @return   the iterator on <code>xs</code>.
   */
  def fromArray[a](xs: Array[a]): Iterator[a] =
    fromArray(xs, 0, xs.length)

  /**
   *  @param xs     the array of elements
   *  @param start  ...
   *  @param length ...
   *  @return       ...
   */
  def fromArray[a](xs: Array[a], start: Int, length: Int): Iterator[a] =
    new BufferedIterator[a] {
      private var i = start
      val end = if ((start + length) < xs.length) start else xs.length
      def hasNext: Boolean = i < end
      def next(): a = if (hasNext) { val x = xs(i) ; i = i + 1 ; x }
                      else throw new NoSuchElementException("next on empty iterator")
      def head: a = if (hasNext) xs(i);
                    else throw new NoSuchElementException("head on empty iterator")
  }

  /**
   *  @param str the given string
   *  @return    the iterator on <code>str</code>
   */
  def fromString(str: String): Iterator[Char] =
    new BufferedIterator[Char] {
      private var i = 0
      private val len = str.length()
      def hasNext = i < len
      def next() = { val c = str charAt i; i = i + 1; c }
      def head = str charAt i
    }

  /**
   *  @param n the product arity
   *  @return  the iterator on <code>Product&lt;n&gt;</code>.
   */
  def fromProduct(n: Product): Iterator[Any] = new Iterator[Any] {
    private var c: Int = 0
    private val cmax = n.arity
    def hasNext = c < cmax
    def next() = { val a = n element c; c = c + 1; a }
  }

  /**
   * @deprecated use <code>fromProduct</code> instead.
   */
  @deprecated
  def fromCaseClass(n: Product) = fromProduct(n)

  /** Create an iterator with elements
   *  <code>e<sub>n+1</sub> = e<sub>n</sub> + 1</code>
   *  where <code>e<sub>0</sub> = lo</code>
   *  and <code>e<sub>i</sub> &lt; end</code>.
   *
   *  @param lo  the start value of the iterator
   *  @param end the end value of the iterator
   *  @return    the iterator with values in range <code>[lo;end)</code>.
   */
  def range(lo: Int, end: Int): Iterator[Int] =
    range(lo, end, 1)

  /** Create an iterator with elements
   *  <code>e<sub>n+1</sub> = e<sub>n</sub> + step</code>
   *  where <code>e<sub>0</sub> = lo</code>
   *  and <code>e<sub>i</sub> &lt; end</code>.
   *
   *  @param lo   the start value of the iterator
   *  @param end  the end value of the iterator
   *  @param step the increment value of the iterator (must be positive or negative)
   *  @return     the iterator with values in range <code>[lo;end)</code>.
   */
  def range(lo: Int, end: Int, step: Int): Iterator[Int] = {
    assert(step != 0)
    new BufferedIterator[Int] {
      private var i = lo
      def hasNext: Boolean = if (step > 0) i < end else i > end
      def next(): Int =
        if (hasNext) { val j = i; i = i + step; j }
        else throw new NoSuchElementException("next on empty iterator")
      def head: Int =
        if (hasNext) i
        else throw new NoSuchElementException("head on empty iterator")
    }
  }

  /** Create an iterator with elements
   *  <code>e<sub>n+1</sub> = step(e<sub>n</sub>)</code>
   *  where <code>e<sub>0</sub> = lo</code>
   *  and <code>e<sub>i</sub> &lt; end</code>.
   *
   *  @param lo   the start value of the iterator
   *  @param end  the end value of the iterator
   *  @param step the increment function of the iterator
   *  @return     the iterator with values in range <code>[lo;end)</code>.
   */
  def range(lo: Int, end: Int, step: Int => Int): Iterator[Int] =
    new BufferedIterator[Int] {
      private var i = lo
      def hasNext: Boolean =  i < end
      def next(): Int =
        if (i < end) { val j = i; i = step(i); j }
        else throw new NoSuchElementException("next on empty iterator")
      def head: Int =
        if (i < end) i
        else throw new NoSuchElementException("head on empty iterator")
    }

  /** Create an iterator with elements
   *  <code>e<sub>n+1</sub> = e<sub>n</sub> + 1</code>
   *  where <code>e<sub>0</sub> = lo</code>.
   *
   *  @param lo the start value of the iterator
   *  @return   the iterator starting at value <code>lo</code>.
   */
  def from(lo: Int): Iterator[Int] =
    from(lo, 1)

  /** Create an iterator with elements
   * <code>e<sub>n+1</sub> = e<sub>n</sub> + step</code>
   *  where <code>e<sub>0</sub> = lo</code>.
   *
   *  @param lo   the start value of the iterator
   *  @param step the increment value of the iterator
   *  @return     the iterator starting at value <code>lo</code>.
   */
  def from(lo: Int, step: Int): Iterator[Int] =
    new BufferedIterator[Int] {
      private var i = lo
      def hasNext: Boolean = true
      def next(): Int = { val j = i; i = i + step; j }
      def head: Int = i
    }

  /** Create an iterator with elements
   *  <code>e<sub>n+1</sub> = step(e<sub>n</sub>)</code>
   *  where <code>e<sub>0</sub> = lo</code>.
   *
   *  @param lo   the start value of the iterator
   *  @param step the increment function of the iterator
   *  @return     the iterator starting at value <code>lo</code>.
   */
  def from(lo: Int, step: Int => Int): Iterator[Int] =
    new BufferedIterator[Int] {
      private var i = lo
      def hasNext: Boolean = true
      def next(): Int = { val j = i; i = step(i); j }
      def head: Int = i
    }

}

/** Iterators are data structures that allow to iterate over a sequence
 *  of elements. They have a <code>hasNext</code> method for checking
 *  if there is a next element available, and a <code>next</code> method
 *  which returns the next element and discards it from the iterator.
 *
 *  @author  Martin Odersky, Matthias Zenger
 *  @version 1.2, 15/03/2004
 */
trait Iterator[+A] {

  /** Does this iterator provide another element?
   */
  def hasNext: Boolean

  /** Returns the next element.
   */
  def next(): A

  /** Returns a new iterator that iterates only over the first <code>n</code>
   *  elements.
   *
   *  @param n the number of elements to take
   *  @return  the new iterator
   */
  def take(n: Int) = new Iterator[A] {
    var remaining = n
    def hasNext = remaining > 0 && Iterator.this.hasNext
    def next(): A =
      if (hasNext) { remaining = remaining - 1; Iterator.this.next }
      else throw new NoSuchElementException("next on empty iterator")
  }

  /** Removes the first <code>n</code> elements from this iterator.
   *
   *  @param n the number of elements to drop
   *  @return  the new iterator
   */
  def drop(n: Int): Iterator[A] =
    if (n > 0) { next; drop(n - 1) } else this

  /** Returns a new iterator that maps all elements of this iterator
   *  to new elements using function <code>f</code>.
   */
  def map[B](f: A => B): Iterator[B] = new Iterator[B] {
    def hasNext = Iterator.this.hasNext
    def next() = f(Iterator.this.next)
  }

  /** Returns a new iterator that first yields the elements of this
   *  iterator followed by the elements provided by iterator <code>that</code>.
   *  @deprecated  use <code>++</code>
   */
  def append[B >: A](that: Iterator[B]) = new Iterator[B] {
    def hasNext = Iterator.this.hasNext || that.hasNext
    def next() = if (Iterator.this.hasNext) Iterator.this.next else that.next
  }

  /** Returns a new iterator that first yields the elements of this
   *  iterator followed by the elements provided by iterator <code>that</code>.
   */
  def ++[B >: A](that: Iterator[B]) = new Iterator[B] {
    def hasNext = Iterator.this.hasNext || that.hasNext
    def next() = if (Iterator.this.hasNext) Iterator.this.next else that.next
  }

  /** Applies the given function <code>f</code> to each element of
   *  this iterator, then concatenates the results.
   *
   *  @param f the function to apply on each element.
   *  @return  an iterator over <code>f(a<sub>0</sub>), ... ,
   *           f(a<sub>n</sub>)</code> if this iterator yields the
   *           elements <code>a<sub>0</sub>, ..., a<sub>n</sub></code>.
   */
  def flatMap[B](f: A => Iterator[B]): Iterator[B] = new Iterator[B] {
    private var cur: Iterator[B] = Iterator.empty
    def hasNext: Boolean =
      if (cur.hasNext) true
      else if (Iterator.this.hasNext) {
        cur = f(Iterator.this.next)
        hasNext
      } else false
    def next(): B =
      if (cur.hasNext) cur.next
      else if (Iterator.this.hasNext) {
        cur = f(Iterator.this.next)
        next
      } else throw new NoSuchElementException("next on empty iterator")
  }

  private def predicatedIterator(p: A => boolean, isFilter: boolean) = new BufferedIterator[A] {
    private var hd: A = _
    private var ahead: Boolean = false
    private def skip: Unit =
      while (!ahead && Iterator.this.hasNext) {
        hd = Iterator.this.next
        ahead = !isFilter || p(hd)
      }
    def hasNext: Boolean = {
      skip
      ahead && p(hd)
    }
    def next(): A =
      if (hasNext) { ahead = false; hd }
      else throw new NoSuchElementException("next on empty iterator")
    def head: A = { skip; hd }
  }

  /** Returns an iterator over all the elements of this iterator that
   *  satisfy the predicate <code>p</code>. The order of the elements
   *  is preserved.
   *
   *  @param p the predicate used to filter the iterator.
   *  @return  the elements of this iterator satisfying <code>p</code>.
   */
  def filter(p: A => Boolean): Iterator[A] = predicatedIterator(p, true)

  /** Returns an iterator over the longest prefix of this iterator such that
   *  all elements of the result satisfy the predicate <code>p</code>.
   *  The order of the elements is preserved.
   *
   *  @param p the predicate used to filter the iterator.
   *  @return  the longest prefix of this iterator satisfying <code>p</code>.
   */
  def takeWhile(p: A => Boolean): Iterator[A] = predicatedIterator(p, false)

  /** Skips longest sequence of elements of this iterator which satisfy given
   *  predicate <code>p</code>, and returns an iterator of the remaining elements.
   *
   *  @param p the predicate used to skip elements.
   *  @return  an iterator consisting of the remaining elements
   */
  def dropWhile(p: A => Boolean): Iterator[A] =
    if (hasNext) {
      val x = next
      if (p(x)) dropWhile(p)
      else Iterator.single(x) append this
    } else this

  /** Return an iterator formed from this iterator and the specified iterator
   *  <code>that</code> by associating each element of the former with
   *  the element at the same position in the latter.
   *  If one of the two iterators is longer than the other, its remaining elements are ignored.
   *
   *  @return     an iterator yielding <code>{a<sub>0</sub>,b<sub>0</sub>},
   *              {a<sub>1</sub>,b<sub>1</sub>}, ...</code> where
   *              <code>a<sub>i</sub></code> are the elements from this iterator
   *              and <code>b<sub>i</sub></code> are the elements from iterator
   *              <code>that</code>.
   */
  def zip[B](that: Iterator[B]) = new Iterator[(A, B)] {
    def hasNext = Iterator.this.hasNext && that.hasNext
    def next = (Iterator.this.next, that.next)
  }

  /** Return an iterator that pairs each element of this iterator
   *  with its index, counting from 0.
   *
   *  @param start the index of the first element.
   *  @return      an iterator yielding <code>{a<sub>0</sub>,0},
   *               {a<sub>1</sub>,1}...</code> where <code>a<sub>i</sub></code>
   *               are the elements from this iterator.
   */
  def zipWithIndex = new Iterator[(A, int)] {
    var idx = 0
    def hasNext = Iterator.this.hasNext
    def next = {
      val ret = (Iterator.this.next, idx)
      idx = idx + 1
      ret
    }
  }

  /** Apply a function <code>f</code> to all elements of this
   *  iterable object.
   *
   *  @param  f   a function that is applied to every element.
   */
  def foreach(f: A => Unit): Unit = while (hasNext) f(next)

  /** Apply a predicate <code>p</code> to all elements of this
   *  iterable object and return <code>true</code> iff the predicate yields
   *  <code>true</code> for all elements.
   *
   *  @param p the predicate
   *  @return  <code>true</code> iff the predicate yields <code>true</code>
   *           for all elements.
   */
  def forall(p: A => Boolean): Boolean = {
    var res = true
    while (res && hasNext) { res = p(next) }
    res
  }

  /** Apply a predicate <code>p</code> to all elements of this
   *  iterable object and return true, iff there is at least one
   *  element for which <code>p</code> yields <code>true</code>.
   *
   *  @param p the predicate
   *  @return  <code>true</code> iff the predicate yields <code>true</code>
   *           for at least one element.
   */
  def exists(p: A => Boolean): Boolean = {
    var res = false
    while (!res && hasNext) { res = p(next) }
    res
  }

  /** Tests if the given value <code>elem</code> is a member of this iterator.
   *
   *  @param elem element whose membership has to be tested.
   *  @return     <code>true</code> iff there is an element of this iterator which
   *              is equal (w.r.t. <code>==</code>) to <code>elem</code>.
   */
  def contains(elem: Any): Boolean = exists { x => x == elem }

  /** Find and return the first element of the iterable object satisfying a
   *  predicate, if any.
   *
   *  @param p the predicate
   *  @return  the first element in the iterable object satisfying
   *           <code>p</code>, or <code>None</code> if none exists.
   */
  def find(p: A => Boolean): Option[A] = {
    var res: Option[A] = None
    while (res.isEmpty && hasNext) {
      val e = next
      if (p(e)) res = Some(e)
    }
    res
  }

  /** Combines the elements of this iterator together using the binary
   *  operator <code>op</code>, from left to right, and starting with
   *  the value <code>z</code>.
   *
   *  @return <code>op(... (op(op(z,a<sub>0</sub>),a<sub>1</sub>) ...),
   *          a<sub>n</sub>)</code> if the iterator yields elements
   *          <code>a<sub>0</sub>, a<sub>1</sub>, ..., a<sub>n</sub></code>.
   */
  def foldLeft[B](z: B)(op: (B, A) => B): B = {
    var acc = z
    while (hasNext) { acc = op(acc, next) }
    acc
  }

  /** Combines the elements of this iterator together using the binary
   *  operator <code>op</code>, from right to left, and starting with
   *  the value <code>z</code>.
   *
   *  @return <code>a<sub>0</sub> op (... op (a<sub>n</sub> op z)...)</code>
   *          if the iterator yields elements <code>a<sub>0</sub>, a<sub>1</sub>, ...,
   *          a<sub>n</sub></code>.
   */
  def foldRight[B](z: B)(op: (A, B) => B): B = {
    def fold(z: B): B = if (hasNext) op(next, fold(z)) else z
    fold(z)
  }

  /** Similar to <code>foldLeft</code> but can be used as
   *  an operator with the order of iterator and zero arguments reversed.
   *  That is, <code>z /: xs</code> is the same as <code>xs foldLeft z</code>.
   *
   *  @param z the left argument of the first application of <code>op</code>
   *           (evaluation occurs from left to right).
   *  @param op the applied operator.
   *  @return  the result value
   *  @see     <code><a href="#foldLeft">foldLeft</a></code>.
   */
  def /:[B](z: B)(op: (B, A) => B): B = foldLeft(z)(op)

  /** An alias for <code>foldRight</code>.
   *  That is, <code>xs :\ z</code> is the same as <code>xs foldRight z</code>.
   *
   *  @param z the right argument of the first application of <code>op</code>
   *           (evaluation occurs from right to left).
   *  @param op the applied operator.
   *  @return  the result value.
   *  @see     <code><a href="#foldRight">foldRight</a></code>.
   */
  def :\[B](z: B)(op: (A, B) => B): B = foldRight(z)(op)

  /** Combines the elements of this iterator together using the binary
   *  operator <code>op</code>, from left to right
   *  @param op  The operator to apply
   *  @return <code>op(... op(a<sub>0</sub>,a<sub>1</sub>), ..., a<sub>n</sub>)</code>
      if the iterator yields elements
   *          <code>a<sub>0</sub>, a<sub>1</sub>, ..., a<sub>n</sub></code>.
   *  @throws Predef.UnsupportedOperationException if the iterator is empty.
   */
  def reduceLeft[B >: A](op: (B, B) => B): B = {
    if (hasNext) foldLeft[B](next)(op)
    else throw new UnsupportedOperationException("empty.reduceLeft")
  }

  /** Combines the elements of this iterator together using the binary
   *  operator <code>op</code>, from right to left
   *  @param op  The operator to apply
   *
   *  @return <code>a<sub>0</sub> op (... op (a<sub>n-1</sub> op a<sub>n</sub>)...)</code>
   *          if the iterator yields elements <code>a<sub>0</sub>, a<sub>1</sub>, ...,
   *          a<sub>n</sub></code>.

   *  @throws Predef.UnsupportedOperationException if the iterator is empty.
   */
  def reduceRight[B >: A](op: (B, B) => B): B = {
    if (!hasNext) throw new UnsupportedOperationException("empty.reduceRight")
    val x = next
    if (hasNext) op(x, reduceRight(op))
    else x
  }

  /** Returns a buffered iterator from this iterator.
   */
  def buffered: BufferedIterator[A] = new BufferedIterator[A] {
    private var hd: A = _
    private var ahead: Boolean = false
    def head: A = {
      if (!ahead) {
        hd = Iterator.this.next
        ahead = true
      }
      hd
    }
    def next: A =
      if (ahead) { ahead = false; hd } else head;
    def hasNext: Boolean = ahead || Iterator.this.hasNext
  }

  /** Returns a counted iterator from this iterator.
   */
  def counted = new CountedIterator[A] {
    private var cnt = -1
    def count = cnt
    def hasNext: Boolean = Iterator.this.hasNext
    def next: A = { cnt = cnt + 1; Iterator.this.next }
  }

  /** Creates two new iterators that both iterate over the same elements
   *  than this iterator (in the same order).
   *
   *  @return a pair of iterators
   */
  def duplicate: (Iterator[A], Iterator[A]) = {
    var xs: List[A] = Nil
    var ahead: Iterator[A] = null
    class Partner extends Iterator[A] {
      var ys: List[A] = Nil
      def hasNext: Boolean = Iterator.this.synchronized (
        ((this == ahead) && Iterator.this.hasNext) ||
        ((this != ahead) && (!xs.isEmpty || !ys.isEmpty || Iterator.this.hasNext))
      )
      def next: A = Iterator.this.synchronized {
        if (this == ahead) {
          val e = Iterator.this.next
          xs = e :: xs; e
        } else {
          if (ys.isEmpty) {
            ys = xs.reverse
            xs = Nil
          }
          ys match {
            case Nil =>
              val e = Iterator.this.next
              ahead = this
              xs = e :: xs; e
            case z :: zs =>
              ys = zs; z
          }
        }
      }
    }
    ahead = new Partner
    (ahead, new Partner)
  }

  /** Fills the given array <code>xs</code> with the elements of
   *  this sequence starting at position <code>start</code>.
   *
   *  @param  xs    the array to fill.
   *  @param  start the starting index.
   *  @pre          the array must be large enough to hold all elements.
   */
  def copyToArray[B >: A](xs: Array[B], start: Int): Unit = {
    var i = start
    while (hasNext) {
      xs(i) = next
      i = i + 1
    }
  }

  /** Copy all elements to a buffer
   *  @param   The buffer to which elements are copied
   *  @return  The buffer to which elements are copied
   */
  def copyToBuffer[B >: A](dest: Buffer[B]): Unit =
    while (hasNext) dest += next

  /** Transform this iterator into a list of all elements.
   *
   *  @return  a list which enumerates all elements of this iterator.
   */
  def toList: List[A] = {
    val res = new collection.mutable.ListBuffer[A]
    while (hasNext) {
      res += next
    }
    res.toList
  }

  /** Returns a string representation of the elements in this iterator. The resulting string
   *  begins with the string <code>start</code> and is finished by the string
   *  <code>end</code>. Inside, the string representations of elements (w.r.t.
   *  the method <code>toString()</code>) are separated by the string
   *  <code>sep</code>.
   *  <p/>
   *  Ex: <br/>
   *  <code>List(1, 2, 3).mkString("(", "; ", ")") = "(1; 2; 3)"</code>
   *
   *  @param start starting string.
   *  @param sep separator string.
   *  @param end ending string.
   *  @return a string representation of this iterable object.
   */
  def mkString(start: String, sep: String, end: String): String = {
    val buf = new StringBuilder()
    addString(buf, start, sep, end).toString
  }

  /** Returns a string representation of this iterable object. The string
   *  representations of elements (w.r.t. the method <code>toString()</code>)
   *  are separated by the string <code>sep</code>.
   *
   *  @param sep separator string.
   *  @return a string representation of this iterable object. */
  def mkString(sep: String): String = this.mkString("", sep, "")

  /** Write all elements of this string into given string builder */
  def addString(buf: StringBuilder, start: String, sep: String, end: String): StringBuilder = {
    buf.append(start)
    val elems = this
    if (elems.hasNext) buf.append(elems.next)
    while (elems.hasNext) {
      buf.append(sep); buf.append(elems.next)
    }
    buf.append(end)
  }
}
