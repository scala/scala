/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala


import Predef._

/** The <code>Iterator</code> object provides various functions for
 *  creating specialized iterators.
 *
 *  @author  Martin Odersky
 *  @author  Matthias Zenger
 *  @version 1.1, 04/02/2004
 */
object Iterator {

  def empty[a] = new Iterator[a] {
    def hasNext: Boolean = false
    def next: a =  throw new NoSuchElementException("next on empty iterator")
  }

  def single[a](x: a) = new Iterator[a] {
    private var hasnext = true
    def hasNext: Boolean = hasnext
    def next: a =
      if (hasnext) { hasnext = false; x }
      else throw new NoSuchElementException("next on empty iterator")
  }

  def fromValues[a](xs: a*) = xs.elements

  def fromArray[a](xs: Array[a]): Iterator[a] =
    fromArray(xs, 0, xs.length)

  /**
   *  @param xs     ...
   *  @param start  ...
   *  @param length ...
   *  @return ...
   */
  def fromArray[a](xs: Array[a], start: Int, length: Int): Iterator[a] =
    new BufferedIterator[a] {
      private var i = start
      val end = if ((start + length) < xs.length) start else xs.length
      def hasNext: Boolean = i < end
      def next: a = if (hasNext) { val x = xs(i) ; i = i + 1 ; x }
                    else throw new NoSuchElementException("next on empty iterator")
      def head: a = if (hasNext) xs(i);
                    else throw new NoSuchElementException("head on empty iterator")
  }

  def fromString(str: String): Iterator[Char] =
    new BufferedIterator[Char] {
      private var i = 0
      private val len = str.length()
      def hasNext = i < len
      def next = { val c = str charAt i; i = i + 1; c }
      def head = str charAt i
    }

  def fromProduct(n:Product): Iterator[Any] = new Iterator[Any] {
    private var c: Int = 0
    private val cmax = n.arity
    def hasNext = c < cmax
    def next = { val a = n element c; c = c + 1; a }
  }

  /** deprecated */
  def fromCaseClass(n:Product) = fromProduct(n)

  /** Create an iterator with elements
   *  <code>e<sub>n+1</sub> = e<sub>n</sub> + 1</code>
   *  where <code>e<sub>0</sub> = lo</code>
   *  and <code>e<sub>i</sub> &lt; end</code>.
   *
   *  @param lo the start value of the iterator
   *  @param end the end value of the iterator
   *  @return the iterator with values in range [lo;end).
   */
  def range(lo: Int, end: Int): Iterator[Int] =
    range(lo, end, 1)

  /** Create an iterator with elements
   *  <code>e<sub>n+1</sub> = e<sub>n</sub> + step</code>
   *  where <code>e<sub>0</sub> = lo</code>
   *  and <code>e<sub>i</sub> &lt; end</code>.
   *
   *  @param lo the start value of the iterator
   *  @param end the end value of the iterator
   *  @param step the increment value of the iterator (must be positive or negative)
   *  @return the iterator with values in range [lo;end).
   */
  def range(lo: Int, end: Int, step: Int): Iterator[Int] = {
    assert(step != 0)
    new BufferedIterator[Int] {
      private var i = lo
      def hasNext: Boolean = if (step > 0) i < end else i > end
      def next: Int =
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
   *  @param lo the start value of the iterator
   *  @param end the end value of the iterator
   *  @param step the increment function of the iterator
   *  @return the iterator with values in range [lo;end).
   */
  def range(lo: Int, end: Int, step: Int => Int): Iterator[Int] =
    new BufferedIterator[Int] {
      private var i = lo
      def hasNext: Boolean =  i < end
      def next: Int =
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
   *  @return the iterator starting at value <code>lo</code>.
   */
  def from(lo: Int): Iterator[Int] =
    from(lo, 1)

  /** Create an iterator with elements
   * <code>e<sub>n+1</sub> = e<sub>n</sub> + step</code>
   *  where <code>e<sub>0</sub> = lo</code>.
   *
   *  @param lo the start value of the iterator
   *  @param step the increment value of the iterator
   *  @return the iterator starting at value <code>lo</code>.
   */
  def from(lo: Int, step: Int): Iterator[Int] =
    new BufferedIterator[Int] {
      private var i = lo
      def hasNext: Boolean = true
      def next: Int = { val j = i; i = i + step; j }
      def head: Int = i
    }

  /** Create an iterator with elements
   *  <code>e<sub>n+1</sub> = step(e<sub>n</sub>)</code>
   *  where <code>e<sub>0</sub> = lo</code>.
   *
   *  @param lo the start value of the iterator
   *  @param step the increment function of the iterator
   *  @return the iterator starting at value <code>lo</code>.
   */
  def from(lo: Int, step: Int => Int): Iterator[Int] =
    new BufferedIterator[Int] {
      private var i = lo
      def hasNext: Boolean = true
      def next: Int = { val j = i; i = step(i); j }
      def head: Int = i
    }

}

/** Iterators are data structures that allow to iterate over a sequence
 *  of elements. They have a <code>hasNext</code> method for checking
 *  if there is a next element available, and a <code>next</code> method
 *  which returns the next element and discards it from the iterator.
 *
 *  @author  Martin Odersky
 *  @author  Matthias Zenger
 *  @version 1.2, 15/03/2004
 */
trait Iterator[+A] {

  /** Does this iterator provide another element?
   */
  def hasNext: Boolean

  /** Returns the next element.
   */
  def next: A

  /** Returns a new iterator that iterates only over the first <code>n</code>
   *  elements.
   */
  def take(n: Int) = new Iterator[A] {
    var remaining = n
    def hasNext = remaining > 0 && Iterator.this.hasNext
    def next: A =
      if (hasNext) { remaining = remaining - 1; Iterator.this.next }
      else throw new NoSuchElementException("next on empty iterator")
  }

  /** Removes the first <code>n</code> elements from this iterator.
   */
  def drop(n: Int): Iterator[A] =
    if (n > 0) { next; drop(n - 1) } else this

  /** Returns a new iterator that maps all elements of this iterator
   *  to new elements using function <code>f</code>.
   */
  def map[B](f: A => B): Iterator[B] = new Iterator[B] {
    def hasNext = Iterator.this.hasNext
    def next = f(Iterator.this.next)
  }

  /** Returns a new iterator that first yields the elements of this
   *  iterator followed by the elements provided by iterator <code>that</code>.
   */
  def append[B >: A](that: Iterator[B]) = new Iterator[B] {
    def hasNext = Iterator.this.hasNext || that.hasNext
    def next = if (Iterator.this.hasNext) Iterator.this.next else that.next
  }

  /** Applies the given function <code>f</code> to each element of
   *  this iterator, then concatenates the results.
   *
   *  @param f the function to apply on each element.
   *  @return an iterator over <code>f(a0), ... , f(an)</code> if this iterator
   *          yields the elements <code>a0, ..., an</code>.
   */
  def flatMap[B](f: A => Iterator[B]): Iterator[B] = new Iterator[B] {
    private var cur: Iterator[B] = Iterator.empty
    def hasNext: Boolean =
      if (cur.hasNext) true
      else if (Iterator.this.hasNext) {
        cur = f(Iterator.this.next)
        hasNext
      } else false
    def next: B =
      if (cur.hasNext) cur.next
      else if (Iterator.this.hasNext) {
        cur = f(Iterator.this.next)
        next
      } else throw new NoSuchElementException("next on empty iterator")
  }

  /** Returns an iterator over all the elements of this iterator that
   *  satisfy the predicate <code>p</code>. The order of the elements
   *  is preserved.
   *
   *  @param p the predicate used to filter the iterator.
   *  @return  the elements of this iterator satisfying <code>p</code>.
   */
  def filter(p: A => Boolean): Iterator[A] = new BufferedIterator[A] {
    private var hd: A = _
    private var ahead: Boolean = false
    private var hasMore = Iterator.this.hasNext
    private def skip: Unit =
      while (!ahead && hasMore) {
        hd = Iterator.this.next
        hasMore = Iterator.this.hasNext
        ahead = p(hd)
      }
    def hasNext: Boolean = { skip; ahead || hasMore }
    def next: A =
      if (hasNext) { ahead = false; hd }
      else throw new NoSuchElementException("next on empty iterator");
    def head: A = { skip; hd }
 }

  /** Return an iterator formed from this iterator and the specified iterator
   *  <code>that</code> by associating each element of the former with
   *  the element at the same position in the latter.
   *
   *  @param   <code>that</code> must have the same number of elements as this
   *           iterator.
   *  @return  an iterator yielding <code>(a0,b0), ..., (an,bn)</code> where
   *           <code>ai</code> are the elements from this iterator and
   *           <code>bi</code> are the elements from iterator <code>that</code>.
   */
  def zip[B](that: Iterator[B]) = new Iterator[Pair[A, B]] {
    def hasNext = Iterator.this.hasNext && that.hasNext
    def next = Pair(Iterator.this.next, that.next)
  }


  /** Return an iterator that pairs each element of this iterator
   *  with its index, counting from 0.
   *
   *  @param   <code>start</code> the index of the first element
   *
   *  @return  an iterator yielding <code>(a0,0), (a0,1)...</code>
   *           where <code>ai</code> are the elements from this iterator.
   */
  def zipWithIndex = new Iterator[Pair[A, int]] {
    var idx = 0
    def hasNext = Iterator.this.hasNext
    def next = {
      val ret = Pair(Iterator.this.next, idx)
      idx = idx + 1
      ret
    }
  }

  /** Apply a function <code>f</code> to all elements of this
   *  iterable object.
   *
   *  @param  f   a function that is applied to every element.
   */
  def foreach(f: A => Unit): Unit = while (hasNext) { f(next) }

  /** Apply a predicate <code>p</code> to all elements of this
   *  iterable object and return true, iff the predicate yields
   *  true for all elements.
   *
   *  @param   p     the predicate
   *  @returns true, iff the predicate yields true for all elements.
   */
  def forall(p: A => Boolean): Boolean = {
    var res = true
    while (res && hasNext) { res = p(next) }
    res
  }

  /** Apply a predicate <code>p</code> to all elements of this
   *  iterable object and return true, iff there is at least one
   *  element for which <code>p</code> yields true.
   *
   *  @param   p     the predicate
   *  @returns true, iff the predicate yields true for at least one element.
   */
  def exists(p: A => Boolean): Boolean = {
    var res = false
    while (!res && hasNext) { res = p(next) }
    res
  }

  /** Tests if the given value <code>elem</code> is a member of this list.
   *
   *  @param elem element whose membership has to be tested.
   *  @return True iff there is an element of this list which is
   *  equal (w.r.t. <code>==</code>) to <code>elem</code>.
   */
  def contains(elem: Any): Boolean = exists { x => x == elem }

  /** Find and return the first element of the iterable object satisfying a
   *  predicate, if any.
   *
   *  @param p the predicate
   *  @return the first element in the iterable object satisfying <code>p</code>,
   *  or <code>None</code> if none exists.
   */
  def find(p: A => Boolean): Option[A] = {
    var res: Option[A] = None
    while (res.isEmpty && hasNext) {
      val e = next
      if (p(e)) res = Some(e);
    }
    res
  }

  /** Combines the elements of this list together using the binary
   *  operator <code>op</code>, from left to right, and starting with
   *  the value <code>z</code>.
   *  @return <code>op(... (op(op(z,a0),a1) ...), an)</code> if the list
   *  is <code>List(a0, a1, ..., an)</code>.
   */
  def foldLeft[B](z: B)(op: (B, A) => B): B = {
    var acc = z
    while (hasNext) { acc = op(acc, next) }
    acc
  }

  /** Combines the elements of this list together using the binary
   *  operator <code>op</code>, from rigth to left, and starting with
   *  the value <code>z</code>.
   *  @return <code>a0 op (... op (an op z)...)</code> if the list
   *  is <code>[a0, a1, ..., an]</code>.
   */
  def foldRight[B](z: B)(op: (A, B) => B): B = {
    def fold(z: B): B =
      if (hasNext) op(next, fold(z)) else z;
    fold(z)
  }

  /** Similar to <code>foldLeft</code> but can be used as
   *  an operator with the order of list and zero arguments reversed.
   *  That is, <code>z /: xs</code> is the same as <code>xs foldLeft z</code>
   */
  def /:[B](z: B)(f: (B, A) => B): B = foldLeft(z)(f)

  /** An alias for <code>foldRight</code>.
   *  That is, <code>xs :\ z</code> is the same as <code>xs foldRight z</code>
   */
  def :\[B](z: B)(f: (A, B) => B): B = foldRight(z)(f)

  /** Returns a buffered iterator from this iterator.
   */
  def buffered: BufferedIterator[A] = new BufferedIterator[A] {
    private var hd: A = _
    private var ahead: Boolean = false
    def head: A = {
      if (!ahead) {
        hd = Iterator.this.next;
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
   */
  def duplicate: Pair[Iterator[A], Iterator[A]] = {
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
    Pair(ahead, new Partner)
  }

  /** Fills the given array <code>xs</code> with the elements of
   *  this sequence starting at position <code>start</code>.
   *
   *  @param  xs the array to fill.
   *  @param  start starting index.
   *  @return the given array <code>xs</code> filled with the elements of
   *          this iterator.
   *  @pre The array must be large enough to hold all elements.
   */
  def copyToArray[B >: A](xs: Array[B], start: Int): Array[B] = {
    var i = start
    while (hasNext) {
      xs(i) = next
      i = i + 1
    }
    xs
  }

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

}
