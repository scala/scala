package strawman.collection

import scala.{Array, Any, Boolean, Int, NoSuchElementException, Nothing, Unit}
import strawman.collection.mutable.ArrayBuffer

/** A core Iterator class
  *
  * @define consumesIterator
  * After calling this method, one should discard the iterator it was called
  * on. Using it is undefined and subject to change.
  */
trait Iterator[+A] extends IterableOnce[A] { self =>
  def hasNext: Boolean
  def next(): A
  def iterator() = this

  /** Tests whether a predicate holds for all values produced by this iterator.
    *  $mayNotTerminateInf
    *
    *  @param   p     the predicate used to test elements.
    *  @return        `true` if the given predicate `p` holds for all values
    *                 produced by this iterator, otherwise `false`.
    *  @note          Reuse: $consumesIterator
    */
  def forall(p: A => Boolean): Boolean = {
    var res = true
    while (res && hasNext) res = p(next())
    res
  }

  def foldLeft[B](z: B)(op: (B, A) => B): B = {
    var result = z
    while (hasNext) {
      result = op(result, next())
    }
    result
  }

  def foldRight[B](z: B)(op: (A, B) => B): B =
    if (hasNext) op(next(), foldRight(z)(op)) else z

  def foreach[U](f: A => U): Unit =
    while (hasNext) f(next())

  def indexWhere(p: A => Boolean): Int = {
    var i = 0
    while (hasNext) {
      if (p(next())) return i
      i += 1
    }
    -1
  }

  def length = {
    var len = 0
    while (hasNext) { len += 1; next() }
    len
  }

  def filter(p: A => Boolean): Iterator[A] = new Iterator[A] {
    private var hd: A = _
    private var hdDefined: Boolean = false

    def hasNext: Boolean = hdDefined || {
      do {
        if (!self.hasNext) return false
        hd = self.next()
      } while (!p(hd))
      hdDefined = true
      true
    }

    def next() =
      if (hasNext) {
        hdDefined = false
        hd
      }
      else Iterator.empty.next()
  }

  def map[B](f: A => B): Iterator[B] = new Iterator[B] {
    def hasNext = self.hasNext
    def next() = f(self.next())
  }

  def flatMap[B](f: A => IterableOnce[B]): Iterator[B] = new Iterator[B] {
    private var myCurrent: Iterator[B] = Iterator.empty
    private def current = {
      while (!myCurrent.hasNext && self.hasNext)
        myCurrent = f(self.next()).iterator()
      myCurrent
    }
    def hasNext = current.hasNext
    def next() = current.next()
  }

  def ++[B >: A](xs: IterableOnce[B]): Iterator[B] = new Iterator[B] {
    private var myCurrent: Iterator[B] = self
    private var first = true
    private def current = {
      if (!myCurrent.hasNext && first) {
        myCurrent = xs.iterator()
        first = false
      }
      myCurrent
    }
    def hasNext = current.hasNext
    def next() = current.next()
  }

  def take(n: Int): Iterator[A] = new Iterator[A] {
    private var i = 0
    def hasNext = self.hasNext && i < n
    def next() =
      if (hasNext) {
        i += 1
        self.next()
      }
      else Iterator.empty.next()
  }

  /*
   * Implemented by means of a buffer to keep track of the last n elements during iteration.
   */
  def takeRight(n: Int): Iterator[A] = {
    if (n == 0) Iterator.empty
    else {
      val buffer = ArrayBuffer[A]()
      var count = 0
      var index = 0
      // First iterate over all elements while keeping track of the last n
      while (self.hasNext) {
        if (index >= buffer.length) buffer += self.next()
        else buffer(index) = self.next()
        index = if ((index + 1) >= n) 0 else index + 1
        if (count < n) count += 1
      }
      // Adjust the starting index if needed
      index = if (index >= buffer.length) 0 else index
      // Return an iterator for the elements in the buffer starting from index
      new Iterator[A]() {
        override def hasNext: Boolean = count > 0
        override def next(): A = {
          val value = buffer(index)
          index = if (index + 1 >= buffer.length) 0 else index + 1
          count -= 1
          value
        }
      }
    }
  }

  def drop(n: Int): Iterator[A] = {
    var i = 0
    while (i < n && hasNext) {
      next()
      i += 1
    }
    this
  }

  /*
   * Implemented by means of a buffer to keep the last n elements from being returned during iteration.
   */
  def dropRight(n: Int): Iterator[A] = {
    if (n == 0) self
    else {
      val buffer = ArrayBuffer[A]()
      var index = 0
      // First fill the buffer with the first n elements (or fewer if the n is greater than the iterator length)
      while (index < n && self.hasNext) {
        buffer += self.next()
        index += 1
      }
      index = 0
      // Return an iterator that returns already buffered elements as it buffers new ones (using a buffer of most n elements)
      new Iterator[A]() {
        // End when the iterator is exhausted (without having returned the elements currently in the buffer since those are the ones to drop)
        override def hasNext: Boolean = self.hasNext
        override def next(): A = {
          val value = buffer(index)
          buffer(index) = self.next()
          index = if (index + 1 >= buffer.length) 0 else index + 1
          value
        }
      }
    }
  }

  def zip[B](that: IterableOnce[B]): Iterator[(A, B)] = new Iterator[(A, B)] {
    val thatIterator = that.iterator()
    def hasNext = self.hasNext && thatIterator.hasNext
    def next() = (self.next(), thatIterator.next())
  }

  def sameElements[B >: A](that: IterableOnce[B]): Boolean = {
    val those = that.iterator()
    while (hasNext && those.hasNext)
      if (next() != those.next())
        return false
    // At that point we know that *at least one* iterator has no next element
    // If *both* of them have no elements then the collections are the same
    hasNext == those.hasNext
  }
}

object Iterator {

  val empty: Iterator[Nothing] = new Iterator[Nothing] {
    def hasNext = false
    def next() = throw new NoSuchElementException("next on empty iterator")
  }

  def single[A](a: A): Iterator[A] = new Iterator[A] {
    private var consumed: Boolean = false
    def hasNext = !consumed
    def next() = if (consumed) empty.next() else { consumed = true; a }
  }

  def apply[A](xs: A*): Iterator[A] = new IndexedView[A] {
    val length = xs.length
    def apply(n: Int) = xs(n)
  }.iterator()

}
