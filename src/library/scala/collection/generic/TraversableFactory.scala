 package scala.collection.generic

/** A template for companion objects of Traversable and subclasses thereof.
 */
abstract class TraversableFactory[CC[X] <: Traversable[X] with TraversableClass[X, CC]]
  extends Companion[CC] {

  class VirtualBuilderFactory[A] extends BuilderFactory[A, CC[A], CC[_]] {
    def apply(from: Coll) = from.traversableBuilder[A]
  }

  /** Concatenate all the argument collections into a single collection.
   *
   *  @param xss the collections that are to be concatenated
   *  @return the concatenation of all the collections
   */
  def concat[A](xss: Traversable[A]*): CC[A] = {
    val b = newBuilder[A]
    for (xs <- xss) b ++= xs
    b.result
  }

  /** An iterable that contains the results of some element computation a number of times.
   *  @param   n  the number of elements returned
   *  @param   elem the element computation
   */
  def fill[A](n: Int)(elem: => A): CC[A] = {
    val b = newBuilder[A]
    var i = 0
    while (i < n) {
      b += elem
      i += 1
    }
    b.result
  }

  /** A two-dimensional iterable that contains the results of some element computation a number of times.
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   elem the element computation
   */
  def fill[A](n1: Int, n2: Int)(elem: => A): CC[CC[A]] =
    tabulate(n1)(_ => fill(n2)(elem))

  /** A three-dimensional iterable that contains the results of some element computation a number of times.
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   n3  the number of elements in the 3nd dimension
   *  @param   elem the element computation
   */
  def fill[A](n1: Int, n2: Int, n3: Int)(elem: => A): CC[CC[CC[A]]] =
    tabulate(n1)(_ => fill(n2, n3)(elem))

  /** A four-dimensional iterable that contains the results of some element computation a number of times.
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   n3  the number of elements in the 3nd dimension
   *  @param   n4  the number of elements in the 4th dimension
   *  @param   elem the element computation
   */
  def fill[A](n1: Int, n2: Int, n3: Int, n4: Int)(elem: => A): CC[CC[CC[CC[A]]]] =
    tabulate(n1)(_ => fill(n2, n3, n4)(elem))

  /** A five-dimensional iterable that contains the results of some element computation a number of times.
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   n3  the number of elements in the 3nd dimension
   *  @param   n4  the number of elements in the 4th dimension
   *  @param   n5  the number of elements in the 5th dimension
   *  @param   elem the element computation
   */
  def fill[A](n1: Int, n2: Int, n3: Int, n4: Int, n5: Int)(elem: => A): CC[CC[CC[CC[CC[A]]]]] =
    tabulate(n1)(_ => fill(n2, n3, n4, n5)(elem))

  /** An iterable containing values of a given function over a range of integer values starting from 0.
   *  @param  n   The number of elements in the iterable
   *  @param  f   The function computing element values
   *  @return An iterable consisting of elements `f(0), ..., f(n -1)`
   */
  def tabulate[A](n: Int)(f: Int => A): CC[A] = {
    val b = newBuilder[A]
    var i = 0
    while (i < n) {
      b += f(i)
      i += 1
    }
    b.result
  }

  /** A two-dimensional iterable containing values of a given function over ranges of integer values starting from 0.
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   f   The function computing element values
   */
  def tabulate[A](n1: Int, n2: Int)(f: (Int, Int) => A): CC[CC[A]] =
    tabulate(n1)(i1 => tabulate(n2)(f(i1, _)))

  /** A three-dimensional iterable containing values of a given function over ranges of integer values starting from 0.
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   n3  the number of elements in the 3nd dimension
   *  @param   f   The function computing element values
   */
  def tabulate[A](n1: Int, n2: Int, n3: Int)(f: (Int, Int, Int) => A): CC[CC[CC[A]]] =
    tabulate(n1)(i1 => tabulate(n2, n3)(f(i1, _, _)))

  /** A four-dimensional iterable containing values of a given function over ranges of integer values starting from 0.
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   n3  the number of elements in the 3nd dimension
   *  @param   n4  the number of elements in the 4th dimension
   *  @param   f   The function computing element values
   */
  def tabulate[A](n1: Int, n2: Int, n3: Int, n4: Int)(f: (Int, Int, Int, Int) => A): CC[CC[CC[CC[A]]]] =
    tabulate(n1)(i1 => tabulate(n2, n3, n4)(f(i1, _, _, _)))

  /** A five-dimensional iterable containing values of a given function over ranges of integer values starting from 0.
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   n3  the number of elements in the 3nd dimension
   *  @param   n4  the number of elements in the 4th dimension
   *  @param   n5  the number of elements in the 5th dimension
   *  @param   f   The function computing element values
   */
  def tabulate[A](n1: Int, n2: Int, n3: Int, n4: Int, n5: Int)(f: (Int, Int, Int, Int, Int) => A): CC[CC[CC[CC[CC[A]]]]] =
    tabulate(n1)(i1 => tabulate(n2, n3, n4, n5)(f(i1, _, _, _, _)))

  /** An iterable containing a sequence of increasing integers in a range.
   *
   *  @param from the start value of the iterable
   *  @param end the end value of the iterable (the first value NOT returned)
   *  @return  the iterable with values in range `start, start + 1, ..., end - 1`
   *  up to, but exclusding, `end`.
   */
  def range[A](start: Int, end: Int): CC[Int] = range(start, end, 1)

  /** An iterable containing equally spaced values in some integer interval.

   *  @param start the start value of the iterable
   *  @param end   the end value of the iterable (the first value NOT returned)
   *  @param step  the increment value of the iterable (must be positive or negative)
   *  @return      the iterable with values in `start, start + step, ...` up to, but excluding `end`
   */
  def range(start: Int, end: Int, step: Int): CC[Int] = {
    if (step == 0) throw new IllegalArgumentException("zero step")
    val b = newBuilder[Int]
    var i = start
    while (if (step < 0) end < i else i < end) {
      b += i
      i += step
    }
    b.result
  }

  /** An iterable containing repeated applications of a function to a start value.
   *
   *  @param start the start value of the iterable
   *  @param len   the number of elements returned by the iterable
   *  @param f     the function that's repeatedly applied
   *  @return      the iterable returning `len` values in the sequence `start, f(start), f(f(start)), ...`
   */
  def iterate(start: Int, len: Int)(f: Int => Int): CC[Int] = {
    val b = newBuilder[Int]
    var acc = start
    var i = 0
    while (i < len) {
      b += acc
      acc = f(acc)
      i += 1
    }
    b.result
  }
}

