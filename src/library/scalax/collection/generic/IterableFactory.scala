package scalax.collection.generic

trait IterableFactory[CC[A] <: Iterable[A]] {

  /** Create CC collection of specified elements */
  def apply[A](args: A*): CC[A]

  protected def newBuilder[A]: Builder[CC, A] =
    apply().newBuilder[A].asInstanceOf[Builder[CC, A]]

  // can't have an empty here because it is defined in subclass covariant.IterableFactory with type
  // CC[Nothing]. This type does not make sense for immutable iterables.

  /** Concatenate all the argument lists into a single list.
   *
   *  @param xss the lists that are to be concatenated
   *  @return the concatenation of all the lists
   */
  def concat[A](xss: CC[A]*): CC[A] = {
    val b = newBuilder[A]
    for (xs <- xss) b ++= xs
    b.result
  }

  /** An iterable that contains the same element a number of times
   *  @param   n  The number of elements returned
   *  @param   elem The element returned each time
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

  def fill[A](n1: Int, n2: Int)(elem: => A): CC[CC[A]] =
    tabulate(n1)(_ => fill(n2)(elem))

  def fill[A](n1: Int, n2: Int, n3: Int)(elem: => A): CC[CC[CC[A]]] =
    tabulate(n1)(_ => fill(n2, n3)(elem))

  def fill[A](n1: Int, n2: Int, n3: Int, n4: Int)(elem: => A): CC[CC[CC[CC[A]]]] =
    tabulate(n1)(_ => fill(n2, n3, n4)(elem))

  def fill[A](n1: Int, n2: Int, n3: Int, n4: Int, n5: Int)(elem: => A): CC[CC[CC[CC[CC[A]]]]] =
    tabulate(n1)(_ => fill(n2, n3, n4, n5)(elem))

  def tabulate[A](n: Int)(f: Int => A): CC[A] = {
    val b = newBuilder[A]
    var i = 0
    while (i < n) {
      b += f(i)
      i += 1
    }
    b.result
  }

  def tabulate[A](n1: Int, n2: Int)(f: (Int, Int) => A): CC[CC[A]] =
    tabulate(n1)(i1 => tabulate(n2)(f(i1, _)))

  def tabulate[A](n1: Int, n2: Int, n3: Int)(f: (Int, Int, Int) => A): CC[CC[CC[A]]] =
    tabulate(n1)(i1 => tabulate(n2, n3)(f(i1, _, _)))

  def tabulate[A](n1: Int, n2: Int, n3: Int, n4: Int)(f: (Int, Int, Int, Int) => A): CC[CC[CC[CC[A]]]] =
    tabulate(n1)(i1 => tabulate(n2, n3, n4)(f(i1, _, _, _)))

  def tabulate[A](n1: Int, n2: Int, n3: Int, n4: Int, n5: Int)(f: (Int, Int, Int, Int, Int) => A): CC[CC[CC[CC[CC[A]]]]] =
    tabulate(n1)(i1 => tabulate(n2, n3, n4, n5)(f(i1, _, _, _, _)))

  /** Create a sequence of increasing integers in a range.
   *
   *  @param from the start value of the sequence
   *  @param end the end value of the sequence
   *  @return the sorted list of all from `from` (inclusive)
   *  up to, but exclusding, `end`.
   */
  def range[A](start: Int, end: Int): CC[Int] = range(start, end, 1)

  /** Create a sequence of increasing integers in a range.
   *
   *  @param from the start value of the sequence
   *  @param end the end value of the sequence
   *  @param step the increment value of successive elements
   *  @return a list of values <code>from + n * step</code> for
   *          increasing n. If `step > 0` the sequence terminates
   *          with the largest value less than `end`. If `step < 0`
   *          the sequence terminates with the smallest value greater than `end`.
   *          If `step == 0`, the sequence gors on infinitely (in that
   *          case the `range` operation might not terminate.
   */
  def range(start: Int, end: Int, step: Int): CC[Int] = {
    val b = newBuilder[Int]
    var i = start
    while ((step <= 0 || i < end) && (step >= 0 || i > end)) {
      b += i
      i += step
    }
    b.result
  }

  /** Create a sequence by repeatedly applying a given function to a start value.
   *
   *  @param start the start value of the sequence
   *  @param len   the length of the sequence
   *  @param f     the function that's repeatedly applied
   *  @return      the sequence with elements <code>(start, f(start), f(f(start)), ..., f<sup>len-1</sup>(start))</code>
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
