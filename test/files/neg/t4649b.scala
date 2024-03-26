//> using options -Wstrict-tailrec

import annotation.tailrec

object Test {
  @tailrec
  def lazyFilter[E](s: LazyList[E], p: E => Boolean): LazyList[E] = s match {
    case h #:: t => if (p(h)) h #:: lazyFilter(t, p) else lazyFilter(t, p) // error
  }

  @tailrec
  def f(i: Int): Int =
    if (i <= 0) i
    /* not optimized
    else if (i == 27) {
      val x = f(i - 1)
      x
    }
    */
    else if (i == 42) {
      val g: Int => Int = f(_) // error
      f(i - 1)
    }
    else f(i - 1)
}
