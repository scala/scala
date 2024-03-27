//> abusing options -Vlog:tailcalls -Vdebug -Vprint:~tailcalls

import annotation.tailrec

object Test {
  @tailrec
  def lazyFilter[E](s: LazyList[E], p: E => Boolean): LazyList[E] = s match {
    case h #:: t => if (p(h)) h #:: lazyFilter(t, p) else lazyFilter(t, p)
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
      val g: Int => Int = f(_)
      f(i - 1)
    }
    else f(i - 1)

  var sz = 3
  def remove(idx: Int) =
    if (idx >= 0 && idx < sz)
      sz -= 1
    else throw new IndexOutOfBoundsException(s"$idx is out of bounds (min 0, max ${sz-1})")

  @tailrec final def remove(idx: Int, count: Int): Unit =
    if (count > 0) {
      remove(idx) // after rewrite, don't flag me as a leftover tailrec
      remove(idx, count-1)
    }
}
