object Test {
  // @annotation.tailrec
  def lazyFilter[E](s: Stream[E], p: E => Boolean): Stream[E] = s match {
    case h #:: t => if (p(h)) h #:: lazyFilter(t, p) else lazyFilter(t, p)
  }
}
