
//> using options -Xfatal-warnings
//
object Test {
  // @annotation.tailrec
  def lazyFilter[E](s: LazyList[E], p: E => Boolean): LazyList[E] = s match {
    case h #:: t => if (p(h)) h #:: lazyFilter(t, p) else lazyFilter(t, p)
    case _       => LazyList.empty[E]
  }
}
