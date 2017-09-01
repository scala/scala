package strawman.collection
package decorators

/** Views used by decorators */
object View {

  case class Intersperse[A](underlying: Iterable[A], sep: A) extends View[A] {
    def iterator(): Iterator[A] = underlying.iterator().intersperse(sep)

    override def knownSize: Int = if (underlying.knownSize > 0) (2 * underlying.knownSize - 1) else underlying.knownSize
  }

  case class IntersperseSurround[A](underlying: Iterable[A], start: A, sep: A, end: A) extends View[A] {
    def iterator(): Iterator[A] = underlying.iterator().intersperse(start, sep, end)

    override def knownSize: Int =
      if (underlying.knownSize > 0) (2 * underlying.knownSize + 1)
      else if (underlying.knownSize == 0) 2
      else underlying.knownSize
  }

}