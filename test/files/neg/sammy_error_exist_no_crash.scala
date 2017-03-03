trait F[T] { def apply(s: T): Int }

object NeedsNiceError {
  def bar(x: F[_ >: String]) = ???
  bar(_.parseInt)
}
