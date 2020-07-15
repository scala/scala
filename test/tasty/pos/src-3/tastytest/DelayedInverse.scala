package tastytest

object DelayedInverse {
  object Internal {
    final class Impl(value: Any) extends DelayedInverse.Parent[Nothing]
  }
  sealed trait Parent[T]
}
