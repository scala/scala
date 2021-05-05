package tastytest

object DelayedPrivateInverse {
  private object Internal {
    final class Impl extends DelayedPrivateInverse.Parent[Nothing]
  }
  sealed trait Parent[T]
}
