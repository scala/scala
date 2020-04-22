package tastytest

sealed trait DelayedInternal[E]

object DelayedInternal {
  object Internal {
    final class Instance(value: Any) extends DelayedInternal[Nothing]
  }
}
