package tastytest

sealed trait DelayedInternalEmpty[E]

object DelayedInternalEmpty {
  object Internal {
    object Empty extends DelayedInternalEmpty[Nothing]
  }
}
