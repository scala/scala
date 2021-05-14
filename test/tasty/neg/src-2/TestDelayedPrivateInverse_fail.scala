package tastytest

object TestDelayedPrivateInverse {
  def test: DelayedPrivateInverse.Parent[Nothing] = ??? // force sealed children of parent
  locally {
    val _ = DelayedPrivateInverse.Internal
  }
}
