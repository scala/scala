package tastytest

object TestDelayedInternal2 {
  def test: DelayedInternal[Nothing] = new DelayedInternal.Internal.Instance("") // error
}
