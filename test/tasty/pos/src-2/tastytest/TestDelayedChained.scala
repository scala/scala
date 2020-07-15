package tastytest

object TestDelayedChained {
  def test: DelayedChained.Root = ??? // logs should assert that just accessing Root forces all sealed children up to LeafA/LeafB
}
