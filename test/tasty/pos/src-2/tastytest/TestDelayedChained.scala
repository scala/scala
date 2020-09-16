package tastytest

object TestDelayedChained {
  compiletimeHasNestedChildren[DelayedChained.Root](
    "tastytest.DelayedChained.Internal.Branch",
    "tastytest.DelayedChained.Internal.Deeper.LeafA",
    "tastytest.DelayedChained.Internal.Deeper.LeafB",
  )
}
