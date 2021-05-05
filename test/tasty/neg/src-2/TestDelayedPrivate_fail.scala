package tastytest

object TestDelayedPrivate {

  locally {
    val _ = Nil: List[DelayedPrivate.Root] // force Root to be seen first
    DelayedPrivate.Nested.Deeper
  }
}
