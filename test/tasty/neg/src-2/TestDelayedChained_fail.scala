package tastytest

object TestDelayedChained {
  def foo(r: DelayedChained.Root): Unit = r match { case _: DelayedChained.LeafA => ()  }
}
