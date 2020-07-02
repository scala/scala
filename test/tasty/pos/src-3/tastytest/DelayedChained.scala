package tastytest

object DelayedChained {

  sealed trait Root

  object Internal {

    sealed trait Branch extends Root

    object Deeper {
      final class LeafA extends Branch
      final class LeafB extends Branch
    }

  }

}
