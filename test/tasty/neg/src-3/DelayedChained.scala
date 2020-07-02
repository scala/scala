package tastytest

object DelayedChained {

  sealed trait Root

  final class LeafA extends Root

  object Internal {

    sealed trait Branch extends Root

    object Deeper {
      final class LeafB extends Branch
      final class LeafC extends Branch
    }

  }

}
