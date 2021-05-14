package tastytest

object DelayedPrivate {

  sealed trait Root

  object Nested {

    private object Deeper {
      final class Leaf extends Root
    }

  }

}
