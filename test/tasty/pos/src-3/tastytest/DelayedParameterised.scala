package tastytest

sealed trait DelayedParameterised[+L, +R]

object DelayedParameterised {
  final class Empty extends DelayedParameterised[Nothing, Nothing]
}
