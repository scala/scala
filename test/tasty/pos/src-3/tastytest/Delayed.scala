package tastytest

sealed trait Delayed

object Delayed {
  final class Instance[T]() extends Delayed
}
