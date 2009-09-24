class Bound
class NotThatBound
trait T[A <: Bound]
trait Wrap[X]

object Wrong extends Wrap[T[NotThatBound]]
