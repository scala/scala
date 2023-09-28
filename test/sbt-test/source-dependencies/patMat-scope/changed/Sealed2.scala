package foo

sealed trait Sealed extends SealedBase

object SealedChild1 extends Sealed
object SealedChild2 extends Sealed
object SealedChild3 extends Sealed

trait SealedBase