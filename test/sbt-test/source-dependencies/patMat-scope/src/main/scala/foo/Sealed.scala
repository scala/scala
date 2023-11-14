package foo

sealed trait Sealed // 'extends SealedBase' will be added as 2nd step

object SealedChild1 extends Sealed
object SealedChild2 extends Sealed
//'object SealedChild3 extends Sealed' will be added

trait SealedBase