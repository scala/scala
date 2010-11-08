import annotation._
import elidable._

class ElidableCrashTest {
  trait My

  @elidable(ALL) def foo[a >: My <: My]: scala.Unit = ()

  foo[My] // crash
}