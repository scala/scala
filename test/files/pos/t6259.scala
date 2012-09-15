package t6259

import scala.reflect.runtime.universe._

class A[X](implicit val tt: TypeTag[X]) {}
object B extends A[String]

object C {
  object D extends A[String]
}

trait E {
  object F extends A[String]
}

class G {
  object H extends A[String]
}

object Test {
  val x = {
    object InVal extends A[String]
    5
  }

}

// Note: Both of these fail right now.

trait NeedsEarly {
 val x: AnyRef
}

object Early extends {
  // Drops to this.getClass and is not ok...
  val x = { object EarlyOk extends A[String]; EarlyOk }
} with NeedsEarly


class DoubleTrouble[X](x: AnyRef)(implicit override val tt: TypeTag[X]) extends A[X]

object DoubleOk extends DoubleTrouble[String]({
  // Drops to this.getClass and is an issue 
  object InnerTrouble extends A[String]; 
  InnerTrouble 
})

