import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

object O {
  type A = Unit
}

object Test extends App {
  val expr = reify {
    import O.{A => X}

    val a: X = ()

    object P {
      type B = Unit
    }

    import P.{B => Y}

    val b: Y = ()
  }

  println(expr.eval)
}