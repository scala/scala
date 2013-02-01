import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

object A {
  object B {
    val c = ()
  }
}

object Test extends App {
  val expr = reify {
    import A.{B => X}
    import A.B.{c => y}
    import X.{c => z}

    (X.c, y, z)
  }

  println(expr.eval)
}