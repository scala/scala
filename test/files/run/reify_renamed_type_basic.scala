import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

object O {
  type A = Unit
}

object Test extends App {
  import O.{A => X}

  def expr = reify {
    val a: X = ()
  }

  println(expr.eval)
}