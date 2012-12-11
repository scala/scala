import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

object O {
  def show(i: Int)    = i.toString
  def show(s: String) = s
}

object Test extends App {
  import O.{show => s}

  val expr = reify {
    s("1") + s(2)
  }

  println(expr.eval)
}