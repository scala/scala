import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

object Test extends App {
  val code = reify {
    class C(val y: Int) {
      val code = reify {
        reify{y}.splice
      }
    }

    new C(2).code.splice
  }

  println(code.eval)
}