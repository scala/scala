import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

object Test extends App {
  {
    class C(val y: Int) {
      val code = reify {
        reify{y}.splice
      }
    }

    println(new C(2).code.eval)
  }
}