import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

object Test extends App {
  reify {
    class C {
      class D {
        val x = 2
      }
    }

    val outer = new C()
    val inner = new outer.D()
    println(inner.x)
  }.eval
}