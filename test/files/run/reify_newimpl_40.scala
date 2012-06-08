import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

object Test extends App {
  {
    val x = 42
    def foo() = reify{ val y = x; reify{ val z = y * x; reify(z * x) } };
    {
      val x = 2
      val code1 = foo()
      val code2 = code1.eval
      val code3 = code2.eval
      println(code3.eval)
    }
  }
}