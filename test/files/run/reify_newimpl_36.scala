import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

object Test extends App {
  {
    val x = 42
    def foo() = reify(reify(x));
    {
      val x = 2
      val code1 = foo()
      val code2 = code1.eval
      println(code2.eval)
    }
  }
}