import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

object Test extends App {
  reify {
    class C(val foo: Int)
    println(new C(2).foo)
  }.eval
}