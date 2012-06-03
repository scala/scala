import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

object Test extends App {
  reify {
    object E extends Enumeration { val foo, bar = Value }
    println(E.foo)
  }.eval
}