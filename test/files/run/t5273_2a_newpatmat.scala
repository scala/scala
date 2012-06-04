import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

object Test extends App {
  reify {
    val foo :: bar :: _ = List(1, 2, 3)
    println(foo * bar)
  }.eval
}