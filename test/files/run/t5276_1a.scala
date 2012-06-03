import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

object Test extends App {
  reify {
    lazy val x = 2
    println(x)
  }.eval
}