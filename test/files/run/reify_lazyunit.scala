import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

object Test extends App {
  reify {
    lazy val x = { 0; println("12")}
    x
    println("one")
    x
    println("two")
  }.eval
}

