import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

object Test extends App {
  reify {
    implicit lazy val x = 2
    println(implicitly[Int])
  }.eval
}