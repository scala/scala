import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

object Test extends App {
  reify {
    println(new {def x = 2; def y = x * x}.y)
  }.eval
}