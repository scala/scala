import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

object Test extends App {
  reify {
    println("%s %s %s".format(List("a", "b", "c"): _*))
  }.eval
}
