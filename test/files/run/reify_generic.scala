import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

object Test extends App {
  reify {
    val product = List(1, 2, 3).head * List[Any](4, 2, 0).head.asInstanceOf[Int]
    println(product)
  }.eval
}