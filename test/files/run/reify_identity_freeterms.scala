import scala.reflect.runtime.universe._

object Test extends App {
  locally {
    val x = 2
    val tree1 = reify(x).tree
    val tree2 = reify(x).tree
    println(tree1.symbol == tree2.symbol)
  }
}