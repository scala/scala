import scala.reflect.runtime.universe._

object Test extends App {
  val tree: Tree = null
  tree match {
    case TypeTree() => println("lolwut")
    case null => println("correct")
  }
}