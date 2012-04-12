import scala.reflect.mirror._
import scala.annotation._
import scala.annotation.meta._

object Test extends App {
  // test 1: reify
  val tree = reify{
    class Tree[A, +B](@(inline @getter) final val key: A)
  }.tree
  println(tree.toString)

  // test 2: import and typecheck
  val toolbox = mkToolBox()
  val ttree = toolbox.typeCheck(tree)
  println(ttree.toString)

  // test 3: import and compile
  toolbox.runExpr(tree)
}
