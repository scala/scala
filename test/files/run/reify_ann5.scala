import scala.reflect.mirror._
import scala.annotation._
import scala.annotation.meta._
import scala.beans._

object Test extends App {
  // test 1: reify
  val tree = reify{
    class C(@BeanProperty @(inline @beanGetter) val x: Int)
  }.tree
  println(tree.toString)

  // test 2: import and typecheck
  val toolbox = mkToolBox()
  val ttree = toolbox.typeCheck(tree)
  println(ttree.toString)

  // test 3: import and compile
  toolbox.runExpr(tree)
}
