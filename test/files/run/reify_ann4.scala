import scala.reflect.mirror._
import scala.annotation._
import scala.annotation.meta._

object Test extends App {
  // test 1: reify
  val tree = reify{
    class D extends StaticAnnotation
    class C
    val c1 = new C @D
    //val c2 = (new C) @D // illegal syntax
    //val c3 = c1 @D // illegal syntax
  }.tree
  println(tree.toString)

  // test 2: import and typecheck
  val toolbox = mkToolBox()
  val ttree = toolbox.typeCheck(tree)
  println(ttree.toString)

  // test 3: import and compile
  toolbox.runExpr(tree)
}
