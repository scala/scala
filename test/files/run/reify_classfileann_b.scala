import scala.reflect.mirror._

class ann(bar: String, quux: Array[String] = Array(), baz: ann = null) extends ClassfileAnnotation

object Test extends App {
  // test 1: reify
  val tree = reify{
    class C {
      def x: Int = {
        2: @ann(bar="1", quux=Array("2", "3"), baz = new ann(bar = "4"))
      }
    }
  }.tree
  println(tree.toString)

  // test 2: import and typecheck
  val toolbox = mkToolBox()
  val ttree = toolbox.typeCheck(tree)
  println(ttree.toString)

  // test 3: import and compile
  toolbox.runExpr(tree)
}