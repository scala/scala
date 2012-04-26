import scala.reflect.mirror._

object Test extends App {
  val x = 2
  val y = 3
  val code = reify{println(x + y); x + y}

  val toolbox = mkToolBox()
  println(toolbox.runExpr(code.tree))
}
