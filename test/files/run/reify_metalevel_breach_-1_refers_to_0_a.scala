import scala.reflect.mirror._

object Test extends App {
  val x = 2
  val outer = reify{reify{x}}
  val code = reify{outer.eval.eval}

  val toolbox = mkToolBox()
  val evaluated = toolbox.runExpr(code.tree)
  println("evaluated = " + evaluated)
}
