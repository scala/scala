import scala.reflect.mirror._

object Test extends App {
  val code = reify {
    def x = 2
    def y = x
    println(y)
  };

  val toolbox = mkToolBox()
  val evaluated = toolbox.runExpr(code.tree)
  println("evaluated = " + evaluated)
}
