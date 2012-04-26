import scala.reflect.mirror._

object Test extends App {
  val outer = {
    val x = 2
    reify{x}
  }
  val code = reify{
    val x = 42
    outer.eval
  };

  val toolbox = mkToolBox()
  val evaluated = toolbox.runExpr(code.tree)
  println("evaluated = " + evaluated)
}
