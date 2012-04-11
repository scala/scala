import scala.reflect.mirror._

object Test extends App {
  val code = {
    val x = 2
    val outer = reify{x}
    reify{
      val x = 42
      outer.eval
    };
  }

  val toolbox = mkToolBox()
  val evaluated = toolbox.runExpr(code.tree)
  println("evaluated = " + evaluated)
}
