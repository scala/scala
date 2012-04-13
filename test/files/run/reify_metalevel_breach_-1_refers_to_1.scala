import scala.reflect.mirror._

object Test extends App {
  val code = reify{
    val x = 2
    val inner = reify{reify{x}}
    inner.eval.eval
  };

  val toolbox = mkToolBox()
  val evaluated = toolbox.runExpr(code.tree)
  println("evaluated = " + evaluated)
}
