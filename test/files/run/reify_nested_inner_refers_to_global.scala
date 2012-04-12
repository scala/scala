import scala.reflect.mirror._

object Test extends App {
  val code = {
    val x = 2
    reify{
      reify{x}.eval
    }
  }

  val toolbox = mkToolBox()
  val evaluated = toolbox.runExpr(code.tree)
  println("evaluated = " + evaluated)
}
