import scala.reflect.mirror._

object Test extends App {
  val code = reify {
    class C {
      val x = 2
    }

    new C().x
  }

  val toolbox = mkToolBox()
  val evaluated = toolbox.runExpr(code.tree)
  println("evaluated = " + evaluated)
}
