import scala.reflect.mirror._

object Test extends App {
  val code = reify {
    object C {
      val x = 2
    }

    println(C.x)
  };

  val toolbox = mkToolBox()
  val evaluated = toolbox.runExpr(code.tree)
  println("evaluated = " + evaluated)
}
