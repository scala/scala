import scala.reflect.mirror._

class Expression {
  override def toString = "Expression"
}

object Test extends App {
  val code = reify {
    List(new Expression, new Expression)
  };

  val toolbox = mkToolBox()
  val evaluated = toolbox.runExpr(code.tree)
  println("evaluated = " + evaluated)
}
