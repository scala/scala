import scala.reflect.mirror._

object Expression {
  override def toString = "Expression"
}

object Test extends App {
  val code = reify {
    List(Expression, Expression)
  };

  val toolbox = mkToolBox()
  val evaluated = toolbox.runExpr(code.tree)
  println("evaluated = " + evaluated)
}
