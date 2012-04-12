import scala.reflect.mirror._

object foo {
  object Expression {
    override def toString = "Expression"
  }
}

object Test extends App {
  val code = reify {
    List(foo.Expression, foo.Expression)
  };

  val toolbox = mkToolBox()
  val evaluated = toolbox.runExpr(code.tree)
  println("evaluated = " + evaluated)
}
