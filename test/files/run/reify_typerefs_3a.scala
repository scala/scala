import scala.reflect.mirror._

object foo {
  class Expression {
    override def toString = "Expression"
  }
}

object Test extends App {
  val code = reify {
    List(new foo.Expression, new foo.Expression)
  };

  val toolbox = mkToolBox()
  val evaluated = toolbox.runExpr(code.tree)
  println("evaluated = " + evaluated)
}
