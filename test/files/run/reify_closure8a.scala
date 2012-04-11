import scala.reflect.mirror._

object Test extends App {
  class Foo(val y: Int) {
    def fun = reify{y}
  }

  val toolbox = mkToolBox()
  val dyn = toolbox.runExpr(new Foo(10).fun.tree)
  val foo = dyn.asInstanceOf[Int]
  println(foo)
}
