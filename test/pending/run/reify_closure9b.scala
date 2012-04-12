import scala.reflect.mirror._
object Test extends App {
  def foo(y: Int) = {
    class Foo(y: Int) {
      def fun = reify{y}
    }

    val toolbox = mkToolBox()
    val dyn = toolbox.runExpr(new Foo(y).fun.tree)
    dyn.asInstanceOf[Int]
  }

  println(foo(10))
}
