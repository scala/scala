import scala.reflect.mirror._

object Test extends App {
  // will fail because y is a private field
  // reification doesn't magically make unavailable stuff available
  class Foo(y: Int) {
    def fun = reify{y}
  }

  try {
    val dyn = mkToolBox().runExpr(new Foo(10).fun.tree)
    val foo = dyn.asInstanceOf[Int]
    println(foo)
  } catch {
    case ex: Throwable =>
      println(ex)
  }
}
