import scala.reflect.mirror._

object Test extends App {
  val code = reify {
    case class C(foo: Int, bar: Int)
    val c = C(2, 2)
    println(c.foo * c.bar)
  };

  val toolbox = mkToolBox()
  println(code.tree)
  println(code.eval)
}
