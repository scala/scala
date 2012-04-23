import scala.reflect.mirror._

object Test extends App {
  val code = reify {
    object C { def qwe = 4 }
    case class C(foo: Int, bar: Int)
    val c = C(2, 2)
    println(c.foo * c.bar == C.qwe)
  };

  val toolbox = mkToolBox()
  println(code.tree)
  println(code.eval)
}
