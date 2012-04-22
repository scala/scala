import scala.reflect.mirror._

object Test extends App {
  val code = reify {
    case class C(foo: Int, bar: Int)
  };

  val toolbox = mkToolBox()
  println(code.tree)
  println(code.eval)
}
