import scala.reflect.mirror._

object Test extends App {
  val code = reify {
    class C { override def toString() = "C" }
    val ret = List((new C, new C))
    ret.asInstanceOf[List[Any]]
  };

  val toolbox = mkToolBox()
  println(toolbox.runExpr(code.tree))
}
