import scala.reflect.mirror._

object Test extends App {
  val code = reify {
    class C { override def toString = "C" }
    val ret = new C
    ret.asInstanceOf[Object]
  };

  val toolbox = mkToolBox()
  println(toolbox.runExpr(code.tree))
}
