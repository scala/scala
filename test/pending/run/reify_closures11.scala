import scala.reflect.mirror._
object Test extends App {
  def fun() = {
    def z() = 2
    reify{z}
  }

  val toolbox = mkToolBox()
  val dyn = toolbox.runExpr(fun().tree)
  val foo = dyn.asInstanceOf[Int]
  println(foo)
}
