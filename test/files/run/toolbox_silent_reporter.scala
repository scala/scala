import scala.reflect.mirror._

object Test extends App {
  val toolbox = mkToolBox(options = "-deprecation")
  toolbox.runExpr(reify{
    object Utils {
      @deprecated("test", "2.10.0")
      def foo { println("hello") }
    }

    Utils.foo
  }.tree)
  println("============compiler messages============")
  toolbox.frontEnd.infos.foreach(println(_))
  println("=========================================")
}