import scala.reflect.mirror._

object Test extends App {
  val toolbox = mkToolBox(options = "-deprecation")
  toolbox.runExpr(reify{
    object Utils {
      @deprecated("test", "2.10.0")
      def foo { println("hello") }
    }

    Utils.foo
  })
  println("============compiler messages============")
  toolbox.reporter.infos.foreach(println(_))
  println("=========================================")
}