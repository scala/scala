import scala.reflect.mirror._

object Test extends App {
  val toolbox = mkToolBox()
  toolbox.runExpr(reify{
    object Utils {
      @deprecated("test", "2.10.0")
      def foo { println("hello") }
    }

    Utils.foo
  }.tree)
}
