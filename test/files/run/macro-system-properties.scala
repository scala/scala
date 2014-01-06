import scala.tools.nsc._
import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
    import language.experimental._, reflect.macros.BlackboxContext
    object GrabContext {
      def lastContext = Option(System.getProperties.get("lastContext").asInstanceOf[reflect.macros.runtime.Context])
      // System.properties lets you stash true globals (unlike statics which are classloader scoped)
      def impl(c: BlackboxContext)() = { import c.universe._; System.getProperties.put("lastContext", c); c.Expr[Unit](q"()") }
      def grab(): Unit = macro impl
    }
    object Test { class C(implicit a: Any) { GrabContext.grab } }
    object Test { class C(implicit a: Any) { GrabContext.grab } }
  """
}
