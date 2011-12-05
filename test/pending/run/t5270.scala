import scala.tools.nsc.reporters._
import scala.tools.nsc.Settings
import reflect.runtime.Mirror.ToolBox

object Test extends App {
  val code = scala.reflect.Code.lift{
    class Y {
      def y = 100
    }

    trait Z { this: Y =>
      val z = 2 * y
    }

    class X extends Y with Z {
      def println() = Predef.println(z)
    }

    new X().println()
  };

  val reporter = new ConsoleReporter(new Settings)
  val toolbox = new ToolBox(reporter)
  val ttree = toolbox.typeCheck(code.tree)
  toolbox.runExpr(ttree)
}
