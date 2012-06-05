import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

object Test extends App {
  reify {
    trait Z {
      val z = 2
    }

    class X extends Z {
      def println() = Predef.println(z)
    }

    new X().println()
  }.eval
}