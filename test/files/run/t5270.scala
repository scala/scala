import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

object Test extends App {
  reify {
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
  }.eval
}