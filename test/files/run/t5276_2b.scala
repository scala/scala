import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

object Test extends App {
  reify {
    class C {
      implicit lazy val x = 2
      def y = implicitly[Int]
    }

    println(new C().y)
  }.eval
}