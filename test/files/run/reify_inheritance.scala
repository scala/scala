import scala.reflect.mirror._

object Test extends App {
  reify {
    class C {
      def x = 2
      def y = x * x
    }

    class D extends C {
      override def x = 3
    }

    println(new D().y * new C().x)
  }.eval
}
