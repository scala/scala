import scala.reflect.mirror._

object Test extends App {
  reify {
    class C {
      lazy val x = 2
    }

    println(new C().x)
  }.eval
}
