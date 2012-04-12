import scala.reflect.mirror._

object Test extends App {
  reify {
    class C {
      object D {
        val x = 2
      }
    }

    val outer = new C()
    val inner = outer.D
    println(inner.x)
  }.eval
}
