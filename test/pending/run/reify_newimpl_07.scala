import scala.reflect.mirror._

object Test extends App {
  {
    class C(val y: Int) {
      val code = reify {
        reify{y}.eval
      }
    }

    println(new C(2).code.eval)
  }
}