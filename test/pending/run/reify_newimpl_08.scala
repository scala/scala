import scala.reflect.mirror._

object Test extends App {
  val code = reify {
    class C(val y: Int) {
      val code = reify {
        reify{y}.eval
      }
    }

    new C(2).code.eval
  }

  println(code.eval)
}