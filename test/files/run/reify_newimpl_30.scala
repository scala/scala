import scala.reflect.mirror._

object Test extends App {
  {
    class C {
      type T = Int
      val code = reify {
        List[C#T](2)
      }
      println(code.eval)
    }

    new C
  }
}