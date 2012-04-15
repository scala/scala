import scala.reflect.mirror._

object Test extends App {
  class C[U: TypeTag] {
    type T = U
    val code = reify {
      List[T](2.asInstanceOf[T])
    }
    println(code.eval)
  }

  new C[Int]
}