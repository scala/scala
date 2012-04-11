import scala.reflect.mirror._

object Test extends App {
  reify {
    class C
    val product = List(new C, new C).length * List[C](new C, new C).length
    println(product)
  }.eval
}
