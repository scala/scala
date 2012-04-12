import scala.reflect.mirror._

object Test extends App {
  reify {
    class C(val foo: Int)
    println(new C(2).foo)
  }.eval
}
