import scala.reflect.mirror._

object Test extends App {
  reify {
    case class C(foo: Int, bar: Int)
    val c = C(2, 2)
    println(c.foo * c.bar)
  }.eval
}
