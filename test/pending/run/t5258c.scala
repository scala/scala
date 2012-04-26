import scala.reflect.mirror._

object Test extends App {
  reify {
    object E extends Enumeration { val foo, bar = Value }
    println(E.foo)
  }.eval
}