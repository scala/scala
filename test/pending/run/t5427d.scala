import scala.reflect.runtime.universe._

class Foo(val bar: Int)

object Test extends App {
  val foo = new Foo(2)
  val tpe = getType(foo)
  val bar = tpe.nonPrivateMember(TermName("bar"))
  val value = getValue(foo, bar)
  println(value)
}