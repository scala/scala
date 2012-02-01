import scala.reflect.mirror._

class Foo(val bar: Int)

object Test extends App {
  val foo = new Foo(2)
  val tpe = getType(foo)
  val bar = tpe.nonPrivateMember(newTermName("bar"))
  val value = getValue(foo, bar)
  println(value)
}