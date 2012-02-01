import scala.reflect.mirror._

class Foo { val bar = 2 }

object Test extends App {
  val foo = new Foo
  val tpe = getType(foo)
  val bar = tpe.nonPrivateMember(newTermName("bar"))
  val value = getValue(foo, bar)
  println(value)
}