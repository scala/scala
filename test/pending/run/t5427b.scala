import scala.reflect.runtime.universe._

class Foo { val bar = 2 }

object Test extends App {
  val foo = new Foo
  val tpe = getType(foo)
  val bar = tpe.nonPrivateMember(TermName("bar"))
  val value = getValue(foo, bar)
  println(value)
}