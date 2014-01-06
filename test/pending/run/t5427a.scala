import scala.reflect.runtime.universe._

object Foo { val bar = 2 }

object Test extends App {
  val tpe = getType(Foo)
  val bar = tpe.nonPrivateMember(TermName("bar"))
  val value = getValue(Foo, bar)
  println(value)
}