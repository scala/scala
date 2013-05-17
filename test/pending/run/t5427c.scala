import scala.reflect.runtime.universe._

class Foo(bar: Int)

object Test extends App {
  val foo = new Foo(2)
  val tpe = getType(foo)
  val bar = tpe.nonPrivateMember(TermName("bar"))
  bar match {
    case NoSymbol => println("no public member")
    case _ => println("i'm screwed")
  }
}