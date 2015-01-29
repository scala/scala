
trait A

trait B extends A

class C extends A with B

object Test extends App {
  val c = classOf[C]

  println(c.getGenericInterfaces.toList)

  assert(c.getGenericInterfaces.length == c.getInterfaces.length,
    s"mismatch between ${c.getGenericInterfaces} and ${c.getInterfaces}")
}
