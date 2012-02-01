import scala.reflect.mirror._

class A
trait B

object Test extends App {
  val mutant = new A with B
  val c = classToType(mutant.getClass)
  println(c)
  println(c.typeSymbol == classToSymbol(mutant.getClass))
}
