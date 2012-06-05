import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}

class A
trait B

object Test extends App {
  val mutant = new A with B
  val c = cm.reflectClass(mutant.getClass).symbol
  println(c)
  println(c.fullName)
  println(c.typeSignature)
}
