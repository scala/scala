import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}

object Test extends App {
  val mutant = new { val x = 2 }
  val c = cm.reflectClass(mutant.getClass).symbol
  println(c)
  println(c.fullName)
  println(c.typeSignature)
}
