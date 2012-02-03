import scala.reflect.mirror._

object Test extends App {
  val mutant = new { val x = 2 }
  val c = classToType(mutant.getClass)
  println(c)
  println(c.typeSymbol == classToSymbol(mutant.getClass))
}
