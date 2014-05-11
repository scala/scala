import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}

object Test extends App {
  val mutant = new { val x = 2 }
  val c = cm.classSymbol(mutant.getClass)
  println(c)
  println(c.fullName)
  // under -Xcheckinit there's an additional $init$ field
  c.info.toString.lines.filter(_ != "  private var bitmap$init$0: Boolean") foreach println
}
