import scala.language.dynamics
import language.experimental.macros

class C extends Dynamic {
  def updateDynamic(name: String)(value: Any): Unit = macro Macros.impl
}

object Test extends App {
  val c = new C
  c.foo = 2
}