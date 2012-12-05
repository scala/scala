import scala.reflect.runtime.universe._

class A { lazy val x = 1 }

object Test {
  def main(args: Array[String]): Unit = {
    val vars = typeOf[A].members.toList filter (x => x.isTerm && x.asTerm.isVar)
    assert(vars.isEmpty, vars)
  }
}
