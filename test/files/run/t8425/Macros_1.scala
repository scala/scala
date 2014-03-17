import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object Macros {
  def foo: Unit = macro impl
  def impl(c: Context) = {
    import c.universe._
    val test1 = c.freshName()
    val test2 = c.freshName("$")
    q"println(List($test1, $test2))"
  }
}