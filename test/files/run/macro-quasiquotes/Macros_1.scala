import language.experimental.macros
import scala.reflect.macros.blackbox.Context

class Impls(val c: Context) {
  import c.universe._
  def impl1 = q"println(1)"
  def impl2 = q"{ println(2); println(3) }"
  def impl3 = q"4"
}

object Macros {
  def m1: Unit = macro Impls.impl1
  def m2: Unit = macro Impls.impl2
  def m3: Int = macro Impls.impl3
}