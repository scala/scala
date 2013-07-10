import language.experimental.macros
import scala.reflect.macros.Macro

trait Impls extends Macro {
  import c.universe._
  def impl1 = q"println(1)"
  def impl2 = q"{ println(2); println(3) }"
  def impl3 = q"4"
}

object Macros {
  def m1 = macro Impls.impl1
  def m2 = macro Impls.impl2
  def m3 = macro Impls.impl3
}