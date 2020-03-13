import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object Macros {
  def fImpl(c: Context) = {
    import c.universe._
    q"""{ _ => "hello" }"""
  }

  def f: Int => String = macro fImpl
}
