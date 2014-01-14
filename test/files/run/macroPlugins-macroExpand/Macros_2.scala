import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object Macros {
  def impl1(c: Context) = {
    import c.universe._
    q"""println("impl1")"""
  }

  def impl2(c: Context) = {
    import c.universe._
    q"""println("impl2")"""
  }

  def foo1: Unit = macro impl1

  def foo2: Unit = macro impl2
}