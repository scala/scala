import scala.language.experimental.macros
import scala.reflect.macros.BlackboxContext

object Macros {
  def impl1(c: BlackboxContext) = {
    import c.universe._
    q"""println("impl1")"""
  }

  def impl2(c: BlackboxContext) = {
    import c.universe._
    q"""println("impl2")"""
  }

  def foo1: Unit = macro impl1

  def foo2: Unit = macro impl2
}