import scala.tools.nsc.Settings
import scala.tools.partest.ReplTest

object Test extends ReplTest {
    override def transformSettings(s: Settings) = {
    s.Yreplclassbased.value = false // macros are object-based only
    s
  }

  def code = """
  import scala.reflect.macros.blackbox.Context
import language.experimental.macros

def impl1(c: Context) = { import c.universe._; c.Expr[Unit](q"()") }
def foo1: Unit = macro impl1
foo1

def impl2(c: Context)() = { import c.universe._; c.Expr[Unit](q"()") }
def foo2(): Unit = macro impl2
foo2()

def impl3(c: Context)(x: c.Expr[Int])(y: c.Expr[Int]) = { import c.universe._; c.Expr[Unit](q"()") }
def foo3(x: Int)(y: Int): Unit = macro impl3
foo3(2)(3)
  """
}