// see the following discussions to understand what's being tested here:
// * https://issues.scala-lang.org/browse/SI-6992
// * https://issues.scala-lang.org/browse/SI-8048
// * http://stackoverflow.com/questions/14370842/getting-a-structural-type-with-an-anonymous-classs-methods-from-a-macro
// * http://stackoverflow.com/questions/18480707/method-cannot-be-accessed-in-macro-generated-class/18485004#18485004
// * https://groups.google.com/forum/#!topic/scala-internals/eXQt-BPm4i8

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

object Macros {
  def impl1(c: Context) = {
    import c.universe._
    q"""
      trait Foo { def x = 2 }
      new Foo {}
    """
  }
  def foo1: Any = macro impl1

  def impl2(c: Context) = {
    import c.universe._
    q"""
      class Foo { def x = 2 }
      new Foo
    """
  }
  def foo2: Any = macro impl2

  def impl3(c: Context) = {
    import c.universe._
    q"""
      new { def x = 2 }
    """
  }
  def foo3: Any = macro impl3
}