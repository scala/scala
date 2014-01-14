import scala.reflect.macros.blackbox.Context
import scala.language.experimental.macros

class Test

object Test {
  def foo: Unit = macro fooImpl
  def fooImpl(c: Context) = { import c.universe._; c.Expr[Unit](q"()") }

  def main(args: Array[String]) {
    try {
      val method = classOf[Test].getMethod("foo")
      sys.error("Static forwarder generated for macro: " + method)
    } catch {
      case _: NoSuchMethodException => // okay
    }
  }
}
