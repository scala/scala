
//> using options -Xsource:3

import language.experimental.macros
import scala.reflect.macros.whitebox.Context

trait Greeter {
  def greeting: Option[Any] = Some("Take me to your leader, Greeter.")
}

class Welcomer {
  def greeter: Greeter = macro Macros.impl
}

object Macros {
  def impl(c: Context) = {
    import c.universe._
    q"""new Greeter { override def greeting = Some("hello, quoted world") }"""
  }
}
